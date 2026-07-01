library(rgl)
library(magick)
library(webshot2)
library(faraway)

# Data
data(hormone)

# Recode orientation to 0/1
hormone$orientation <- as.numeric(hormone$orientation)
hormone$orientation[hormone$orientation == 2] <- 0

n   <- length(hormone$orientation)
mod <- lm(orientation ~ estrogen, data = hormone)

# Open 3D window
open3d(windowRect = c(0, 0, 900, 550))

# Set the desired ranges
xlim <- c(min(hormone$estrogen), max(hormone$estrogen))

# Expand the binary outcome axis slightly so the impossible values
# implied by the normal model are visible
ylim <- c(-1, 2)

# Visual height of the normal curves
curve_height <- 0.12
zlim <- c(0, 0.5)

# Plot invisible points to define the bounds
plot3d(
  NA,
  xlim = xlim,
  ylim = ylim,
  zlim = zlim,
  type = "n",
  axes = FALSE,
  xlab = "",
  ylab = "",
  zlab = ""
)

# Plot the 3D scatter plot
points3d(
  hormone$estrogen,
  hormone$orientation,
  rep(0, n),
  col = "red",
  size = 5
)

# Regression line
pred <- predict(
  mod,
  newdata = data.frame(estrogen = c(min(hormone$estrogen), max(hormone$estrogen)))
)

lines3d(
  xlim,
  c(pred[1], pred[2]),
  c(0, 0),
  lwd = 2
)

# Overlay the 1D normal distribution curves
n.norms <- seq(xlim[1], xlim[2], length.out = 7)

for (i in seq_along(n.norms)) {
  
  # Model data
  x.val <- n.norms[i]
  beta  <- coef(mod)
  mu    <- beta[1] + beta[2] * x.val
  sigma <- summary(mod)$sigma
  
  # Curve data
  y_curve <- seq(ylim[1], ylim[2], length.out = 200)
  x_curve <- rep(x.val, length(y_curve))
  
  # Raw normal density
  z_raw <- dnorm(y_curve, mean = mu, sd = sigma)
  
  # Rescale visually so each curve peaks at curve_height
  z_curve <- curve_height * z_raw / dnorm(mu, mean = mu, sd = sigma)
  
  # Normal curve
  lines3d(
    x_curve,
    y_curve,
    z_curve,
    col = "blue",
    lwd = 3
  )
  
  # Mean line
  lines3d(
    c(x.val, x.val),
    c(mu, mu),
    c(0, max(z_curve)),
    lwd = 2,
    col = "green3"
  )
}

# Add axes and grid
axes3d(edges = c("x--", "y--"), col = "black")
grid3d(c("z"), col = "gray")

# Add axis labels
mtext3d("Estrogen",    edge = "x--", line = 3)
mtext3d("Orientation", edge = "y--", line = 3)

par3d(
  userMatrix = matrix(c(1,0,0,0,0,0.34,0.94,1.3,0,-0.94,0.34,0,0,0,0,1), nrow=4, ncol=4, byrow=TRUE),
  zoom       = 0.45
)

fps <- 20
rotation_seconds <- 15

# movie3d() includes both endpoints, so stop one frame short
# of the duplicate 360-degree frame.
render_seconds <- rotation_seconds - 1 / fps

# Force external ImageMagick rather than the R magick package:
# this avoids accumulating all frames in R memory.
im <- if (nzchar(Sys.which("magick"))) {
  "magick"
} else if (nzchar(Sys.which("convert"))) {
  "convert"
} else {
  stop("ImageMagick was not found on PATH.")
}

movie3d(
  f = spin3d(
    axis = c(0, 0, 1),
    rpm  = 60 / rotation_seconds
  ),
  duration = render_seconds,
  fps      = fps,
  movie    = "binary_norm_3D",
  frames   = "binary_norm_3D_frame_",
  dir      = "images",
  type     = "gif",

  # Infinite loop, external assembly, no R-memory frame stack
  convert = paste(
    im,
    "-delay 1x%d -loop 0 -dispose previous %s*.png %s.%s"
  ),

  clean   = TRUE,
  webshot = TRUE,
  top     = FALSE
)