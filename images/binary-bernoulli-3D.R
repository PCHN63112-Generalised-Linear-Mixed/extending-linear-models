library(rgl)
library(magick)
library(webshot2)
library(faraway)

# Data
data(hormone)

# Recode orientation to 0/1
hormone$orientation <- as.numeric(hormone$orientation)
hormone$orientation[hormone$orientation == 2] <- 0

n <- length(hormone$orientation)

# Bernoulli regression model
mod <- glm(
  orientation ~ estrogen,
  data   = hormone,
  family = binomial(link = "logit")
)

# Open 3D window
open3d(windowRect = c(0, 0, 900, 550))

# Plot ranges
xlim <- c(min(hormone$estrogen), max(hormone$estrogen))
ylim <- c(-0.2, 1.2)

# Make the spikes shorter
curve_height <- 0.03
zlim         <- c(0, 0.10)

# Define plotting bounds
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

# Observed binary data
points3d(
  hormone$estrogen,
  hormone$orientation,
  rep(0, n),
  col = "red",
  size = 5
)

# Fitted probability curve
x_pred <- seq(xlim[1], xlim[2], length.out = 300)

p_pred <- predict(
  mod,
  newdata = data.frame(estrogen = x_pred),
  type = "response"
)

lines3d(
  x_pred,
  p_pred,
  rep(0, length(x_pred)),
  lwd = 3,
  col = "green3"
)

# Locations at which to show Bernoulli distributions
n.bern <- seq(xlim[1], xlim[2], length.out = 7)

for (i in seq_along(n.bern)) {

  x.val <- n.bern[i]

  # Predicted Bernoulli probability at this x value
  p <- predict(
    mod,
    newdata = data.frame(estrogen = x.val),
    type = "response"
  )

  # Bernoulli probabilities
  prob0 <- 1 - p
  prob1 <- p

  # Scaled heights
  z0 <- curve_height * prob0
  z1 <- curve_height * prob1

  # Probability mass at Y = 0
  lines3d(
    c(x.val, x.val),
    c(0, 0),
    c(0, z0),
    col = "blue",
    lwd = 4
  )

  # Probability mass at Y = 1
  lines3d(
    c(x.val, x.val),
    c(1, 1),
    c(0, z1),
    col = "blue",
    lwd = 4
  )

  # Points at spike tops
  points3d(
    c(x.val, x.val),
    c(0, 1),
    c(z0, z1),
    col = "blue",
    size = 5
  )
}

# Axes and grid
axes3d(edges = c("x--", "y--"), col = "black")
grid3d(c("z"), col = "gray")

# Axis labels
mtext3d("Temperature", edge = "x--", line = 3)
mtext3d("Defective", edge = "y--", line = 3)

par3d(
  userMatrix = matrix(
    c(
      1, 0, 0, 0,
      0, 0.34, 0.94, 1.3,
      0, -0.94, 0.34, 0,
      0, 0, 0, 1
    ),
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  ),
  zoom = 0.45
)

fps <- 20
rotation_seconds <- 15
render_seconds <- rotation_seconds - 1 / fps

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
  movie    = "binary_bernoulli_3D",
  frames   = "binary_bernoulli_3D_frame_",
  dir      = "images",
  type     = "gif",
  convert  = paste(
    im,
    "-delay 1x%d -loop 0 -dispose previous %s*.png %s.%s"
  ),
  clean   = TRUE,
  webshot = TRUE,
  top     = FALSE
)