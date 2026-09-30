library(rgl)
library(magick)
library(webshot2)
library(faraway)

# Data
data(hormone)

# Recode orientation to 0/1
hormone$orientation <- as.numeric(hormone$orientation)
hormone$orientation[hormone$orientation == 2] <- 0

# Logistic model to define p(x)
mod <- glm(
  orientation ~ estrogen,
  data   = hormone,
  family = binomial(link = "logit")
)

# -----------------------------
# Settings for grouped batches
# -----------------------------
batch_size      <- 20   # number of items per batch
batches_per_x   <- 5    # number of replicate batches per temperature
n_temps         <- 7    # number of temperatures to display

# Temperatures at which distributions are shown
x_vals <- seq(
  min(hormone$estrogen),
  max(hormone$estrogen),
  length.out = n_temps
)

# Predicted probabilities at those temperatures
p_vals <- predict(
  mod,
  newdata = data.frame(estrogen = x_vals),
  type = "response"
)

# Simulate 5 batch counts at each temperature
set.seed(123)

obs <- do.call(
  rbind,
  lapply(seq_along(x_vals), function(i) {
    data.frame(
      temperature = x_vals[i],
      defective   = rbinom(
        n = batches_per_x,
        size = batch_size,
        prob = p_vals[i]
      )
    )
  })
)

# Small jitter so repeated counts are visible
#obs$temperature_jitter <- obs$temperature +
#  rep(seq(-0.02, 0.02, length.out = batches_per_x), times = n_temps)

# -----------------------------
# 3D plot
# -----------------------------
open3d(windowRect = c(0, 0, 900, 550))

xlim <- c(min(x_vals), max(x_vals))
ylim <- c(-0.3, batch_size + 0.3)

# Shorter spikes so they stay inside the viewport
curve_height <- 0.10
zlim         <- c(0, 0.22)

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

# -----------------------------
# Observed batch counts (red)
# -----------------------------
points3d(
  obs$temperature,
  obs$defective,
  rep(0, nrow(obs)),
  col = "red",
  size = 6
)

# Optional: connect the expected count 20*p(x) across temperature
x_pred <- seq(xlim[1], xlim[2], length.out = 300)
p_pred <- predict(
  mod,
  newdata = data.frame(estrogen = x_pred),
  type = "response"
)

lines3d(
  x_pred,
  batch_size * p_pred,
  rep(0, length(x_pred)),
  lwd = 3,
  col = "green3"
)

# -----------------------------
# Binomial PMFs (blue)
# -----------------------------
for (i in seq_along(x_vals)) {

  x.val <- x_vals[i]
  p     <- p_vals[i]

  y_vals <- 0:batch_size+2
  probs  <- dbinom(y_vals, size = batch_size, prob = p)

  # Scale each PMF so its tallest spike has the same visual height
  z_vals <- curve_height * probs / max(probs)

  for (j in seq_along(y_vals)) {

    lines3d(
      c(x.val, x.val),
      c(y_vals[j], y_vals[j]),
      c(0, z_vals[j]),
      col = "blue",
      lwd = 3
    )

    #points3d(
    #  x.val,
    #  y_vals[j],
    #  z_vals[j],
    #  col = "blue",
    #  size = 5
    #)
  }
}

# Axes and grid
axes3d(edges = c("x--", "y--"), col = "black")
grid3d(c("z"), col = "gray")

# Labels
mtext3d("Temperature", edge = "x--", line = 3)
mtext3d("Defective Count", edge = "y--", line = 3)


aspect3d(1, 1.2, 0.5)

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
  zoom = 0.55
)

# -----------------------------
# GIF rendering
# -----------------------------
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
  movie    = "count_binomial_3D",
  frames   = "binomial_batch_3D_frame_",
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
