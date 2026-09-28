library(rgl)
library(magick)
library(webshot2)

# -----------------------------
# Simulate Poisson count data
# -----------------------------
set.seed(123)

# Temperatures at which counts are observed
n_temps <- 7
temp_vals <- seq(15, 45, length.out = n_temps)

# Number of repeated observation intervals at each temperature
reps_per_temp <- 5

# True relationship:
# log(lambda) = beta0 + beta1 * temperature
beta0 <- -0.2
beta1 <- 0.055

lambda_true <- exp(beta0 + beta1 * temp_vals)

# Simulate counts:
# number of defective items produced during a fixed time interval
dat <- do.call(
  rbind,
  lapply(seq_along(temp_vals), function(i) {
    data.frame(
      temperature = temp_vals[i],
      defective = rpois(
        reps_per_temp,
        lambda = lambda_true[i]
      )
    )
  })
)

# Small x-jitter so overlapping observed counts are visible
# dat$temperature_jitter <- dat$temperature +
#   rep(
#     seq(-0.35, 0.35, length.out = reps_per_temp),
#     times = n_temps
#   )

# -----------------------------
# Fit Poisson regression model
# -----------------------------
mod <- glm(
  defective ~ temperature,
  data = dat,
  family = poisson(link = "log")
)

# Predicted lambda at displayed temperatures
lambda_hat <- predict(
  mod,
  newdata = data.frame(temperature = temp_vals),
  type = "response"
)

# -----------------------------
# Plot settings
# -----------------------------
xlim <- c(
  min(temp_vals),
  max(temp_vals)
)

# Poisson has no fixed upper bound, so truncate the displayed
# counts where essentially all probability mass is included
max_count <- qpois(
  0.999,
  lambda = max(lambda_hat)
)

# Ensure all observed counts are visible
max_count <- max(
  max_count,
  max(dat$defective)
)

ylim <- c(
  -0.3,
  max_count + 0.3
)

# Maximum visual height of the probability spikes
curve_height <- 0.08

# Keep the plotting box fixed so changing curve_height
# does not alter the camera framing
zlim <- c(0, 0.22)

# Find the largest Poisson probability across all displayed
# temperatures so every distribution uses the same scale
max_prob <- max(
  sapply(
    lambda_hat,
    function(lam) {
      max(
        dpois(
          0:max_count,
          lambda = lam
        )
      )
    }
  )
)

# -----------------------------
# Open 3D window
# -----------------------------
open3d(
  windowRect = c(0, 0, 900, 550)
)

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
# Observed Poisson counts
# -----------------------------
points3d(
  dat$temperature,
  dat$defective,
  rep(0, nrow(dat)),
  col = "red",
  size = 6
)

# -----------------------------
# Poisson probability distributions
# -----------------------------
for (i in seq_along(temp_vals)) {

  x.val <- temp_vals[i]
  lam <- lambda_hat[i]

  # Possible displayed counts
  y_vals <- 0:max_count

  # Poisson probabilities
  probs <- dpois(
    y_vals,
    lambda = lam
  )

  # Common vertical scaling across all temperatures
  z_vals <- curve_height * probs / max_prob

  # Draw probability spikes
  for (j in seq_along(y_vals)) {

    lines3d(
      c(x.val, x.val),
      c(y_vals[j], y_vals[j]),
      c(0, z_vals[j]),
      col = "blue",
      lwd = 3
    )
  }
}

# -----------------------------
# Axes and labels
# -----------------------------
axes3d(
  edges = c("x--", "y--"),
  col = "black"
)

grid3d(
  c("z"),
  col = "gray"
)

mtext3d(
  "Temperature",
  edge = "x--",
  line = 3
)

mtext3d(
  "Number defective in interval",
  edge = "y--",
  line = 3
)

# # Compress the y- and z-axes visually
# aspect3d(1, 0.6, 0.4)

# # Camera position
# par3d(
#   userMatrix = matrix(
#     c(
#       1, 0, 0, 0,
#       0, 0.34, 0.94, 1.3,
#       0, -0.94, 0.34, 0,
#       0, 0, 0, 1
#     ),
#     nrow = 4,
#     ncol = 4,
#     byrow = TRUE
#   ),
#   zoom = 0.45
# )

aspect3d(1, 1, 0.6)

angle <- 65 * pi / 180

par3d(
  userMatrix = matrix(
    c(
      1, 0,          0,         0,
      0, cos(angle), sin(angle), 6,
      0, -sin(angle), cos(angle), 0,
      0, 0,          0,         1
    ),
    nrow = 4,
    byrow = TRUE
  ),
  zoom = 0.55
)

# -----------------------------
# GIF rendering
# -----------------------------
fps <- 20
rotation_seconds <- 15

# movie3d() includes both endpoints, so stop one frame short
# of the duplicate 360-degree frame
render_seconds <- rotation_seconds - 1 / fps

# Force external ImageMagick rather than the R magick package
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
    rpm = 60 / rotation_seconds
  ),
  duration = render_seconds,
  fps = fps,
  movie = "poisson_3D",
  frames = "poisson_3D_frame_",
  dir = "images",
  type = "gif",

  convert = paste(
    im,
    "-delay 1x%d -loop 0 -dispose previous %s*.png %s.%s"
  ),

  clean = TRUE,
  webshot = TRUE,
  top = FALSE
)