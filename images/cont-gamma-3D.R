library(rgl)
library(magick)
library(webshot2)

# -----------------------------
# Simulate Gamma reaction-time data
# -----------------------------
set.seed(123)

# Ages at which reaction times are observed
n_levels <- 7
age_vals <- seq(20, 70, length.out = n_levels)

# Number of repeated observations at each age
reps_per_age <- 10

# True linear mean relationship:
# mean RT decreases with age
beta0 <- 700
beta1 <- -5

mu_true <- beta0 + beta1 * age_vals

# Gamma dispersion parameter
# Var(Y) = phi * mu^2
phi <- 0.20

# Convert mean + dispersion to Gamma shape/scale
shape <- 1 / phi

dat <- do.call(
  rbind,
  lapply(seq_along(age_vals), function(i) {

    mu <- mu_true[i]
    scale <- mu * phi

    data.frame(
      age = age_vals[i],
      rt = rgamma(
        reps_per_age,
        shape = shape,
        scale = scale
      )
    )
  })
)

# -----------------------------
# Fit Gamma regression model
# -----------------------------
mod <- glm(
  rt ~ age,
  data = dat,
  family = Gamma(link = "identity")
)

# Predicted means at displayed ages
mu_hat <- predict(
  mod,
  newdata = data.frame(age = age_vals),
  type = "response"
)

# Estimated dispersion
phi_hat <- summary(mod)$dispersion

# -----------------------------
# Plot settings
# -----------------------------
xlim <- c(
  min(age_vals),
  max(age_vals)
)

# Choose a response range that includes observations
# and most of each Gamma distribution
ymin <- 0

ymax <- max(
  max(dat$rt),
  qgamma(
    0.995,
    shape = 1 / phi_hat,
    scale = max(mu_hat) * phi_hat
  )
)

ylim <- c(ymin, ymax)

# Visual height of the Gamma density curves
curve_height <- 0.08

# Keep plotting box fixed
zlim <- c(0, 0.22)

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
# Observed reaction times
# -----------------------------
points3d(
  dat$age,
  dat$rt,
  rep(0, nrow(dat)),
  col = "red",
  size = 6
)

# -----------------------------
# Fitted Gamma mean
# -----------------------------
x_pred <- seq(
  xlim[1],
  xlim[2],
  length.out = 300
)

mu_pred <- predict(
  mod,
  newdata = data.frame(age = x_pred),
  type = "response"
)

lines3d(
  x_pred,
  mu_pred,
  rep(0, length(x_pred)),
  col = "green3",
  lwd = 3
)

# -----------------------------
# Gamma distributions
# -----------------------------
# Find a common maximum density so all curves use the same z scaling
max_density <- 0

for (i in seq_along(age_vals)) {

  mu <- mu_hat[i]

  shape_i <- 1 / phi_hat
  scale_i <- mu * phi_hat

  y_tmp <- seq(
    ylim[1],
    ylim[2],
    length.out = 500
  )

  dens_tmp <- dgamma(
    y_tmp,
    shape = shape_i,
    scale = scale_i
  )

  max_density <- max(
    max_density,
    max(dens_tmp)
  )
}

# Draw density curves
for (i in seq_along(age_vals)) {

  x.val <- age_vals[i]
  mu <- mu_hat[i]

  shape_i <- 1 / phi_hat
  scale_i <- mu * phi_hat

  y_curve <- seq(
    ylim[1],
    ylim[2],
    length.out = 500
  )

  dens <- dgamma(
    y_curve,
    shape = shape_i,
    scale = scale_i
  )

  # Common visual scaling across all distributions
  z_curve <- curve_height * dens / max_density

  lines3d(
    rep(x.val, length(y_curve)),
    y_curve,
    z_curve,
    col = "blue",
    lwd = 3
  )
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
  "Age",
  edge = "x--",
  line = 3
)

mtext3d(
  "Reaction time (ms)",
  edge = "y--",
  line = 3
)

# -----------------------------
# Camera
# -----------------------------
aspect3d(1, 1, 0.6)

angle <- 65 * pi / 180

par3d(
  userMatrix = matrix(
    c(
      1, 0,           0,          0,
      0, cos(angle),  sin(angle),  200,
      0, -sin(angle), cos(angle),  0,
      0, 0,           0,          1
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
    rpm = 60 / rotation_seconds
  ),
  duration = render_seconds,
  fps = fps,
  movie = "gamma_rt_age_3D",
  frames = "gamma_rt_age_3D_frame_",
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