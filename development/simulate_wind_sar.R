# Directed wind-mediated spatial dependence: a minimal simulation
#
# This is a development script, not part of the installed package. It avoids
# helper functions deliberately so that every step of the generative model is
# visible. Run it from the root directory of the rWind repository after
# installing or loading the current development version of rWind.

library(terra)
library(Matrix)
library(gdistance)
library(rWind)

set.seed(123)


# 1. Spatial grid ----------------------------------------------------------

# A 50 x 50 abstract square grid. Its coordinates are deliberately Cartesian:
# this first simulation is about directed dependence, not geographic distance.
grid <- rast(
  nrows = 50,
  ncols = 50,
  xmin = 0,
  xmax = 50,
  ymin = 0,
  ymax = 50,
  crs = ""
)

n <- ncell(grid)


# 2. Spatially autocorrelated environmental predictor ---------------------

# Start with independent Gaussian noise and smooth it with a Gaussian moving
# window. This produces a simple temperature-like surface without introducing
# a second spatial model at this stage.
temperature_noise <- grid
values(temperature_noise) <- rnorm(n)

temperature_kernel <- focalMat(grid, d = 5, type = "Gauss")
temperature_kernel <- temperature_kernel / sum(temperature_kernel)

X <- focal(
  temperature_noise,
  w = temperature_kernel,
  fun = "sum",
  na.rm = TRUE,
  fillvalue = NA
)

# Standardization makes the intercept and slope easy to interpret.
values(X) <- as.numeric(scale(values(X)))
names(X) <- "temperature"
plot(X)

# 3. Mean response determined by the environment --------------------------

a <- 2
b <- 1.5

mu <- grid
values(mu) <- a + b * values(X)
names(mu) <- "mu"


# 4. Wind field ------------------------------------------------------------

# Direction follows the rWind convention: it indicates where the flow is
# going. A value of 90 degrees therefore represents an eastward wind.
wind_direction <- grid
values(wind_direction) <- 90
names(wind_direction) <- "direction"

# Wind speed is positive and spatially smooth. It is simulated independently
# of temperature so that both processes have distinct roles.
speed_noise <- grid
values(speed_noise) <- rnorm(n)

wind_speed <- focal(
  speed_noise,
  w = temperature_kernel,
  fun = "sum",
  na.rm = TRUE,
  fillvalue = NA
)
values(wind_speed) <- 4 + as.numeric(scale(values(wind_speed)))
values(wind_speed) <- 4 #pmax(values(wind_speed), 0.5)
names(wind_speed) <- "speed"

wind <- c(wind_direction, wind_speed)
plot(wind)

# 5. Directed connectivity matrix -----------------------------------------

# rWind returns conductance from an origin cell (matrix row) to a destination
# cell (matrix column). With eastward wind, west-to-east conductance should be
# larger than east-to-west conductance.
wind_transition <- flow.dispersion(
  wind,
  type = "active",
  speed.scale = median(values(wind_speed)),
  output = "transitionLayer"
)

conductance_origin_to_destination <- transitionMatrix(wind_transition)

# In the SAR equation below W[i, j] means "influence of source j on receiver
# i". The rWind matrix uses the opposite indexing convention, so it is
# transposed once here.
influence <- t(conductance_origin_to_destination)

# Row standardization makes the incoming weights of every receiver sum to one.
# It also gives rho a convenient interpretation as the overall strength of
# wind-mediated dependence.
incoming_sum <- rowSums(influence)
W <- Diagonal(x = 1 / incoming_sum) %*% influence

stopifnot(max(abs(rowSums(W) - 1)) < 1e-10)


# 6. Directed simultaneous autoregressive residual ------------------------

# The model is
#
#   Y = mu + u
#   u = rho * W %*% u + epsilon
#   epsilon ~ Normal(0, sigma_innovation^2 * I)
#
# Rearranging the second equation gives
#
#   (I - rho * W) %*% u = epsilon
#   u = solve(I - rho * W, epsilon)
#
# Because W is row-standardized, choosing abs(rho) < 1 gives a stable model.
# sigma_innovation is the standard deviation of the independent local shocks.
# It is not the marginal standard deviation of u after spatial propagation.
rho <- 0.7
sigma_innovation <- 0.5

epsilon <- rnorm(n, mean = 0, sd = sigma_innovation)

sar_operator <- Diagonal(n) - rho * W
u <- as.numeric(solve(sar_operator, epsilon))

Y <- as.numeric(values(mu)) + u

sar_residual <- grid
values(sar_residual) <- u
names(sar_residual) <- "SAR residual"

response <- grid
values(response) <- Y
names(response) <- "Y"


# 7. Independent reference simulation ------------------------------------

# This is the original model without wind-mediated residual dependence:
#
#   Y_independent ~ Normal(mu, sigma = 1)
Y_independent <- grid
values(Y_independent) <- rnorm(
  n,
  mean = values(mu),
  sd = sigma_innovation
)
names(Y_independent) <- "Y without SAR"


# 8. Visual comparison -----------------------------------------------------

plot(
  c(X, wind_direction, wind_speed, mu, sar_residual, response),
  nc = 3,
  main = c(
    "Temperature X",
    "Wind direction",
    "Wind speed",
    "Environmental mean mu",
    "Wind-mediated residual u",
    "Response Y"
  )
)

plot(
  c(Y_independent, response),
  nc = 2,
  main = c("Independent residuals", "Directed SAR residuals")
)


# 9. Visual interpretation of the SAR operator ----------------------------

# Put a unit innovation in the central cell and no innovation elsewhere. The
# solution shows where that local signal would propagate under the wind field.
impulse <- numeric(n)
central_cell <- cellFromXY(grid, cbind(25, 25))
impulse[central_cell] <- 1

propagated_impulse <- as.numeric(solve(sar_operator, impulse))

impulse_layer <- grid
values(impulse_layer) <- impulse
names(impulse_layer) <- "Local innovation"

propagated_layer <- grid
values(propagated_layer) <- propagated_impulse
names(propagated_layer) <- "Innovation after wind propagation"

plot(
  c(impulse_layer, propagated_layer),
  nc = 2,
  main = c("Innovation at one cell", "Propagation through W")
)


# 10. Objects retained for later modelling --------------------------------

# X          environmental predictor
# mu         deterministic environmental mean
# wind       direction and speed layers
# W          directed, row-standardized influence matrix
# epsilon    independent local innovations
# u          wind-mediated spatial residual
# response   final simulated response
#
# Although W is asymmetric, the implied covariance matrix is symmetric:
#
#   Sigma = sigma_innovation^2 *
#           solve(I - rho W) %*% t(solve(I - rho W))
#
# It is not constructed here because a dense 2500 x 2500 matrix is unnecessary
# for simulation and would use substantially more memory.
