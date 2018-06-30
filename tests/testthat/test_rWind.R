context("test rWind")

X <- data.frame("2017-01-01T00:00:00Z", rep(c(-0.5,0,0.5), each=3),
                rep(c(1,1.5,2)), 1:9, 9:1, 1:9, 9:1)
colnames(X) <- c("time", "lat", "lon", "ugrd10m", "vgrd10m", "dir", "speed")
class(X) <- c("rWind", "data.frame")

data("wind.series")

wind <- wind2raster(X)
wind_s <- wind2raster(wind.series)

data("wind.series")

# This is not working because in flow.dispersion_int, the function as.matrix
# is obtained from base rather than raster package. We have put @importMethodsFrom
# but is not working... try to fix (5 - June - 2018)

fl1 <- flow.dispersion(wind, "passive", "raw")
fl2 <- flow.dispersion(wind, "active", "raw")

fl3 <- flow.dispersion(wind, "passive", "transitionLayer")
fl4 <- flow.dispersion(wind, "active", "transitionLayer")


test_that("rWind works as expected", {
  expect_is(X, "rWind")
  expect_is(wind.series[[1]], "rWind")
  expect_is(wind, "RasterStack")
  expect_is(fl1, "dgCMatrix")
  expect_is(fl3, "TransitionLayer")
})

