context("test rWind")

X <- data.frame("2017-01-01T00:00:00Z", rep(c(-0.5,0,0.5), each=3),
                rep(c(1,1.5,2)), 1:9, 9:1, 1:9, 9:1)
colnames(X) <- c("time", "lat", "lon", "ugrd10m", "vgrd10m", "dir", "speed")
class(X) <- c("rWind", "data.frame")

data("wind.series")

wind <- wind2raster(X)
wind_s <- wind2raster(wind.series)


fl1 <- flow.dispersion(wind, type = "passive", output = "raw")
fl2 <- flow.dispersion(wind, type = "active", output = "raw")

fl3 <- flow.dispersion(wind, type = "passive", output = "transitionLayer")
fl4 <- flow.dispersion(wind, type = "active", output = "transitionLayer")


test_that("rWind works as expected", {
  expect_is(X, "rWind")
  expect_is(wind.series[[1]], "rWind")
  expect_is(wind, "RasterStack")
  expect_is(fl1, "dgCMatrix")
  expect_is(fl3, "TransitionLayer")
})

