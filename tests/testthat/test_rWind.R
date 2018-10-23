context("test rWind")

X <- data.frame("2017-01-01T00:00:00Z", rep(c(-0.5,0,0.5), each=3),
                rep(c(1,1.5,2)), 1:9, 9:1, 1:9, 9:1)
colnames(X) <- c("time", "lat", "lon", "ugrd10m", "vgrd10m", "dir", "speed")
class(X) <- c("rWind", "data.frame")

data("wind.series")
data("wind.data")

wind <- wind2raster(X)
wind_s <- wind2raster(wind.series)


fl1 <- flow.dispersion(wind, type = "passive", output = "raw")
fl2 <- flow.dispersion(wind, type = "active", output = "raw")

fl3 <- flow.dispersion(wind, type = "passive", output = "transitionLayer")
fl4 <- flow.dispersion(wind, type = "active", output = "transitionLayer")


test_that("rWind works as expected", {
  expect_is(X, "rWind")
  expect_is(wind.series[[1]], "rWind")
  expect_is(tidy(wind.series), "rWind")
  expect_is(wind.mean(wind.series), "rWind")
  expect_is(wind, "RasterStack")
  expect_is(fl1, "dgCMatrix")
  expect_is(fl3, "TransitionLayer")
})


test_that("reading files works as expected", {
    tmp <- tempfile()
    write.csv(wind.data, file = tmp, row.names = FALSE)
    tmp2 <- read.rWind(tmp)
    unlink(tmp)
    expect_equal(wind.data, tmp2, check.attributes = FALSE)
})


# may works in future testthat version from https://github.com/r-lib/testthat
# test_that("downloading works", {
#    skip_if_offline()
#    skip_on_cran()
#    dl1 <- wind.dl(2015,2,12,0,-10,5,35,45)
#    dl2 <- wind.dl_2("2015/2/12 0:00:00",-10,5,35,45)
#    expect_equal(dl1, dl2[[1]])
# })

