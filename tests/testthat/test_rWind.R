context("test rWind")

X <- data.frame(
  "2017-01-01T00:00:00Z", rep(c(-0.5, 0, 0.5), each = 3),
  rep(c(1, 1.5, 2)), 1:9, 9:1, 1:9, 9:1
)
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


test_that("provider selection builds the expected GFS archive requests", {
  old_time <- as.POSIXct("2015-02-12 15:00:00", tz = "UTC")
  old_files <- rWind:::.wind_ncei_files(old_time)
  expect_match(old_files[[1]], "model-gfs-g4-anl-files-old")
  expect_match(old_files[[1]], "gfsanl_4_20150212_1200_003[.]grb2$")

  newer_time <- as.POSIXct("2021-07-03 06:00:00", tz = "UTC")
  newer_files <- rWind:::.wind_ncei_files(newer_time)
  expect_match(newer_files[[1]], "model-gfs-g4-anl-files/")
  expect_match(newer_files[[1]], "gfs_4_20210703_0600_000[.]grb2$")

  archive_url <- rWind:::.wind_ncei_url(
    old_files[[1]], old_time, c(353, 356), 34.5, 37.5
  )
  expect_match(archive_url, "u-component_of_wind_height_above_ground")
  expect_match(archive_url, "vertCoord=10")

  current_urls <- rWind:::.wind_pacioos_urls(
    as.POSIXct("2026-09-20 12:00:00", tz = "UTC"),
    -10, 5, 35, 45
  )
  expect_length(current_urls, 2L)
})


test_that("invalid GFS times and extents fail before a network request", {
  expect_error(
    rWind:::.wind_validate_time("2015-02-12 01:00:00"),
    "3-hour intervals"
  )
  expect_error(
    rWind:::.wind_validate_extent(-10, 5, 45, 35),
    "Latitude limits"
  )
})


# may works in future testthat version from https://github.com/r-lib/testthat
test_that("historical downloading works through NOAA/NCEI", {
  skip_if_offline()
  skip_on_cran()
  dl1 <- wind.dl(2015, 2, 12, 12, -7, -4, 34.5, 37.5, trace = 0)
  dl2 <- wind.dl_2("2015/2/12 12:00:00", -7, -4, 34.5, 37.5,
    trace = 0
  )
  reference <- readRDS(test_path("../../vignettes/w.rds"))
  expect_equal(dl1, dl2[[1]])
  expect_equal(dl1, reference, tolerance = 1e-6, check.attributes = FALSE)
})
