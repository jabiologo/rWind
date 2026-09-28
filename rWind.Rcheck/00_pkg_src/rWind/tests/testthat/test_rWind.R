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
  expect_s4_class(wind, "SpatRaster")
  expect_named(wind, c("direction", "speed"))
  expect_equal(terra::crs(wind, proj = TRUE), "+proj=longlat +datum=WGS84 +no_defs")
  expect_true(all(vapply(wind_s, inherits, logical(1), "SpatRaster")))
  expect_is(fl1, "dgCMatrix")
  expect_is(fl3, "TransitionLayer")
})


test_that("flow.dispersion remains compatible with legacy RasterStack input", {
  legacy <- raster::stack(
    raster::raster(wind[["direction"]]),
    raster::raster(wind[["speed"]])
  )
  names(legacy) <- c("direction", "speed")
  expect_equal(
    flow.dispersion(legacy, output = "raw"),
    flow.dispersion(wind, output = "raw")
  )
})


test_that("active cost uses normalized signed flow support", {
  expect_equal(
    cost.active(c(0, 90, 180), c(10, 10, 10), 0, speed.scale = 10),
    c(0.5, 1, 2),
    tolerance = 1e-12
  )
  expect_equal(
    cost.FMGS(c(0, 90, 180), c(10, 10, 10), 0,
      type = "active", speed.scale = 10
    ),
    c(0.5, 1, 2),
    tolerance = 1e-12
  )
  expect_equal(cost.active(NA, 0, 0, speed.scale = 1), 1)
  expect_true(is.infinite(cost.active(0, NA, 0, speed.scale = 1)))
  supportive <- cost.active(0, 1:3, 0, speed.scale = 1)
  opposing <- cost.active(180, 1:3, 0, speed.scale = 1)
  expect_true(all(diff(supportive) < 0))
  expect_true(all(diff(opposing) > 0))
  expect_equal(opposing, 1 / supportive, tolerance = 1e-12)
  expect_equal(
    cost.active(90, c(1, 10, 100), 0, speed.scale = 1),
    rep(1, 3), tolerance = 1e-12
  )
  expect_error(cost.active(0, -1, 0), "cannot be negative")
  expect_error(cost.active(0, 1, 0, speed.scale = 0), "positive finite")
  expect_error(cost.FMGS(0, 1, 0, type = "unknown"), "arg")
})


test_that("passive cost retains the restricted FMGS formula", {
  expect_equal(
    cost.FMGS(c(0, 1, 45), rep(10, 3), 0, type = "passive"),
    c(0.01, 0.2, 9)
  )
  expect_true(all(is.infinite(
    cost.FMGS(c(90, 180, NA), rep(10, 3), 0, type = "passive")
  )))
})


test_that("flow costs average both cells and correct diagonal length", {
  direction <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2,
    ymin = 0, ymax = 2, crs = "EPSG:4326"
  )
  speed <- direction
  terra::values(direction) <- 90
  terra::values(speed) <- c(1, 3, 1, 3)
  flow <- c(direction, speed)
  names(flow) <- c("direction", "speed")

  costs <- flow.dispersion(flow,
    type = "active", speed.scale = 1,
    output = "raw"
  )
  expect_equal(costs[1, 2], mean(c(1 / 2, 1 / 4)))
  expect_equal(costs[2, 1], mean(c(4, 2)))

  terra::values(direction) <- 0
  terra::values(speed) <- 0
  calm <- c(direction, speed)
  names(calm) <- c("direction", "speed")
  calm_costs <- flow.dispersion(calm, type = "active", output = "raw")
  expect_equal(calm_costs[1, 2], 1)
  expect_equal(calm_costs[1, 4], sqrt(2))

  conductance <- flow.dispersion(flow,
    type = "active", speed.scale = 1,
    output = "transitionLayer"
  )
  expect_equal(gdistance::transitionMatrix(conductance)[1, 2], 1 / costs[1, 2])
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


test_that("OSCAR requests use the current ERDDAP dataset", {
  url <- rWind:::.oscar_url(
    as.Date("2014-01-01"), -93, -88, 2, -3
  )
  expect_match(url, "griddap/jplOscar[.]csv", fixed = FALSE)
  expect_match(url, "[(]267[)]:1:[(]272[)]")
  expect_false(grepl("LonPM180", url, fixed = TRUE))

  expect_error(
    rWind:::.oscar_url(as.Date("2014-01-01"), -93, -88, -3, 2),
    "latitude limits"
  )
  expect_error(
    rWind:::.oscar_url(as.Date("2014-01-01"), -200, -88, 2, -3),
    "Longitudes"
  )

  response <- data.frame(
    time = "2014-01-01T00:00:00Z", depth = 15, latitude = 0,
    longitude = 267, u = 1, v = 0
  )
  fitted <- rWind:::oscar.fit_int(response)
  expect_equal(fitted$lon, -93)
  expect_equal(fitted$dir, 90)
  expect_equal(fitted$speed, 1)
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
