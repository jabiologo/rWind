context("test rWind")

X <- data.frame("2017-01-01T00:00:00Z", rep(c(-0.5,0,0.5), each=3),
                rep(c(1,1.5,2)), 1:9, 9:1)
colnames(X) <- c("time (UTC)", "latitude (degrees_north)",
                 "longitude (degrees_east)", "ugrd10m (m s-1)", "vgrd10m (m s-1)")

w <- wind.fit(X)

dl <- wind2raster(w, type="dir")
sl <- wind2raster(w, type="speed")

wind <- wind2raster(w, type="stack")

fl1 <- flow.dispersion(wind, "passive", "raw")
fl2 <- flow.dispersion(wind, "active", "raw")

fl3 <- flow.dispersion(wind, "passive", "transitionLayer")
fl4 <- flow.dispersion(wind, "active", "transitionLayer")


test_that("rWind works as expected", {
  expect_is(w, "data.frame")
  expect_is(dl, "RasterLayer")
  expect_is(sl, "RasterLayer")
  expect_is(wind, "RasterStack")
  expect_is(fl1, "dgCMatrix")
  expect_is(fl3, "TransitionLayer")
})

