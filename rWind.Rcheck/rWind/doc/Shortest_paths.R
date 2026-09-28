## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6
)
suppressPackageStartupMessages(library(rWind))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(gdistance))

## ----download, eval=FALSE-----------------------------------------------------
# w <- wind.dl(
#   2015, 2, 12, 12,
#   lon1 = -7, lon2 = -4,
#   lat1 = 34.5, lat2 = 37.5
# )

## ----cached-data--------------------------------------------------------------
w <- readRDS("w.rds")
head(w)

## ----create-raster------------------------------------------------------------
wind_layer <- wind2raster(w)
wind_layer

## ----inspect-speed------------------------------------------------------------
wind_layer[["speed"]]

## ----conductance--------------------------------------------------------------
conductance <- flow.dispersion(wind_layer, type = "passive")
conductance

## ----active-conductance-------------------------------------------------------
active_conductance <- flow.dispersion(
  wind_layer,
  type = "active",
  speed.scale = median(w$speed[w$speed > 0], na.rm = TRUE)
)
active_conductance

## ----paths--------------------------------------------------------------------
spain <- c(-5.5, 37)
morocco <- c(-5.5, 35)

spain_to_morocco <- shortestPath(
  conductance, spain, morocco,
  output = "SpatialLines"
)
morocco_to_spain <- shortestPath(
  conductance, morocco, spain,
  output = "SpatialLines"
)

## ----map----------------------------------------------------------------------
plot(
  wind_layer[["speed"]],
  col = hcl.colors(20, "YlOrRd", rev = TRUE),
  main = "Wind-assisted paths across the Strait of Gibraltar",
  xlab = "Longitude", ylab = "Latitude"
)

lines(vect(spain_to_morocco), col = "#C62828", lwd = 3, lty = 2)
lines(vect(morocco_to_spain), col = "#1565C0", lwd = 3, lty = 2)
points(spain[1], spain[2], pch = 19, cex = 1.3, col = "#C62828")
points(morocco[1], morocco[2], pch = 19, cex = 1.3, col = "#1565C0")

arrow_scale <- 0.12 / max(w$speed, na.rm = TRUE)
arrows(
  w$lon, w$lat,
  w$lon + w$ugrd10m * arrow_scale,
  w$lat + w$vgrd10m * arrow_scale,
  length = 0.035, col = "grey20"
)

text(spain[1], spain[2] + 0.18, "Spain", col = "#C62828", font = 2)
text(morocco[1], morocco[2] - 0.18, "Morocco", col = "#1565C0", font = 2)
legend(
  "topleft",
  legend = c("Spain to Morocco", "Morocco to Spain"),
  col = c("#C62828", "#1565C0"),
  lwd = 3, lty = 2, cex = 0.8, bg = "white"
)

