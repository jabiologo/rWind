# rWind

[![R-CMD-check](https://github.com/KlausVigo/rWind/workflows/R-CMD-check/badge.svg)](https://github.com/KlausVigo/rWind/actions)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/rWind)](https://cran.r-project.org/package=rWind)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/rWind)](https://cran.r-project.org/package=rWind)
[![Research software impact](http://depsy.org/api/package/cran/rWind/badge.svg)](http://depsy.org/package/r/rWind)
[![codecov](https://codecov.io/gh/jabiologo/rWind/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jabiologo/rWind)

## Overview

 rWind is a library in the R language for statistical computing and graphics (R Development Core Team), designed specifically to download and process wind and sea currents data from the Global Forecasting System. From these data, users can obtain wind/sea currents speed and direction layers in order to compute connectivity values between locations. There are other great R libraries that covers data download and data managing that could overlap with [rWind](https://cran.r-project.org/package=rWind), such us [rnoaa](https://cran.r-project.org/package=rnoaa), [RNCEP](https://cran.r-project.org/package=RNCEP) or [weatherr](https://CRAN.R-project.org/package=weatherr). However, rWind is specially focused to offer to the users a straightforward workflow from data download to cost analysis between locations. rWind fills the gap between wind/sea currents data accessibility and their inclusion in a general framework to be applied broadly in ecological or evolutionary studies.
 
 It has been peer-reviewed published in: Fernández‐López, J. and Schliep, K. (2019), rWind: download, edit and include wind data in ecological and evolutionary analysis. Ecography, 42: 804-810. https://doi.org/10.1111/ecog.03730  
https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.03730  

 For more information about data source, please check: 

NOAA/NCEP Global Forecast System (GFS) Atmospheric Model colection (wind data)  
* <https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.html>
* Historical GFS 0.5 degree data: <https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00634>

Ocean Surface Current Analyses Real-time (OSCAR) (sea currents data)  
* <https://doi.org/10.5067/OSCAR-03D01><br />

To install the latest released version of rWind on CRAN use `install.packages("rWind")`  
To install the latest development version `devtools::install_github("jabiologo/rWind")`  
  
  
  
### Quick example: Computing anisotropic shortest paths across Strait of Gibraltar

First, load the packages used in this example.

```{R}
# use install.packages() if some is not installed
# you can install the latest development version using the command 
# devtools::install_github("jabiologo/rWind")
library(rWind)
library(terra)
library(gdistance)
```


In this simple example, we introduce the most basic functionality of rWind, 
to get the shortest paths between two points across Strait of Gibraltar. Notice
that, as wind connectivity is anisotropic (direction dependent), shortest path
from A to B usually does not match with shortest path from B to A.

First, we download wind data of a selected date (e.g. 2015 February 12th). 

```{R}
w <- wind.dl(2015, 2, 12, 12, -7, -4, 34.5, 37.5)
```
Next we transform this `data.frame` into a two-layer `SpatRaster`, with wind
direction and speed.
```{R}
wind_layer <- wind2raster(w)
```

Then, we will use `flow.dispersion` function to obtain a `transitionLayer` 
object with conductance values, which will be used later to obtain the shortest
paths.
```{R}
Conductance <- flow.dispersion(wind_layer, type = "passive")
```

For a general active-movement index, use `type = "active"`. The model projects
flow speed onto movement direction: supportive flow lowers relative cost,
opposing flow raises it, and perpendicular flow is neutral. `speed.scale`
controls sensitivity and defaults to the median positive speed of the layer.

```{R}
ActiveConductance <- flow.dispersion(
  wind_layer,
  type = "active",
  speed.scale = median(w$speed[w$speed > 0], na.rm = TRUE)
)
```

Transition costs average the origin and destination cells and diagonal steps
are multiplied by `sqrt(2)`. They remain relative grid costs: physical
longitude/latitude distances are not corrected.

Now, we will use `shortestPath` function from `gdistance` package [@gdistance] 
to compute shortest path from our `Conductance` object between the two selected
points.
```{R}
AtoB<- shortestPath(Conductance, 
                    c(-5.5, 37), c(-5.5, 35), output="SpatialLines")
BtoA<- shortestPath(Conductance, 
                    c(-5.5, 35), c(-5.5, 37), output="SpatialLines")
```

Finally, we plot the map and we will add the shortest paths as lines and some
other features.

```{R}
plot(wind_layer[["speed"]],
  col = hcl.colors(20, "YlOrRd", rev = TRUE),
  main = "Wind-assisted paths across the Strait of Gibraltar",
  xlab = "Longitude", ylab = "Latitude"
)

points(-5.5, 37, pch=19, cex=3.4, col="red")
points(-5.5, 35, pch=19, cex=3.4, col="blue")

lines(vect(AtoB), col="red", lwd=4, lty=2)
lines(vect(BtoA), col="blue", lwd=4, lty=2)

arrow_scale <- 0.12 / max(w$speed, na.rm = TRUE)
arrows(w$lon, w$lat,
  w$lon + w$ugrd10m * arrow_scale,
  w$lat + w$vgrd10m * arrow_scale,
  length = 0.035
)

text(-5.75, 37.25,labels="Spain", cex= 2.5, col="red", font=2)
text(-5.25, 34.75,labels="Morocco", cex= 2.5, col="blue", font=2)
legend("topleft", legend = c("From Spain to Morocco", "From Morocco to Spain"),
    lwd=4, lty=1, col=c("red","blue"), cex=0.8, bg="white")
```

  
  
For more information and examples, you can check [my blog](http://allthiswasfield.blogspot.com/2018/11/plotting-wind-highways-using-rwind.html)
