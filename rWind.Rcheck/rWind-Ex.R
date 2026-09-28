pkgname <- "rWind"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rWind-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rWind')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("arrowDir")
### * arrowDir

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: arrowDir
### Title: Arrow direction fitting for Arrowhead function from "shape"
###   package
### Aliases: arrowDir
### Keywords: ~wind

### ** Examples

data(wind.data)

# Create a vector with wind direction (angles) adapted
alpha <- arrowDir(wind.data)
## Not run: 
##D # Now, you can plot wind direction with Arrowhead function from shapes package
##D # Load "shape package
##D require(shape)
##D plot(wind.data$lon, wind.data$lat, type = "n")
##D Arrowhead(wind.data$lon, wind.data$lat,
##D   angle = alpha,
##D   arr.length = 0.1, arr.type = "curved"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("arrowDir", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flow.dispersion")
### * flow.dispersion

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cost.FMGS
### Title: Compute flow-based cost or conductance
### Aliases: cost.FMGS flow.dispersion
### Keywords: ~anisotropy ~conductance

### ** Examples


require(gdistance)

data(wind.data)

wind <- wind2raster(wind.data)

Conductance <- flow.dispersion(wind, type = "passive")

transitionMatrix(Conductance)
image(transitionMatrix(Conductance))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flow.dispersion", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flow.dispersion_int")
### * flow.dispersion_int

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flow.dispersion_int
### Title: Compute flow-based cost or conductance
### Aliases: flow.dispersion_int
### Keywords: internal ~anisotropy ~conductance

### ** Examples


data(wind.data)
wind <- wind2raster(wind.data)
Conductance <- flow.dispersion(wind, type = "passive")
## Not run: 
##D require(gdistance)
##D transitionMatrix(Conductance)
##D image(transitionMatrix(Conductance))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flow.dispersion_int", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("seaOscar.dl")
### * seaOscar.dl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: seaOscar.dl
### Title: OSCAR Sea currents data download
### Aliases: seaOscar.dl
### Keywords: ~currents ~sea

### ** Examples


# Download sea currents for Galapagos Islands
## Not run: 
##D 
##D seaOscar.dl(2015, 1, 1, -93, -88, 2, -3)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("seaOscar.dl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidy.rWind_series")
### * tidy.rWind_series

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidy
### Title: Transforming a rWind_series object into a data.frame
### Aliases: tidy tidy.rWind_series

### ** Examples

data(wind.series)
df <- tidy(wind.series)
head(df)
## Not run: 
##D # use the tidyverse
##D library(dplyr)
##D mean_speed <- tidy(wind.series) %>%
##D   group_by(lat, lon) %>%
##D   summarise(speed = mean(speed))
##D wind_average2 <- wind.mean(wind.series)
##D all.equal(wind_average2$speed, mean_speed$speed)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidy.rWind_series", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("uv2ds")
### * uv2ds

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: uv2ds
### Title: Transform U and V components in direction and speed and vice
###   versa
### Aliases: uv2ds ds2uv
### Keywords: ~wind

### ** Examples


(ds <- uv2ds(c(1, 1, 3, 1), c(1, 1.7, 3, 1)))
ds2uv(ds[, 1], ds[, 2])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("uv2ds", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.data")
### * wind.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.data
### Title: Wind data example
### Aliases: wind.data
### Keywords: datasets

### ** Examples


data(wind.data)
str(wind.data)
head(wind.data[[1]])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.dl")
### * wind.dl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.dl
### Title: Wind-data download
### Aliases: wind.dl read.rWind
### Keywords: ~gfs ~wind

### ** Examples


# Download wind for Iberian Peninsula region at 2015, February 12, 00:00
## Not run: 
##D 
##D wind.dl(2015, 2, 12, 0, -10, 5, 35, 45)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.dl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.dl_2")
### * wind.dl_2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.dl_2
### Title: Wind-data download
### Aliases: wind.dl_2 [[.rWind_series
### Keywords: ~gfs ~wind

### ** Examples


# Download wind for Iberian Peninsula region at 2015, February 12, 00:00
## Not run: 
##D 
##D wind.dl_2("2018/3/15 9:00:00", -10, 5, 35, 45)
##D 
##D library(lubridate)
##D dt <- seq(ymd_hms(paste(2018, 1, 1, 00, 00, 00, sep = "-")),
##D   ymd_hms(paste(2018, 1, 2, 21, 00, 00, sep = "-")),
##D   by = "3 hours"
##D )
##D ww <- wind.dl_2(dt, -10, 5, 35, 45)
##D tidy(ww)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.dl_2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.mean")
### * wind.mean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.mean
### Title: Wind-data mean
### Aliases: wind.mean
### Keywords: ~average ~mean

### ** Examples

data(wind.series)
wind_average <- wind.mean(wind.series)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.mean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.series")
### * wind.series

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.series
### Title: Wind series example
### Aliases: wind.series
### Keywords: datasets

### ** Examples


data(wind.series)
str(tidy(wind.series))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.series", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind2raster")
### * wind2raster

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind2raster
### Title: Wind data to terra rasters
### Aliases: wind2raster
### Keywords: ~gfs ~wind

### ** Examples


data(wind.data)

# Create a SpatRaster with wind direction and speed layers

wind2raster(wind.data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind2raster", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
