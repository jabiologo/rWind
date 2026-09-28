# some trigonemetric functions
rad2deg <- function(rad) {
  (rad * 180) / (pi)
}
deg2rad <- function(deg) {
  (deg * pi) / (180)
}

# circular mean
# https://en.wikipedia.org/wiki/Mean_of_circular_quantities
circ.mean <- function(deg) {
  rad.m <- (deg * pi) / (180)
  mean.cos <- mean(cos(rad.m))
  mean.sin <- mean(sin(rad.m))

  theta <- rad2deg(atan(mean.sin / mean.cos))
  if (mean.cos < 0) theta <- theta + 180
  if ((mean.sin < 0) & (mean.cos > 0)) theta <- theta + 360
  theta
}


# Wind data providers ------------------------------------------------------

.wind_pacioos_start <- as.POSIXct("2022-12-01 12:00:00", tz = "UTC")

.wind_validate_time <- function(time) {
  dt <- as_datetime(time, tz = "UTC")
  if (!length(dt) || anyNA(dt)) {
    stop("'time' must contain valid dates or date-times", call. = FALSE)
  }

  hh <- as.integer(format(dt, "%H", tz = "UTC"))
  mm <- as.integer(format(dt, "%M", tz = "UTC"))
  ss <- as.numeric(format(dt, "%OS", tz = "UTC"))
  if (any(hh %% 3 != 0 | mm != 0 | ss != 0)) {
    stop(
      "GFS wind data are available at 3-hour intervals (00, 03, ..., 21 UTC)",
      call. = FALSE
    )
  }
  dt
}

.wind_validate_extent <- function(lon1, lon2, lat1, lat2) {
  extent <- c(lon1, lon2, lat1, lat2)
  if (length(extent) != 4L || any(!is.finite(extent))) {
    stop("Longitude and latitude limits must be four finite scalars", call. = FALSE)
  }
  if (any(c(lon1, lon2) < -180 | c(lon1, lon2) > 360)) {
    stop("Longitudes must be between -180 and 360 degrees", call. = FALSE)
  }
  if (lat1 < -90 || lat2 > 90 || lat1 > lat2) {
    stop("Latitude limits must satisfy -90 <= lat1 <= lat2 <= 90", call. = FALSE)
  }
  invisible(TRUE)
}

.wind_longitude_parts <- function(lon1, lon2) {
  west <- lon1 %% 360
  east <- lon2 %% 360
  if (west > 180 && east < 180) {
    return(list(c(west, 359.5), c(0, east)))
  }
  list(c(west, east))
}

.wind_pacioos_urls <- function(time, lon1, lon2, lat1, lat2) {
  stamp <- format(time, "%Y-%m-%dT%H:00:00Z", tz = "UTC")
  parts <- .wind_longitude_parts(lon1, lon2)
  vapply(parts, function(part) {
    paste0(
      "https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.csv?",
      "ugrd10m[(", stamp, ")][(", lat1, "):(", lat2, ")][(",
      part[1], "):(", part[2], ")],",
      "vgrd10m[(", stamp, ")][(", lat1, "):(", lat2, ")][(",
      part[1], "):(", part[2], ")]"
    )
  }, character(1))
}

.wind_download_pacioos <- function(time, lon1, lon2, lat1, lat2) {
  urls <- .wind_pacioos_urls(time, lon1, lon2, lat1, lat2)
  pieces <- lapply(urls, function(url) {
    read.csv(url, header = FALSE, skip = 2, stringsAsFactors = FALSE)
  })
  wind.fit_int(do.call(rbind, pieces))
}

.wind_ncei_files <- function(time) {
  day <- format(time, "%Y%m%d", tz = "UTC")
  month <- format(time, "%Y%m", tz = "UTC")
  requested_hour <- as.integer(format(time, "%H", tz = "UTC"))
  cycle_hour <- 6L * (requested_hour %/% 6L)
  forecast_hour <- requested_hour - cycle_hour
  cycle <- sprintf("%02d00", cycle_hour)
  forecast <- sprintf("%03d", forecast_hour)

  old <- paste0(
    "model-gfs-g4-anl-files-old/", month, "/", day,
    "/gfsanl_4_", day, "_", cycle, "_", forecast, ".grb2"
  )
  current <- paste0(
    "model-gfs-g4-anl-files/", month, "/", day,
    "/gfs_4_", day, "_", cycle, "_", forecast, ".grb2"
  )

  # The NCEI migration took place during May 2020 and the catalogues overlap
  # for part of that month. Prefer the likely catalogue, but try both.
  if (time < as.POSIXct("2020-05-15", tz = "UTC")) {
    c(old, current)
  } else {
    c(current, old)
  }
}

.wind_ncei_url <- function(file, time, lon, lat1, lat2) {
  query <- c(
    "var=u-component_of_wind_height_above_ground",
    "var=v-component_of_wind_height_above_ground",
    paste0("north=", lat2),
    paste0("south=", lat1),
    paste0("west=", lon[1]),
    paste0("east=", lon[2]),
    "horizStride=1",
    paste0("time=", format(time, "%Y-%m-%dT%H:00:00Z", tz = "UTC")),
    "vertCoord=10",
    "accept=netcdf3"
  )
  paste0(
    "https://www.ncei.noaa.gov/thredds/ncss/grid/", file, "?",
    paste(query, collapse = "&")
  )
}

.wind_read_ncei <- function(file, time) {
  x <- suppressWarnings(terra::rast(file))
  values <- as.data.frame(x, xy = TRUE, na.rm = FALSE)
  u <- grep("^u-component", names(values))
  v <- grep("^v-component", names(values))
  if (length(u) != 1L || length(v) != 1L) {
    stop("The NOAA/NCEI response did not contain 10 m U and V wind data")
  }
  data.frame(
    time = rep(time, nrow(values)),
    lat = values$y,
    lon = values$x,
    ugrd10m = values[[u]],
    vgrd10m = values[[v]]
  )
}

.wind_download_ncei <- function(time, lon1, lon2, lat1, lat2) {
  files <- .wind_ncei_files(time)
  parts <- .wind_longitude_parts(lon1, lon2)

  for (file in files) {
    pieces <- vector("list", length(parts))
    complete <- TRUE
    for (i in seq_along(parts)) {
      destination <- tempfile(fileext = ".nc")
      url <- .wind_ncei_url(file, time, parts[[i]], lat1, lat2)
      status <- tryCatch(
        suppressWarnings(download.file(url, destination, quiet = TRUE, mode = "wb")),
        error = function(e) 1L
      )
      if (!identical(status, 0L) || !file.exists(destination) ||
          file.info(destination)$size == 0) {
        unlink(destination)
        complete <- FALSE
        break
      }
      pieces[[i]] <- tryCatch(
        .wind_read_ncei(destination, time),
        error = function(e) NULL
      )
      unlink(destination)
      if (is.null(pieces[[i]])) {
        complete <- FALSE
        break
      }
    }
    if (complete) {
      return(wind.fit_int(do.call(rbind, pieces)))
    }
  }
  stop("NOAA/NCEI has no matching archived GFS data", call. = FALSE)
}

.wind_fetch <- function(time, lon1, lon2, lat1, lat2,
                        source = c("auto", "pacioos", "ncei")) {
  source <- match.arg(source)
  providers <- switch(source,
    pacioos = "pacioos",
    ncei = "ncei",
    auto = if (time >= .wind_pacioos_start) c("pacioos", "ncei") else "ncei"
  )
  errors <- character()

  for (provider in providers) {
    result <- tryCatch(
      if (provider == "pacioos") {
        .wind_download_pacioos(time, lon1, lon2, lat1, lat2)
      } else {
        .wind_download_ncei(time, lon1, lon2, lat1, lat2)
      },
      error = function(e) e
    )
    if (!inherits(result, "error")) {
      class(result) <- c("rWind", "data.frame")
      return(list(data = result, source = provider))
    }
    errors <- c(errors, paste0(provider, ": ", conditionMessage(result)))
  }

  stop(
    paste0(
      "Wind data are unavailable for ",
      format(time, "%Y-%m-%d %H:%M UTC", tz = "UTC"), ". ",
      paste(errors, collapse = "; ")
    ),
    call. = FALSE
  )
}


#' Wind-data download
#'
#' wind.dl downloads wind data from the Global Forecast System (GFS) of the
#' USA's National Weather Service (NWS)
#' (https://www.ncei.noaa.gov/products/weather-climate-models/global-forecast).
#' Wind data are taken from NOAA/NCEP Global Forecast System (GFS) Atmospheric
#' Model collection. Geospatial resolution is 0.5 degrees (approximately 50 km),
#' and wind is calculated for Earth surface, at 10 m. More metadata
#' information for the current PacIOOS data:
#' https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.graph
#' Historical data are obtained from the official NOAA/NCEI GFS 0.5 degree
#' archive:
#' https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00634
#'
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "wind_yyyy_mm_dd_tt.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an R object (data.frame) is created.
#'
#' @param yyyy Selected year.
#' @param mm Selected month.
#' @param dd Selected day.
#' @param tt Selected time. There are currently several options at the GFS
#' database: 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00
#' (UTC).
#' @param lon1 Western longitude
#' @param lon2 Eastern longitude
#' @param lat1 Southern latitude
#' @param lat2 Northern latitude
#' @param type Output type. "read-data" is selected by default, creating an R
#' object. If you choose "csv", wind.dl create a a CSV file in your working
#' directory named "wind_yyyy_mm_dd_tt.csv".
#' @param trace if trace = 1 (by default) track downloaded files
#' @param source Data provider. `"auto"` selects PacIOOS for dates in its
#' current coverage and NOAA/NCEI for older dates. Use `"pacioos"` or
#' `"ncei"` to force one provider.
#' @param file file name of the saved ".csv" files.
#' @return "rWind" and "data.frame" class object or .csv file with U and V
#' vector  components and wind direction and speed for each coordinate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note Longitude coordinate are provided by GFS dataset in 0/360 notation
#' and transformed internally into -180/180. Wind "dir" denotes where the
#' wind is going (toward), not from where is coming. With `source = "auto"`,
#' dates before the current PacIOOS coverage are requested from NOAA/NCEI.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl_2}}, \code{\link{wind2raster}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.graph
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' \dontrun{
#'
#' wind.dl(2015, 2, 12, 0, -10, 5, 35, 45)
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd_h year month day hour
#' @importFrom terra rast
#' @rdname wind.dl
#' @export wind.dl
wind.dl <- function(yyyy, mm, dd, tt, lon1, lon2, lat1, lat2,
                    type = "read-data", trace = 1,
                    source = c("auto", "pacioos", "ncei")) {
  type <- match.arg(type, c("read-data", "csv"))
  source <- match.arg(source)
  .wind_validate_extent(lon1, lon2, lat1, lat2)
  dt <- .wind_validate_time(ymd_h(paste(yyyy, mm, dd, tt, sep = "-")))
  fetched <- .wind_fetch(dt, lon1, lon2, lat1, lat2, source = source)
  tmp <- fetched$data
  if (trace) {
    message(
      format(dt, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      " downloaded from ", fetched$source
    )
  }
  if (type == "csv") {
    fname <- paste0("wind_", format(dt, "%Y_%m_%d_%H", tz = "UTC"), ".csv")
    write.table(tmp, fname,
      sep = ",", row.names = FALSE,
      col.names = TRUE, quote = FALSE
    )
  }
  return(tmp)
}


#' @rdname wind.dl
#' @export
read.rWind <- function(file) {
  tmp <- read.csv(file, colClasses = c(
    "POSIXct", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric"
  ))
  tmp[, 1] <- ymd_hms(tmp[, 1], truncated = 3)
  class(tmp) <- c("rWind", "data.frame")
  tmp
}



#' Wind-data download
#'
#' wind.dl_2 downloads time-series wind data from the Global Forecast System
#' (GFS) of the USA's National Weather Service (NWS)
#' (https://www.ncei.noaa.gov/products/weather-climate-models/global-forecast).
#' Wind data are taken from NOAA/NCEP Global Forecast System (GFS) Atmospheric
#' Model collection. Geospatial resolution is 0.5 degrees (approximately 50 km),
#' and wind is calculated for Earth surface, at 10 m. More metadata
#' information for current data:
#' https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.html
#' Historical data are obtained from the official NOAA/NCEI GFS 0.5 degree
#' archive.
#'
#' To get the same format as wind.dl, you should run \code{tidy} function from
#' wind.dl_2 output.
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "wind_yyyy_mm_dd_tt.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an \code{rWind_series} object is created.
#'
#' @param time a scalar or vector of POSIXt or Date objects or an character
#' which can transformed into those, see example below.
#' There are currently these options at the GFS database for the hours:
#' 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00 (UTC) (TO).
#' @param lon1 Western longitude
#' @param lon2 Eastern longitude
#' @param lat1 Southern latitude
#' @param lat2 Northern latitude
#' @param type Output type. "read-data" is selected by default, creating an R
#' object. If you choose "csv", wind.dl create a a CSV file in your work
#' directory named "wind_yyyy_mm_dd_tt.csv".
#' @param trace if trace = 1 (by default) track downloaded files
#' @param source Data provider. `"auto"` selects PacIOOS for dates in its
#' current coverage and NOAA/NCEI for older dates. Use `"pacioos"` or
#' `"ncei"` to force one provider.
#' @return an object of class \code{rWind_series} or .csv file/s with
#' U and V vector components and wind direction and speed for each coordinate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note wind.dl_2 requires two dates that represent the boundaries of the time
#' lapse to download wind series data.
#' U and V vector components allow you to create wind averages or tendencies
#' for each coordinate at the study area. Longitude coordinates are
#' provided by GFS dataset in 0/360 notation and transformed internally into
#' -180/180. "dir" denotes where the
#' wind/sea current is going (toward), not from where is coming. With
#' `source = "auto"`, dates before the current PacIOOS coverage are requested
#' from NOAA/NCEI.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.mean}}, \code{\link{wind2raster}},
#' \code{\link{wind.dl}}, \code{\link[lubridate]{as_datetime}},
#' \code{\link{as.POSIXct}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' https://pae-paha.pacioos.hawaii.edu/erddap/griddap/ncep_global.graph
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' \dontrun{
#'
#' wind.dl_2("2018/3/15 9:00:00", -10, 5, 35, 45)
#'
#' library(lubridate)
#' dt <- seq(ymd_hms(paste(2018, 1, 1, 00, 00, 00, sep = "-")),
#'   ymd_hms(paste(2018, 1, 2, 21, 00, 00, sep = "-")),
#'   by = "3 hours"
#' )
#' ww <- wind.dl_2(dt, -10, 5, 35, 45)
#' tidy(ww)
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd_h year month day hour as_datetime
#' @rdname wind.dl_2
#' @export wind.dl_2
#'
wind.dl_2 <- function(time, lon1, lon2, lat1, lat2, type = "read-data", trace = 1,
                      source = c("auto", "pacioos", "ncei")) {
  type <- match.arg(type, c("read-data", "csv"))
  source <- match.arg(source)
  .wind_validate_extent(lon1, lon2, lat1, lat2)
  dt <- .wind_validate_time(time)
  # We will store each date and time in a list
  resultados <- vector("list", length(dt))
  names(resultados) <- format(dt, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  coordinates <- NULL

  for (id in seq_along(dt)) {
    fetched <- .wind_fetch(dt[id], lon1, lon2, lat1, lat2, source = source)
    tmp <- fetched$data
    if (trace) {
      message(
        format(dt[id], "%Y-%m-%d %H:%M UTC", tz = "UTC"),
        " downloaded from ", fetched$source
      )
    }
    if (type == "csv") {
      fname <- paste0(
        "wind_", format(dt[id], "%Y_%m_%d_%H", tz = "UTC"), ".csv"
      )
      write.table(tmp, fname,
        sep = ",", row.names = FALSE,
        col.names = TRUE, quote = FALSE
      )
    } else {
      current_coordinates <- tmp[, 2:3, drop = FALSE]
      if (is.null(coordinates)) {
        coordinates <- current_coordinates
      } else if (!isTRUE(all.equal(coordinates, current_coordinates,
        check.attributes = FALSE))) {
        stop("Downloaded time points do not share the same spatial grid",
          call. = FALSE
        )
      }
      resultados[[id]] <- tmp[, 4:5, drop = FALSE]
    }
  }

  if (type == "csv") {
    return(invisible(NULL))
  }
  attr(resultados, "lat_lon") <- coordinates
  class(resultados) <- c("rWind_series", "list")
  return(resultados)
}


#' @rdname wind.dl_2
#' @param x object from which to extract element(s).
#' @param i indices specifying elements to extract.
#' @param exact Controls possible partial matching (not used yet).
#' @export
"[[.rWind_series" <- function(x, i, exact = TRUE) {
  tt <- as_datetime(names(x)[i])
  tmp <- cbind(tt, attr(x, "lat_lon"), unclass(x)[[i]])
  tmp <- wind.fit_int(tmp)
  class(tmp) <- c("rWind", "data.frame")
  tmp
}


#' wind.fit_int
#'
#' wind.fit_int is used internally by wind.dl to transform downloaded data from
#' GFS. wind.fit_int applies trigonometry tools to transform U and V vector wind
#' components in wind direction and speed features. It also transforms 0-360
#' longitude notation obtained from GFS data into -180/180 longitude notation.
#' Moreover, it cleans dates names and sorts the data by latitude.
#'
#' @param tmpx downloaded data produced by \code{wind.dl}.
#' @return data.frame
#' @note This function is used internally by wind.dl
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.mean}},
#' \code{\link{wind2raster}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~wind ~gfs
#' @importFrom lubridate ymd_hms
#' @rdname wind.fit_int
#' @keywords internal
wind.fit_int <- function(tmpx) {
  tmpx[, 3] <- tmpx[, 3] %% 360
  tmpx[tmpx[, 3] >= 180, 3] <- tmpx[tmpx[, 3] >= 180, 3] - 360

  ###### DIRECTION
  direction <- atan2(tmpx[, 4], tmpx[, 5])
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED
  speed <- sqrt((tmpx[, 4] * tmpx[, 4]) + (tmpx[, 5] * tmpx[, 5]))

  ######
  names(tmpx) <- c("time", "lat", "lon", "ugrd10m", "vgrd10m")
  res <- cbind(tmpx, dir = direction, speed = speed)
  res <- res[with(res, order(-lat)), ]
  res[, 1] <- ymd_hms(res[, 1], truncated = 3)
  return(res)
}

#' Transform U and V components in direction and speed and vice versa
#'
#'
#' @param u U component.
#' @param v U component.
#' @return "uv2ds" returns a matrix with direction and speed values
#' @note Multiple U and V values can be processed. "dir" denotes where the
#' wind/sea current is going (toward), not from where is coming.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.mean}}, \code{\link{wind2raster}}
#' @keywords ~wind
#' @examples
#'
#' (ds <- uv2ds(c(1, 1, 3, 1), c(1, 1.7, 3, 1)))
#' ds2uv(ds[, 1], ds[, 2])
#' @rdname uv2ds
#' @export uv2ds

uv2ds <- function(u, v) {
  ###### DIRECTION
  direction <- atan2(u, v)
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED
  speed <- sqrt((u * u) + (v * v))

  ######
  res <- cbind(dir = direction, speed = speed)
  return(res)
}


#' @param d direction (degrees).
#' @param s speed (m/s).
#' @return "ds2uv" returns a matrix with U and V values
#' @rdname uv2ds
#' @export ds2uv

ds2uv <- function(d, s) {
  d <- d %% 360
  r <- deg2rad(d)
  u <- sin(r) * s
  v <- cos(r) * s
  cbind(u = u, v = v)
}



#' Wind data to a terra raster
#'
#' `wind2raster_int()` converts one gridded `rWind` data frame to a two-layer
#' [terra::SpatRaster]. Longitude and latitude define the grid and the output
#' layers contain wind direction and speed. The coordinate reference system is
#' WGS 84 (EPSG:4326).
#'
#' @param x an object of class \code{rWind}
#' @return A two-layer [terra::SpatRaster] named `direction` and `speed`.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind2raster}}
#' @keywords ~gfs ~wind
#' @importFrom terra rast
#'
#' @rdname wind2raster_int
#' @keywords internal
wind2raster_int <- function(x) {
  required <- c("lon", "lat", "dir", "speed")
  if (!all(required %in% names(x))) {
    stop(
      "'x' must contain lon, lat, dir, and speed columns",
      call. = FALSE
    )
  }
  xyz <- data.frame(
    lon = x$lon,
    lat = x$lat,
    direction = x$dir,
    speed = x$speed
  )
  terra::rast(xyz, type = "xyz", crs = "EPSG:4326")
}

#' Wind data to terra rasters
#'
#' `wind2raster()` converts an `rWind` object to a two-layer
#' [terra::SpatRaster] containing wind direction and speed. An
#' `rWind_series` is converted to a list of `SpatRaster` objects, one per time
#' step. All outputs use WGS 84 (EPSG:4326).
#'
#' @param x An object of class `rWind` or `rWind_series`.
#' @return A [terra::SpatRaster], or a list of them, with layers `direction`
#' and `speed`.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl}}
#' @keywords ~gfs ~wind
#' @examples
#'
#' data(wind.data)
#'
#' # Create a SpatRaster with wind direction and speed layers
#'
#' wind2raster(wind.data)
#' @importFrom terra rast
#'
#' @rdname wind2raster
#' @export wind2raster
wind2raster <- function(x) {
  if (inherits(x, "rWind_series")) {
    X <- lapply(x, wind2raster_int)
  } else {
    return(wind2raster_int(x))
  }
  X
}

#' Arrow direction fitting for Arrowhead function from "shape" package
#'
#' arrowDir adapts wind direction value to be used by Arrowhead function from
#' "shape" package to plot wind direction for each coordinate.
#'
#' Angle argument of Arrowhead function from "shape" package needs to be fed
#' in an anti-clockwise way, relative to x-axis, in degrees 0,360. arrowDir
#' function adapts wind direction provided by wind.fit (clockwise, relative to
#' y-axis ) to requirements of Arrowhead.
#'
#' @param W An object of class \code{rWind} or a data.frame which should content
#' a column named "dir".
#' @return A vector with angles for each arrow to be plotted by Arrowhead.
#' @note arrowDir function works always together with Arrowhead function from
#' "shape" package.
#' @author Javier Fernández-López
#' @seealso \code{\link{wind.dl}}
#' @references Karline Soetaert (2017). shape: Functions for Plotting Graphical
#' Shapes, Colors. R package version 1.4.3.
#' https://CRAN.R-project.org/package=shape
#' @keywords ~wind
#' @examples
#' data(wind.data)
#'
#' # Create a vector with wind direction (angles) adapted
#' alpha <- arrowDir(wind.data)
#' \dontrun{
#' # Now, you can plot wind direction with Arrowhead function from shapes package
#' # Load "shape package
#' require(shape)
#' plot(wind.data$lon, wind.data$lat, type = "n")
#' Arrowhead(wind.data$lon, wind.data$lat,
#'   angle = alpha,
#'   arr.length = 0.1, arr.type = "curved"
#' )
#' }
#'
#' @export arrowDir
arrowDir <- function(W) {
  if (inherits(W, "rWind_series")) {
    if (length(W) > 1) message("W contained a time series, just took first time point!")
    W <- W[[1]]
  }
  aDir <- (360 - W$dir) + 90
  return(aDir)
}


# Cost computation following Muñoz et al., 2004; Felicísimo et al., 2008
# for passive movement, and signed flow support for active movement.

.flow_speed_scale <- function(speed) {
  positive <- speed[is.finite(speed) & speed > 0]
  if (!length(positive)) return(1)
  stats::median(positive)
}

.flow_angle_difference <- function(direction, target) {
  abs((direction - target + 180) %% 360 - 180)
}

#' Relative cost of active movement through a flow
#'
#' `cost.active()` is a deliberately simple, general index based on signed flow
#' support. The flow component parallel to movement is `speed * cos(angle)`.
#' Supportive flow lowers cost, opposing flow raises it, and perpendicular flow
#' has no effect. This is a relative index, not physical travel time or energy.
#'
#' Let `z = speed * cos(angle) / speed.scale`. When `z >= 0`, cost is
#' `1 / (1 + z)`; when `z < 0`, cost is `1 - z`. Thus support equal to the
#' reference speed halves cost, opposition equal to the reference speed doubles
#' it, and `cost(-z) = 1 / cost(z)`.
#'
#' @param flow.direction Flow direction in degrees, pointing towards where the
#' flow moves.
#' @param flow.speed Non-negative flow speed.
#' @param target Movement direction in degrees.
#' @param speed.scale Positive reference speed used to make flow support
#' dimensionless. By default, the median positive input speed is used.
#' @return Relative movement cost. A value below one indicates support and a
#' value above one indicates opposition.
#' @rdname cost.active
#' @export
cost.active <- function(flow.direction, flow.speed, target,
                        speed.scale = NULL) {
  if (length(target) != 1L || !is.finite(target)) {
    stop("'target' must be one finite direction", call. = FALSE)
  }
  if (any(flow.speed < 0, na.rm = TRUE)) {
    stop("Flow speed cannot be negative", call. = FALSE)
  }
  input_lengths <- c(length(flow.direction), length(flow.speed))
  if (any(input_lengths == 0L)) {
    stop("Flow direction and speed cannot be empty", call. = FALSE)
  }
  output_length <- max(input_lengths)
  if (any(!input_lengths %in% c(1L, output_length))) {
    stop("Flow direction and speed must have compatible lengths", call. = FALSE)
  }
  template <- if (length(flow.speed) == output_length) {
    flow.speed
  } else {
    flow.direction
  }
  flow.direction <- rep(flow.direction, length.out = output_length)
  flow.speed <- rep(flow.speed, length.out = output_length)
  if (is.null(speed.scale)) speed.scale <- .flow_speed_scale(flow.speed)
  if (length(speed.scale) != 1L || !is.finite(speed.scale) ||
      speed.scale <= 0) {
    stop("'speed.scale' must be one positive finite value", call. = FALSE)
  }

  cost <- rep(Inf, output_length)
  calm <- is.finite(flow.speed) & flow.speed == 0
  cost[calm] <- 1
  valid <- is.finite(flow.direction) & is.finite(flow.speed) & flow.speed > 0
  if (any(valid)) {
    angle <- .flow_angle_difference(flow.direction[valid], target)
    support <- flow.speed[valid] * cos(deg2rad(angle))
    z <- support / speed.scale
    cost[valid] <- ifelse(z >= 0, 1 / (1 + z), 1 - z)
  }
  if (!is.null(dim(template))) {
    dim(cost) <- dim(template)
    dimnames(cost) <- dimnames(template)
  } else if (!is.null(names(template))) {
    names(cost) <- names(template)
  }
  cost
}

#' @rdname flow.dispersion
#' @export
cost.FMGS <- function(wind.direction, wind.speed, target,
                      type = c("passive", "active"), speed.scale = NULL) {
  type <- match.arg(type)
  if (type == "active") {
    return(cost.active(wind.direction, wind.speed, target, speed.scale))
  }

  dif <- .flow_angle_difference(wind.direction, target)
  # In passive mode, deviations of 90 degrees or more cannot move to target.
  dif[dif >= 90 & !is.na(dif)] <- Inf
  dif[is.na(dif)] <- Inf
  # Felicísimo et al. (2008) horizontal factor.
  dif[dif < 90] <- 2 * dif[dif < 90]
  dif[dif == 0] <- 0.1

  wind.speed[is.na(wind.speed)] <- 0
  dif / wind.speed
}


#' Compute flow-based cost or conductance
#'
#' flow.dispersion_int computes movement conductance through a flow such as sea
#' or wind currents. Passive movement uses a restricted version of the formula
#' described in Felícisimo et al. (2008):
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' where HorizontalFactor incrementally penalizes angular deviations from the
#' flow direction and prevents passive movement at deviations of 90 degrees or
#' more. Active movement instead uses the signed flow component parallel to the
#' movement direction; supportive flow lowers cost and opposing flow raises it.
#'
#' @param stack A `SpatRaster` returned by `wind2raster()` with direction and
#' speed layers. Legacy `RasterStack` objects are also accepted.
#' @param fun A function to compute the cost to move between cells. The default
#' is \code{cost.FMGS} from Felicísimo et al. (2008), see details.
#' @param output This argument allows to select different kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @param type Movement model: `"passive"` or `"active"`.
#' @param speed.scale Positive reference speed for the active model. If `NULL`,
#' the median positive speed of the complete raster layer is used.
#' @param ... Further arguments passed to or from other methods.
#' @return In "transitionLayer" output, the function returns conductance values
#' (1/cost)to move between all cells in a raster having into account flow speed
#' and direction obtained from wind.fit function("rWind" package). As wind or
#' sea currents implies directionality, flow.dispersion produces an anisotropic
#' conductance matrix (asymmetric). Conductance values are used later to built a
#' TransitionLayer object from "gdistance" package.
#'
#' In "raw" output, flow.dispersion creates a sparse Matrix with cost values.
#' Each edge cost is the mean of the local costs at its origin and destination.
#' Diagonal edges are multiplied by `sqrt(2)`.
#' @note Costs are relative per grid step. The diagonal correction assumes a
#' square grid, but no correction is made for the changing physical distances
#' of longitude/latitude cells. Results should not be interpreted as physical
#' time, energy, or distance and should not be compared directly across grid
#' resolutions or distant latitude ranges.
#'
#' Note that for large data sets, it could take a while. For large study
#' areas is strongly advised perform the analysis in a remote computer or a
#' cluster.
#' @author Javier Fernández-López; Klaus Schliep; Yurena Arjona
#' @seealso \code{\link{wind.dl}}, \code{\link{wind2raster}}
#' @references
#'
#' Felicísimo, Á. M., Muñoz, J., & González-Solis, J. (2008). Ocean surface
#' winds drive dynamics of transoceanic aerial movements. PLoS One, 3(8),
#' e2928.
#'
#' Jacob van Etten (2017). R Package gdistance: Distances and Routes on
#' Geographical Grids. Journal of Statistical Software, 76(13), 1-21.
#' doi:10.18637/jss.v076.i13
#' @keywords ~anisotropy ~conductance
#' @examples
#'
#' data(wind.data)
#' wind <- wind2raster(wind.data)
#' Conductance <- flow.dispersion(wind, type = "passive")
#' \dontrun{
#' require(gdistance)
#' transitionMatrix(Conductance)
#' image(transitionMatrix(Conductance))
#' }
#' @importFrom raster raster
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @keywords internal
flow.dispersion_int <- function(stack, fun = cost.FMGS,
                                output = "transitionLayer",
                                type = c("passive", "active"),
                                speed.scale = NULL, ...) {
  output <- match.arg(output, c("raw", "transitionLayer"))
  type <- match.arg(type)

  if (inherits(stack, "SpatRaster")) {
    DL <- terra::as.matrix(stack[["direction"]], wide = TRUE)
    SL <- terra::as.matrix(stack[["speed"]], wide = TRUE)
    direction_layer <- stack[["direction"]]
    ncells <- terra::ncell(stack[["direction"]])
    raster_nrow <- terra::nrow(stack[["direction"]])
  } else if (inherits(stack, "Raster")) {
    DL <- as.matrix(stack[["direction"]])
    SL <- as.matrix(stack[["speed"]])
    direction_layer <- stack[["direction"]]
    ncells <- raster::ncell(direction_layer)
    raster_nrow <- nrow(direction_layer)
  } else {
    stop("'stack' must be a SpatRaster or Raster* object", call. = FALSE)
  }

  M <- matrix(seq_len(ncells),
    nrow = raster_nrow, byrow = TRUE
  )
  nr <- nrow(M)
  nc <- ncol(M)

  built_in_cost <- identical(fun, cost.FMGS) || identical(fun, cost.active)
  if (type == "active" && built_in_cost && is.null(speed.scale)) {
    speed.scale <- .flow_speed_scale(SL)
  }

  calculate_cost <- function(direction, speed, target) {
    if (identical(fun, cost.FMGS)) {
      return(fun(direction, speed, target,
        type = type, speed.scale = speed.scale, ...
      ))
    }
    if (identical(fun, cost.active)) {
      return(fun(direction, speed, target,
        speed.scale = speed.scale, ...
      ))
    }
    fun(direction, speed, target, ...)
  }

  # Row offsets increase towards the south because raster matrix row 1 is the
  # northernmost row. Costs use the mean local cost at origin and destination.
  moves <- data.frame(
    row = c(-1, -1, -1, 0, 0, 1, 1, 1),
    col = c(-1, 0, 1, -1, 1, -1, 0, 1),
    direction = c(315, 0, 45, 270, 90, 225, 180, 135)
  )
  edge_from <- edge_to <- edge_cost <- vector("list", nrow(moves))

  for (k in seq_len(nrow(moves))) {
    from_rows <- which(seq_len(nr) + moves$row[k] >= 1 &
      seq_len(nr) + moves$row[k] <= nr)
    from_cols <- which(seq_len(nc) + moves$col[k] >= 1 &
      seq_len(nc) + moves$col[k] <= nc)
    if (!length(from_rows) || !length(from_cols)) next
    to_rows <- from_rows + moves$row[k]
    to_cols <- from_cols + moves$col[k]

    origin_cost <- calculate_cost(
      DL[from_rows, from_cols, drop = FALSE],
      SL[from_rows, from_cols, drop = FALSE],
      moves$direction[k]
    )
    destination_cost <- calculate_cost(
      DL[to_rows, to_cols, drop = FALSE],
      SL[to_rows, to_cols, drop = FALSE],
      moves$direction[k]
    )
    step_length <- if (moves$row[k] != 0 && moves$col[k] != 0) {
      sqrt(2)
    } else {
      1
    }

    edge_from[[k]] <- as.vector(M[from_rows, from_cols, drop = FALSE])
    edge_to[[k]] <- as.vector(M[to_rows, to_cols, drop = FALSE])
    edge_cost[[k]] <- as.vector(
      ((origin_cost + destination_cost) / 2) * step_length
    )
  }

  ii <- unlist(edge_from, use.names = FALSE)
  jj <- unlist(edge_to, use.names = FALSE)
  xx <- unlist(edge_cost, use.names = FALSE)

  tl <- sparseMatrix(i = ii, j = jj, x = xx, dims = c(ncells, ncells))
  if (output == "raw") {
    return(tl)
  }
  if (output == "transitionLayer") {
    if (inherits(direction_layer, "SpatRaster")) {
      direction_layer <- raster::raster(direction_layer)
    }
    tmp <- transition(direction_layer,
      transitionFunction = function(x) 0,
      directions = 8
    )
    transitionMatrix(tmp) <- sparseMatrix(
      i = ii, j = jj, x = 1 / xx,
      dims = c(ncells, ncells)
    )
    return(tmp)
  }
  return(NULL)
}



#' Compute flow-based cost or conductance
#'
#' \code{flow.dispersion} computes movement conductance through a flow such as
#' sea or wind currents. Passive movement uses a restricted version of the
#' formula described in Felícisimo et al. (2008):
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' where HorizontalFactor incrementally penalizes angular deviations from the
#' flow direction and prevents passive movement at deviations of 90 degrees or
#' more. Active movement uses a general relative index based on signed flow
#' support rather than the original formula.
#'
#'
#' @param x A `SpatRaster` returned by `wind2raster()` with direction and speed
#' layers, a legacy `RasterStack`, or a list of either type.
#' @param fun A function to compute the cost to move between cells. The default
#' is \code{cost.FMGS} from Felicísimo et al. (2008), see details.
#' @param output This argument allows to select different kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @param ... Further arguments passed to or from other methods.
#' @param wind.direction A vector or scalar containing wind directions.
#' @param wind.speed A vector or scalar containing wind speeds.
#' @param target direction of the target cell
#' @param type Movement model. In `"passive"` mode, deviations of 90 degrees
#' or more from the flow direction are forbidden. In `"active"` mode,
#' supportive flow lowers relative cost and opposing flow raises it; movement
#' against the flow remains possible.
#' @param speed.scale Positive reference speed for the active model. If `NULL`,
#' the median positive speed of the complete raster layer is used. For
#' comparisons among dates or regions, supply one fixed value in the same units
#' as `wind.speed`.
#' @return In "transitionLayer" output, the function returns conductance values
#' (1/cost)to move between all cells in a raster having into account flow speed
#' and direction obtained from wind.fit function("rWind" package). As wind or
#' sea currents implies directionality, flow.dispersion produces an anisotropic
#' conductance matrix (asymmetric). Conductance values are used later to built a
#' TransitionLayer object from "gdistance" package.
#'
#' In "raw" output, flow.dispersion creates a sparse Matrix with cost values.
#' Each edge cost is the mean of the local costs at its origin and destination.
#' Diagonal edges are multiplied by `sqrt(2)`.
#' @note The active model is a relative connectivity index, not a mechanistic
#' model of animal movement. It ignores the component of flow perpendicular to
#' movement.
#'
#' Costs are relative per grid step. The diagonal correction assumes a square
#' grid, but no correction is made for the changing physical distances of
#' longitude/latitude cells. Do not interpret results as physical time, energy,
#' or distance or compare them directly across grid resolutions or distant
#' latitude ranges.
#'
#' Note that for large data sets, it could take a while. For large study
#' areas is strongly advised perform the analysis in a remote computer or a
#' cluster.
#' @author Javier Fernández-López; Klaus Schliep; Yurena Arjona
#' @seealso \code{\link{wind.dl}}, \code{\link{wind2raster}}
#' @references
#'
#' Felicísimo, Á. M., Muñoz, J., & González-Solis, J. (2008). Ocean surface
#' winds drive dynamics of transoceanic aerial movements. PLoS One, 3(8),
#' e2928.
#'
#' Jacob van Etten (2017). R Package gdistance: Distances and Routes on
#' Geographical Grids. Journal of Statistical Software, 76(13), 1-21.
#' doi:10.18637/jss.v076.i13
#' @keywords ~anisotropy ~conductance
#' @examples
#'
#' require(gdistance)
#'
#' data(wind.data)
#'
#' wind <- wind2raster(wind.data)
#'
#' Conductance <- flow.dispersion(wind, type = "passive")
#'
#' transitionMatrix(Conductance)
#' image(transitionMatrix(Conductance))
#' @importFrom raster raster
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @export flow.dispersion
flow.dispersion <- function(x, fun = cost.FMGS, output = "transitionLayer",
                            type = c("passive", "active"),
                            speed.scale = NULL, ...) {
  type <- match.arg(type)
  if (inherits(x, "SpatRaster") || inherits(x, "Raster")) {
    return(flow.dispersion_int(x,
      fun = fun, output = output, type = type,
      speed.scale = speed.scale, ...
    ))
  }
  if (!is.list(x)) {
    stop("'x' must be a spatial raster or a list of spatial rasters", call. = FALSE)
  }
  lapply(x, flow.dispersion_int,
    fun = fun, output = output, type = type,
    speed.scale = speed.scale, ...
  )
}


#' Transforming a rWind_series object into a data.frame
#'
#' The output of tidy is always a data.frame. It is therefore suited for further
#' manipulation by packages like dplyr, reshape2, ggplot2 and ggvis.
#'
#' @param x	An object to be converted into a tidy data.frame
#' @param ... extra arguments
#' @examples
#' data(wind.series)
#' df <- tidy(wind.series)
#' head(df)
#' \dontrun{
#' # use the tidyverse
#' library(dplyr)
#' mean_speed <- tidy(wind.series) %>%
#'   group_by(lat, lon) %>%
#'   summarise(speed = mean(speed))
#' wind_average2 <- wind.mean(wind.series)
#' all.equal(wind_average2$speed, mean_speed$speed)
#' }
#' @rdname tidy.rWind_series
#' @export tidy
tidy <- function(x, ...) UseMethod("tidy")

#' @rdname tidy.rWind_series
#' @export
tidy.rWind_series <- function(x, ...) {
  l <- length(x)
  res <- x[[1]]
  if (l > 1) for (i in 2:l) res <- rbind(res, x[[i]])
  res
}





#' Wind-data mean
#'
#' wind.mean computes the mean (average) wind speed and wind direction of a time
#' series dataset of winds of the same region.
#' Summaries of time series are not trivial to compute. We compute the
#' arithmetic mean for the wind speed.
#' The direction as the circular mean, see
#' \url{https://en.wikipedia.org/wiki/Circular_mean}
#' for more details. The U and V components are afterwards transformed from
#' these values.
#' @param x An object of class \code{rWind_series}
#' @return An object of class \code{rWind}, which is a \code{data.frame}
#' @note For large time series, it could take a while.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~average ~mean
#' @examples
#' data(wind.series)
#' wind_average <- wind.mean(wind.series)
#' @export wind.mean
wind.mean <- function(x) {
  if (!inherits(x, "rWind_series")) stop("x needs to be of class rWind_series")

  tt <- as_datetime(names(x)[1])
  res <- cbind(tt, attr(x, "lat_lon"))

  x <- unclass(x)
  l <- length(x)
  tmpD <- tmpS <- matrix(0, nrow(x[[1]]), l)
  for (i in seq_len(l)) {
    tmp <- uv2ds(x[[i]][, 1], x[[i]][, 2])
    tmpD[, i] <- tmp[, 1]
    tmpS[, i] <- tmp[, 2]
  }
  smean <- apply(tmpS, 1, mean)
  dmean <- apply(tmpD, 1, circ.mean)
  res <- cbind(res, ds2uv(dmean, smean), dmean, smean)

  colnames(res) <- c(
    "time", "lat", "lon", "ugrd10m", "vgrd10m", "dir",
    "speed"
  )
  class(res) <- c("rWind", "data.frame")
  return(res)
}


###############################################################################
# Some new and experimental functions to download OSCAR Sea Surface Velocity data
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplOscar.html
# https://doi.org/10.5067/OSCAR-03D01
# This is a beta version, please use it carefully

oscar.fit_int <- function(tmpx) {
  tmpx <- cbind(tmpx[, 1], tmpx[, 3:6])
  ###### DIRECTION
  direction <- atan2(tmpx[, 4], tmpx[, 5])
  direction <- rad2deg(direction)
  dir_filter <- direction[!is.nan(direction)]
  dir_filter[dir_filter < 0] <- 360 + dir_filter[dir_filter < 0]
  direction[!is.nan(direction)] <- dir_filter
  ###### SPEED
  speed <- sqrt((tmpx[, 4] * tmpx[, 4]) + (tmpx[, 5] * tmpx[, 5]))
  ######
  names(tmpx) <- c("time", "lat", "lon", "u", "v")
  tmpx$lon <- tmpx$lon %% 360
  tmpx$lon[tmpx$lon >= 180] <- tmpx$lon[tmpx$lon >= 180] - 360
  res <- cbind(tmpx, dir = direction, speed = speed)
  res <- res[with(res, order(-lat)), ]
  res[, 1] <- ymd_hms(res[, 1], truncated = 3)
  return(res)
}

.oscar_url <- function(date, lon1, lon2, lat1, lat2) {
  limits <- c(lon1, lon2, lat1, lat2)
  if (length(limits) != 4L || any(!is.finite(limits))) {
    stop("Longitude and latitude limits must be four finite scalars", call. = FALSE)
  }
  if (any(c(lon1, lon2) < -180 | c(lon1, lon2) > 360)) {
    stop("Longitudes must be between -180 and 360 degrees", call. = FALSE)
  }
  if (lat1 > 80 || lat2 < -80 || lat1 < lat2) {
    stop("OSCAR latitude limits must satisfy 80 >= lat1 >= lat2 >= -80",
      call. = FALSE
    )
  }

  # jplOscar stores a continuous 20--420 degree longitude axis. Moving values
  # below 20 degrees into its repeated section avoids splitting most extents.
  oscar_lon <- function(x) {
    x <- x %% 360
    ifelse(x < 20, x + 360, x)
  }
  west <- oscar_lon(lon1)
  east <- oscar_lon(lon2)
  if (east <= west) east <- east + 360
  if (east > 420) {
    stop("The requested longitude interval cannot be represented by OSCAR",
      call. = FALSE
    )
  }

  stamp <- format(date, "%Y-%m-%dT00:00:00Z", tz = "UTC")
  subset <- paste0(
    "[(", stamp, ")][(15.0)][(", lat1, "):1:(", lat2,
    ")][(", west, "):1:(", east, ")]"
  )
  paste0(
    "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplOscar.csv?",
    "u", subset, ",v", subset
  )
}

#' OSCAR Sea currents data download
#'
#' seaOscar.dl downloads sea currents data from the Ocean Surface Current Analyses Real-time (OSCAR)
#' (PO.DAAC, \doi{10.5067/OSCAR-03D01}).
#' Geospatial resolution is 0.33 degrees and sea currents are calculated for
#' 15 m depth. Longitudes in the usual -180 to 180 notation are translated to
#' the 20 to 420 degree axis used by the service.
#'
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "sea_yyyy_mm_dd.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an R object (data.frame) is created.
#'
#' @param yyyy Selected year.
#' @param mm Selected month.
#' @param dd Selected day.
#' @param lon1 Western longitude
#' @param lon2 Eastern longitude
#' @param lat1 Northern latitude
#' @param lat2 Southern latitude
#' @param type Output type. "read-data" is selected by default, creating an R
#' object. If you choose "csv", seaOscar.dl create a a CSV file in your working
#' directory named "oscar_yyyy_mm_dd.csv".
#' @param trace if trace = 1 (by default) track downloaded files
#' @return "rWind" and "data.frame" class object or .csv file with U and V
#' vector  components and sea current direction and speed for each coordinate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @author Javier Fernández-López (jflopez.bio@@gmail.com)
#' @seealso \code{\link{wind.dl_2}}, \code{\link{wind2raster}}
#' @references
#' \doi{10.5067/OSCAR-03D01}
#' @keywords ~currents ~sea
#' @examples
#'
#' # Download sea currents for Galapagos Islands
#' \dontrun{
#'
#' seaOscar.dl(2015, 1, 1, -93, -88, 2, -3)
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd year month day hour
#' @rdname seaOscar.dl
#' @export seaOscar.dl

seaOscar.dl <- function(yyyy, mm, dd, lon1, lon2, lat1, lat2, type = "read-data", trace = 1) {
  type <- match.arg(type, c("read-data", "csv"))
  dt <- suppressWarnings(ymd(paste(yyyy, mm, dd, sep = "-")))
  if (length(dt) != 1L || is.na(dt)) {
    stop("'yyyy', 'mm', and 'dd' must define one valid date", call. = FALSE)
  }
  yyyy_c <- year(dt)
  mm_c <- sprintf("%02d", month(dt))
  dd_c <- sprintf("%02d", day(dt))
  if (trace) {
    message(format(dt), " downloading...")
  }
  url_dir <- .oscar_url(dt, lon1, lon2, lat1, lat2)
  tmp <- tryCatch(
    read.csv(url_dir, header = FALSE, skip = 2, stringsAsFactors = FALSE),
    error = function(e) {
      stop("OSCAR data are unavailable: ", conditionMessage(e), call. = FALSE)
    }
  )
  tmp <- oscar.fit_int(tmp)
  if (type == "csv") {
    fname <- paste0("oscar_", yyyy_c, "_", mm_c, "_", dd_c, ".csv")
    write.table(tmp, fname,
      sep = ",", row.names = FALSE,
      col.names = TRUE, quote = FALSE
    )
  }
  class(tmp) <- c("rWind", "data.frame")
  tmp
}
