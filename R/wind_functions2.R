
#' Wind-data download
#'
#' wind.dl downloads wind data from the Global Forecast System (GFS) of the
#' USA's National Weather Service (NWS)
#' (https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs).
#' Wind data are taken from NOAA/NCEP Global Forecast System (GFS) Atmospheric
#' Model colection. Geospatial resolution is 0.5 degrees (approximately 50 km),
#' and wind is calculated for Earth surface, at 10 m. More metadata
#' information:
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#'
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "wind_yyyy_mm_dd_tt.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an R object (data.frame) is created.
#'
#' @param yyyy Selected start year (FROM).
#' @param mm Selected start month (FROM).
#' @param dd Selected start day (FROM).
#' @param tt Selected start time. There are currently several options at the GFS
#' database: 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00
#' (UTC) (FROM).
#' @param yyyy2 Selected finish year (TO).
#' @param mm2 Selected finish month (TO).
#' @param dd2 Selected finish day (TO).
#' @param tt2 Selected finish time. There are currently several options at the GFS
#' database: 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00
#' (UTC) (TO).
#' @param lon1 Western longitude
#' @param lon2 Eastern longitude
#' @param lat1 Southern latitude
#' @param lat2 Northern latitude
#' @param by Time lapse between downloaded data (from 3 hours)
#' @param type Output type. "read-data" is selected by default, creating an R
#' object. If you choose "csv", wind.dl create a a CSV file in your work
#' directory named "wind_yyyy_mm_dd_tt.csv".
#' @param trace if trace = 1 (by default) track downloaded files
#' @return "rWind list" class object (a list of data.frames) or .csv file/s with
#' U and V vector  components and wind direction and speed for each coordenate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note wind.dl requires two dates that represent the boundaries of the time
#' lapse to download wind series data.
#' U and V vector components allow you to create wind averages or tendences
#' for each coordenate at the study area. Longitude coordenates are
#' provided by GFS dataset in 0/360 notation and transformed internaly into
#' -180/180.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.stats}}, \code{\link{wind2raster}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.graph
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' \dontrun{
#'
#' wind.dl(2015,2,12,0,2015,2,12,0,-10,5,35,45)
#'
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd_h year month day hour
#' @rdname wind.dl
#' @export wind.dl

wind.dl <- function (yyyy,mm,dd,tt,yyyy2,mm2,dd2,tt2,lon1,lon2,lat1,lat2,
                     by="3 hours",type="read-data", trace=1){

  type <- match.arg(type, c("read-data", "csv"))
  resultados <- list() # We will store each date and time in a list

  mm<-sprintf("%02d", mm)
  dd<-sprintf("%02d", dd)
  tt<-sprintf("%02d", tt)

  mm2<-sprintf("%02d", mm2)
  dd2<-sprintf("%02d", dd2)
  tt2<-sprintf("%02d", tt2)

  # Create a sequence with all dates available between selected dates
  dt <- seq(ymd_h(paste(yyyy,mm,dd,tt, sep="-")),
            ymd_h(paste(yyyy2,mm2,dd2,tt2, sep="-")),by=by)

  for (id in 1:length(dt)) {

    yyyy_c <- year(dt[id])
    mm_c <- sprintf("%02d",month(dt[id]))
    dd_c <- sprintf("%02d",day(dt[id]))
    tt_c <- sprintf("%02d",hour(dt[id]))

    testDate <- paste(yyyy_c,"-",mm_c,"-",dd_c, sep="")

    if(trace)print(paste( ymd_h(paste(yyyy_c,mm_c,dd_c,tt_c, sep="-")),
                          "downloading...", sep= " "))

    tryCatch({
      as.Date(testDate)
      if (lon1 < 0){
        lon1<-360-(abs(lon1))
      }
      if (lon2 < 0){
        lon2<-360-(abs(lon2))
      }

      if (lon1 > 180 && lon2 <180){
        url_west<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
        url_east<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
        tmp<-rbind(read.csv(url_west, header=FALSE, skip=2, stringsAsFactors=FALSE),
                   read.csv(url_east, header=FALSE, skip=2, stringsAsFactors=FALSE))
        tmp <- wind.fit_int(tmp)
        if (type == "csv"){
          fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                         "_",tt_c,".csv", sep="")
          write.table(tmp, fname, sep = ",", row.names = FALSE,
                      col.names = TRUE, quote = FALSE)
        }
        else{
          resultados[[id]] <- tmp
        }
      }

      else {
        url_dir<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
        tmp <- read.csv(url_dir, header=FALSE, skip=2, stringsAsFactors=FALSE)
        tmp <- wind.fit_int(tmp)
        if (type == "csv"){
          fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                         "_",tt_c,".csv", sep="")
          write.table(tmp, fname, sep = ",", row.names = FALSE,
                      col.names = TRUE, quote = FALSE)
        }
        else{
          resultados[[id]] <- tmp
        }
      }
    },
    error=function(e){cat("ERROR: database not found. Please, check server
                          connection, date or geographical ranges \n")},
    warning=function(w){cat("ERROR: database not found. Please, check server
                            connection, date or geographical ranges  \n")}
    )

  }
  class(resultados) <-  c("rWind", "list")
  return(resultados)
}

#' Wind-data download
#'
#' wind.dl_2 downloads wind data from the Global Forecast System (GFS) of the
#' USA's National Weather Service (NWS)
#' (https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs).
#' Wind data are taken from NOAA/NCEP Global Forecast System (GFS) Atmospheric
#' Model colection. Geospatial resolution is 0.5 degrees (approximately 50 km),
#' and wind is calculated for Earth surface, at 10 m. More metadata
#' information:
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#'
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "wind_yyyy_mm_dd_tt.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an R object (data.frame) is created.
#'
#' @param time a scalar or vector of POSIXt or Date objects or an character
#' which can transfored into those, see example below.
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
#' @return "rWind list" class object (a list of data.frames) or .csv file/s with
#' U and V vector  components and wind direction and speed for each coordenate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note wind.dl requires two dates that represent the boundaries of the time
#' lapse to download wind series data.
#' U and V vector components allow you to create wind averages or tendences
#' for each coordenate at the study area. Longitude coordenates are
#' provided by GFS dataset in 0/360 notation and transformed internaly into
#' -180/180.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.stats}}, \code{\link{wind2raster}},
#' \code{\link{wind.dl}}, \code{\link{as_datetime}}, \code{\link{as.POSIXct}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.graph
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' \dontrun{
#'
#' wind.dl_2("2018/3/15 9:00:00",-10,5,35,45)
#'
#' # library(lubridate)
#' # dt <- seq(ymd_h(paste(2018,1,1,00, sep="-")),
#' #           ymd_h(paste(2018,1,2,21, sep="-")),by="3 hours")
#' #  wind.dl_2(dt,-10,5,35,45)
#'
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd_h year month day hour as_datetime
#' @rdname wind.dl_2
#' @export wind.dl_2
#'
wind.dl_2 <- function(time, lon1, lon2, lat1, lat2, type="read-data", trace=1){

    type <- match.arg(type, c("read-data", "csv"))
    resultados <- list() # We will store each date and time in a list

    dt <- as_datetime(time)

# Create a sequence with all dates available between selected dates
#    dt <- seq(ymd_h(paste(yyyy,mm,dd,tt, sep="-")),
#              ymd_h(paste(yyyy2,mm2,dd2,tt2, sep="-")),by="3 hours")

    for (id in seq_along(dt)) {

        yyyy_c <- year(dt[id])
        mm_c <- sprintf("%02d",month(dt[id]))
        dd_c <- sprintf("%02d",day(dt[id]))
        tt_c <- sprintf("%02d",hour(dt[id]))

        testDate <- paste(yyyy_c,"-",mm_c,"-",dd_c, sep="")

        if(trace)print(paste( ymd_h(paste(yyyy_c,mm_c,dd_c,tt_c, sep="-")),
                              "downloading...", sep= " "))

        tryCatch({
            as.Date(testDate)
            if (lon1 < 0){
                lon1<-360-(abs(lon1))
            }
            if (lon2 < 0){
                lon2<-360-(abs(lon2))
            }

            if (lon1 > 180 && lon2 <180){
                url_west<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
                url_east<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")

                tmp<-rbind(read.csv(url_west, header=FALSE, skip=2,
                                    stringsAsFactors=FALSE),
                           read.csv(url_east, header=FALSE, skip=2,
                                    stringsAsFactors=FALSE))
                tmp <- wind.fit_int(tmp)
                if (type == "csv"){
                    fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                                   "_",tt_c,".csv", sep="")
                    write.table(tmp, fname, sep = ",", row.names = FALSE,
                                col.names = TRUE, quote = FALSE)
                }
                else{
                    resultados[[id]] <- tmp
                }
            }

            else {
                url_dir<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
                tmp <- read.csv(url_dir, header=FALSE, skip=2,
                                colClasses = c("POSIXct", "double", "double", "double", "double"))

                tmp <- wind.fit_int(tmp)
                if (type == "csv"){
                    fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                                   "_",tt_c,".csv", sep="")
                    write.table(tmp, fname, sep = ",", row.names = FALSE,
                                col.names = TRUE, quote = FALSE)
                }
                else{
                    resultados[[id]] <- tmp
                }
            }
        },
        error=function(e){cat("ERROR: database not found. Please, check server
                          connection, date or geographical ranges \n")},
        warning=function(w){cat("ERROR: database not found. Please, check server
                            connection, date or geographical ranges  \n")}
        )

    }
    class(resultados) <-  c("rWind", "list")
    return(resultados)
}


#' wind.fit_int
#'
#' wind.fit_int is used internaly by wind.dl to transform downloaded data from
#' GFS. wind.fit_int applies trigonometry tools to transform U and V vector wind
#' components in wind direction and speed features. It also transforms 0-360
#' longitude notation obtained from GFS data into -180/180 longitude notation.
#' Moreover, it cleans dates names and sorts the data by latitude.
#'
#' @param X downloaded data by wind.dl function from "rWind" package.
#' @return data.frame
#' @note This function is used internaly by wind.dl
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.stats}},
#' \code{\link{wind2raster}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~wind ~gfs
#' @examples
#'
#' # wind.dl(2015,2,12,0,2015,2,12,0,-10,5,35,45)
#'
#' @importFrom lubridate ymd_hms
#' @rdname wind.fit_int
#' @keywords internal
wind.fit_int <- function (tmpx) {
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  tmpx[,3] <- tmpx[,3] %% 360
  tmpx[tmpx[,3]>=180,3] <- tmpx[tmpx[,3]>=180,3] - 360

  ###### DIRECTION
  direction <- atan2(tmpx[,4], tmpx[,5])
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED
  speed <- sqrt( (tmpx[,4] * tmpx[,4]) + (tmpx[,5] * tmpx[,5]))

  ######
  names(tmpx)<- c("time", "lat","lon", "ugrd10m", "vgrd10m")
  res <- cbind(tmpx, dir=direction, speed=speed)
  res <- res[with(res, order(-lat)), ]
  res[,1] <- ymd_hms(res[,1], truncated = 3)
  return(res)
}

#' Transform U and V components in direction and speed
#'
#'
#' @param u U component.
#' @param v U component.
#' @return "uv2ds" returns a matrix with direction and speed values
#' @note Multiple U and V values can be procesed.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.stats}}, \code{\link{wind2raster}}
#' @keywords ~wind
#' @examples
#'
#' \dontrun{
#'
#' uv2ds(c(1,1,3,1), c(1,1.7,3,1))
#'
#' }
#'
#' @rdname uv2ds
#' @export uv2ds

uv2ds <- function (u,v) {
    rad2deg <- function(rad) {(rad * 180) / (pi)}

    ###### DIRECTION
    direction <- atan2(u, v)
    direction <- rad2deg(direction)
    direction[direction < 0] <- 360 + direction[direction < 0]

    ###### SPEED
    speed <- sqrt( (u * u) + (v * v))

    ######
    res <- cbind(dir=direction, speed=speed)
    return(res)
}


#' Transform direction and speed in U and V components
#'
#'
#' @param d direction (degrees).
#' @param s speed (m/s).
#' @return "ds2uv" returns a matrix with U and V values
#' @note Multiple speed and direction values can be procesed.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.stats}}, \code{\link{wind2raster}}
#' @keywords ~wind
#' @examples
#'
#' \dontrun{
#'
#' ds2uv(c(32,78,300,234),c(6,2,8,3.2))
#'
#' }
#'
#' @rdname ds2uv
#' @export ds2uv

ds2uv <- function(d,s){
    deg2rad <- function(deg) {(deg * pi) / (180)}
    d <- d %% 360
    r <- deg2rad(d)
    u <- sin(r) * s
    v <- cos(r) * s
    cbind(u=u, v=v)
}



#' Wind-data to raster file
#'
#' wind2raster_int crates a raster file (gridded) from a data.frame created by
#' wind.fit function from "rWind" package. Latitude and logitude values are used
#' to locate raster file and create raster resolution using rasterFromXYZ
#' function from raster package. As raster files only can store one field of
#' information, you should choose between direction (by default, type="dir")
#' and speed (type="speed") to be represented by the new raster file.
#'
#' WGS84 datum (non-projected) CRS is selected by default to build the raster
#' file.
#'
#' @param x a data.frame obtained by wind.fit
#' @return A raster file representing wind direction, wind speed or both of the
#' study area.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind2raster}}
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' data(wind_data)
#'
#' # Fit downloaded dataset to be plotted
#' #wind_fitted_data <- wind.fit(wind_data)
#'
#' # Create raster file from the data.frame created by wind.fit, representing wind speed.
#' #wind2raster(wind_fitted_data, type="speed")
#'
#' @importFrom raster rasterFromXYZ stack
#'
#' @rdname wind2raster_int
#' @keywords internal
wind2raster_int<- function(x){
  ras <- rasterFromXYZ(x[,c("lon","lat")], crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  ras2 <- ras
  ras[] <- x$dir
  ras2[] <- x$speed
  tmp <- stack (ras, ras2)
  names(tmp)<- c("wind.direction", "wind.speed")
  return(tmp)
}

#' Wind-data to raster file
#'
#' wind2raster crates a raster stack (gridded) with 2 layers: wind speed and
#' wind direction from the output of wind.dl function from "rWind" package.
#' Latitude and logitude values are used to locate raster file and to create
#' raster using rasterFromXYZ function from raster package. If input file is a
#' list of wind data created by wind.dl, a list of raster stacks will be
#' returned
#'
#' WGS84 datum (non-projected) CRS is selected by default to build the raster
#' file.
#'
#' @param x an "rWind list" obtained by wind.fit
#' @return A raster stack or a list of raster stacks representing wind direction
#' and speed.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind.dl(2015,2,12,0,2015,2,12,0,-10,5,35,45)
#'
#' data(wind.data)
#'
#' # Create raster stack from the downloaded data with wind directon and speed
#' # layers
#'
#' wind2raster(wind.data)
#'
#' @importFrom raster rasterFromXYZ stack
#'
#' @rdname wind2raster
#' @export wind2raster
wind2raster<- function(x){
  X <- lapply(x , wind2raster_int)
  if (length(x) == 1) return (X[[1]])
  X
}

#' Wind-data stats
#'
#' wind.stats computes any stats (mean, max, min, etc.) of a time series dataset
#' of winds in the same region. To do this, wind.stats uses U and V vector
#' components of several wind data.frames stored in a list.
#'
#' @param wind_series A list of data.frames downloaded by wind.dl function.
#' @param fun any stats function.
#' @return A data.frame with a similar format as resulted by wind.dl
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#'
#' # Use wind.dl to download and store wind data of
#' # the January 3rd 2015 at several hours around New Zealand.
#'
#' data(wind.series)
#'
#' # Use wind.stats to compute several measures:
#'
#' wind.mean<- wind.stats(wind.series, fun=mean)
#' wind.median<- wind.stats(wind.series, fun=median)
#' wind.max<- wind.stats(wind.series, fun=max)
#'
#'
#' @export wind.stats
wind.stats <- function(wind_series, fun=mean){
  options(scipen = 999)
  wind_mean <- cbind(data.frame(wind_series[[1]][,1]),
                     data.frame(wind_series[[1]][,2]),
                     data.frame(wind_series[[1]][,3]))

  l <- length(wind_series)
  row_mean_matrix <- matrix(NA, nrow(wind_series[[1]]), l)

  for (h in 1:l) row_mean_matrix[,h] <- wind_series[[h]][,4]

  umean <- apply(row_mean_matrix, 1, fun)

  row_mean_matrix[] <- NA

  for (h in 1:l) row_mean_matrix[,h] <- wind_series[[h]][,5]

  vmean <- apply(row_mean_matrix, 1, fun)

  wind_mean<-cbind(wind_mean,umean,vmean)
  tmp <- wind.fit_int(wind_mean)
  return(tmp)
}


#' Arrow direction fitting for Arrowhead function from "shape" package
#'
#' arrowDir adapts wind direction value to be used by Arrowhead function from
#' "shape" package to plot wind direction for each coordinate.
#'
#' Angle argument of Arrowhead function from "shape" package needs to be fed
#' in an anti-clockwise way, relative to x-axis, in degrees [0,360]. arrowDir
#' function adapts wind direction provided by wind.fit (clockwise, relative to
#' y-axis ) to requirements of Arrowhead.
#'
#' @param W A data.frame obtained by function wind.fit. It should content a
#' column named "dir".
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
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' #data(wind_data)
#'
#' # Fit downloaded dataset to be plotted
#' #wind_fitted_data <- wind.fit(wind_data)
#'
#' # Create a vector with wind direction (angles) adapted
#' #alpha=arrowDir(wind_fitted_data)
#'
#' # Now, you can plot wind direction with Arrowhead function from shapes package
#' #Load "shape package
#' #require(shape)
#' # plot(wind_data$lon, wind_data$lat, type="n")
#' # Arrowhead(wind_data$lon, wind_data$lat, angle=alpha, arr.length = 0.1, arr.type="curved")
#'
#'
#' @export arrowDir
arrowDir <- function(W){
  aDir<-(360-W$dir) + 90
  return(aDir)
}


#' Compute flow-based cost or conductance
#'
#' flow.dispersion_int computes movement conductance through a flow either, sea or
#' wind currents. It implements the formula described in Felícisimo et al.
#' 2008:
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' being HorizontalFactor a "function that incrementaly penalized angular
#' deviations from the wind direction" (Felicísimo et al. 2008).
#'
#' @param stack RasterStack object with layers obtained from wind2raster
#' function ("rWind" package) with direction and speed flow values.
#' @param type Could be either "passive" or "active".In "passive" mode,
#' movement against flow direction is not allowed (deviations from the wind
#' direction higher than 90). In "active" mode, the movement can go against flow
#' direction, by increasing the cost.
#' @param output This argument allows to select diferent kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @return In "transitionLayer" output, the function returns conductance values
#' (1/cost)to move betwen all cells in a raster having into account flow speed
#' and direction obtained from wind.fit function("rWind" package). As wind or
#' sea currents implies directionality, flow.dispersion produces an anisotropic
#' conductance matrix (asimetric). Conductance values are used later to built a
#' TransitionLayer object from "gdistance" package.
#'
#' In "raw" output, flow.dispersion creates a sparse Matrix with cost values.
#' @note Note that for large data sets, it could take a while. For large study
#' areas is strongly adviced perform the analysis in a remote computer or a
#' cluster.
#' @author Javier Fernández-López; Klaus Schliep
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
#' # require(gdistance)
#'
#' # w<-wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' # data(wind_data)
#' # w<-wind.fit(wind_data)
#'
#' # wind <- wind2raster(w, type="stack")
#'
#' # Conductance<-flow.dispersion(wind,"passive", "transitionLayer")
#'
#' # transitionMatrix(Conductance)
#' # image(transitionMatrix(Conductance))
#'
#' @importClassesFrom raster RasterLayer
#' @importFrom raster ncell
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @keywords internal

flow.dispersion_int <-function(stack, type="passive", output="raw"){

  type <- match.arg(type, c("passive", "active"))
  output <- match.arg(output, c("raw", "transitionLayer"))

  DL <- as.matrix(stack$wind.direction)
  SL <- as.matrix(stack$wind.speed)
  M <- matrix(as.integer(1:ncell(stack$wind.direction)),
              nrow = nrow(stack$wind.direction), byrow=TRUE)
  nr <- nrow(M)
  nc <- ncol(M)

  ###################################################################
  # Cost computation following Muñoz et al., 2004; Felicísimo et al., 2008

  cost.Felicisimo<- function(wind,celda,type="passive"){
    dif=(abs(wind-celda))
    dif[dif > 180] = 360 - dif[dif > 180]
    if (type=="passive"){
      dif[dif >= 90] = Inf # check
      dif[dif < 90] = 2 * dif[dif < 90]
      dif[dif==0] = 0.1
    }
    else {
      dif[dif < 90] = 2 * dif[dif < 90]
      dif[dif==0] = 0.1
    }
    dif
  }

  ###################################################################

  directions <- c(315 ,0, 45, 270, 90, 225, 180,135 )


  ###################################################################

  # Go Nortwest

  north.west.from <- as.vector(M[-1,-1])
  north.west.to <- as.vector(M[-nr,-nc])
  north.west.cost <-cost.Felicisimo(DL[-1,-1],directions[1], type) / SL[-1,-1]

  ###################################################################

  # Go North

  north.from <- as.vector(M[-1,])
  north.to <- as.vector(M[-nr,])
  north.cost <- as.vector( cost.Felicisimo(DL[-1,],directions[2], type) / SL[-1,] )


  ###################################################################

  # Go Norteast

  north.east.from <- as.vector(M[-1,-nc])
  north.east.to <- as.vector(M[-nr,-1])
  north.east.cost <- as.vector( cost.Felicisimo(DL[-1,-nc],directions[3], type) / SL[-1,-nc] )

  ###################################################################

  # Go West

  west.from <- as.vector(M[,-1])
  west.to <- as.vector(M[,-nc])
  west.cost <- as.vector( cost.Felicisimo(DL[,-1],directions[4], type) / SL[,-1] )

  ###################################################################

  # Go East

  east.from <- as.vector(M[,-nc])
  east.to <- as.vector(M[,-1])
  east.cost <- as.vector( cost.Felicisimo(DL[,-nc],directions[5], type) / SL[,-nc] )

  ###################################################################

  # Go Southwest

  south.west.from <- as.vector(M[-nr,-1])
  south.west.to <- as.vector(M[-1,-nc])
  south.west.cost <- as.vector( cost.Felicisimo(DL[-nr,-1],directions[6], type) / SL[-nr,-1] )

  ###################################################################

  # Go South

  south.from <- as.vector(M[-nr,])
  south.to <- as.vector(M[-1,])
  south.cost <- as.vector( cost.Felicisimo(DL[-nr,],directions[7], type) / SL[-nr,] )

  ###################################################################

  # Go Southeast

  south.east.from <- as.vector(M[-nr,-nc])
  south.east.to <- as.vector(M[-1,-1])
  south.east.cost <- as.vector( cost.Felicisimo(DL[-nr,-nc],directions[8], type) / SL[-nr,-nc] )

  ###################################################################

  ii <- c(north.west.from, north.from, north.east.from, west.from, east.from, south.west.from, south.from, south.east.from)
  jj <- c(north.west.to, north.to, north.east.to, west.to, east.to, south.west.to, south.to, south.east.to)
  xx <- c(north.west.cost, north.cost, north.east.cost, west.cost, east.cost, south.west.cost, south.cost, south.east.cost)


  tl <- sparseMatrix(i=ii, j=jj, x=xx)
  if(output == "raw") return(tl)
  if(output == "transitionLayer") {
    tmp <- transition(stack$wind.direction, transitionFunction=function(x) 0, directions=8)
    transitionMatrix(tmp)<-sparseMatrix(i=ii, j=jj, x= 1 / xx)
    #        transitionMatrix(tl)<-replace(transitionMatrix(tl), is.infinite(transitionMatrix(tl)), 0)
    return(tmp)
  }

  return(NULL)
}

#' Compute flow-based cost or conductance
#'
#' flow.dispersion_int computes movement conductance through a flow either, sea or
#' wind currents. It implements the formula described in Felícisimo et al.
#' 2008:
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' being HorizontalFactor a "function that incrementaly penalized angular
#' deviations from the wind direction" (Felicísimo et al. 2008).
#'
#' @param x RasterStack object with layers obtained from wind2raster
#' function ("rWind" package) with direction and speed flow values.
#' @param type Could be either "passive" or "active".In "passive" mode,
#' movement against flow direction is not allowed (deviations from the wind
#' direction higher than 90). In "active" mode, the movement can go against flow
#' direction, by increasing the cost.
#' @param output This argument allows to select diferent kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @return In "transitionLayer" output, the function returns conductance values
#' (1/cost)to move betwen all cells in a raster having into account flow speed
#' and direction obtained from wind.fit function("rWind" package). As wind or
#' sea currents implies directionality, flow.dispersion produces an anisotropic
#' conductance matrix (asimetric). Conductance values are used later to built a
#' TransitionLayer object from "gdistance" package.
#'
#' In "raw" output, flow.dispersion creates a sparse Matrix with cost values.
#' @note Note that for large data sets, it could take a while. For large study
#' areas is strongly adviced perform the analysis in a remote computer or a
#' cluster.
#' @author Javier Fernández-López; Klaus Schliep
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
#' # require(gdistance)
#'
#' # w<-wind.dl_2("2015/2/12 00:00:00",-10,5,35,45)
#'
#' # data(wind_data)
#'
#' # wind <- wind2raster(w)
#'
#' # Conductance<-flow.dispersion(wind,"passive", "transitionLayer")
#'
#' # transitionMatrix(Conductance)
#' # image(transitionMatrix(Conductance))
#'
#' @importClassesFrom raster RasterLayer
#' @importFrom raster ncell
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @export flow.dispersion
#'
flow.dispersion <- function(x, type = "passive", output = "raw") {
    if(inherits(x, "RasterStack")){
        return(flow.dispersion_int(x, type=type, output=output))
    }
    lapply(x, flow.dispersion_int, type=type, output=output)
}

