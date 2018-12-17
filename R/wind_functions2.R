# some trigonemetric functions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# circular mean
# https://en.wikipedia.org/wiki/Mean_of_circular_quantities
circ.mean <- function(deg){
    rad.m <- (deg * pi) / (180)
    mean.cos <- mean(cos(rad.m))
    mean.sin <- mean(sin(rad.m))

    theta <- rad2deg(atan(mean.sin/mean.cos))
    if(mean.cos < 0) theta <- theta + 180
    if((mean.sin < 0) & (mean.cos > 0)) theta <- theta + 360
    theta
}


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
#' @param file file name of the saved ".csv" files.
#' @return "rWind" and "data.frame" class object or .csv file with U and V
#' vector  components and wind direction and speed for each coordenate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note Longitude coordenates are provided by GFS dataset in 0/360 notation
#' and transformed internaly into -180/180. Wind "dir" denotes where the
#' wind is going (toward), not from where is coming.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl_2}}, \code{\link{wind2raster}}
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
#' wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd_h year month day hour
#' @rdname wind.dl
#' @export wind.dl
wind.dl <- function (yyyy,mm,dd,tt,lon1,lon2,lat1,lat2,
                         type="read-data", trace=1){

    type <- match.arg(type, c("read-data", "csv"))

    mm<-sprintf("%02d", mm)
    dd<-sprintf("%02d", dd)
    tt<-sprintf("%02d", tt)

    # Create a sequence with all dates available between selected dates
    dt <- ymd_h(paste(yyyy,mm,dd,tt, sep="-"))

    yyyy_c <- year(dt)
    mm_c <- sprintf("%02d",month(dt))
    dd_c <- sprintf("%02d",day(dt))
    tt_c <- sprintf("%02d",hour(dt))

    testDate <- paste(yyyy_c,"-",mm_c,"-",dd_c, sep="")
    print(testDate)
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
        }
    },
    error=function(e){cat("ERROR: database not found. Please, check server
                      connection, date or geographical ranges \n")},
    warning=function(w){cat("ERROR: database not found. Please, check server
                        connection, date or geographical ranges  \n")}
    )
    class(tmp) <-  c("rWind", "data.frame")
    return(tmp)
}


#' @rdname wind.dl
#' @export
read.rWind <- function(file){
    tmp <- read.csv(file, colClasses = c("POSIXct", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric"))
    tmp[,1] <- ymd_hms(tmp[,1], truncated = 3)
    class(tmp) <- c("rWind", "data.frame")
    tmp
}



#' Wind-data download
#'
#' wind.dl_2 downloads time-series wind data from the Global Forecast System
#' (GFS) of the USA's National Weather Service (NWS)
#' (https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs).
#' Wind data are taken from NOAA/NCEP Global Forecast System (GFS) Atmospheric
#' Model colection. Geospatial resolution is 0.5 degrees (approximately 50 km),
#' and wind is calculated for Earth surface, at 10 m. More metadata
#' information:
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#'
#' To get the same format as wind.dl, you should run \code{tidy} function from
#' wind.dl_2 output.
#' The output type is determined by type="csv" or type="read-data". If
#' type="csv" is selected, the function creates a "wind_yyyy_mm_dd_tt.csv" file
#' that is downloaded at the work directory. If type="read-data" is selected,
#' an \code{rWind_series} object is created.
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
#' @return an object of class \code{rWind_series} or .csv file/s with
#' U and V vector components and wind direction and speed for each coordenate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @note wind.dl_2 requires two dates that represent the boundaries of the time
#' lapse to download wind series data.
#' U and V vector components allow you to create wind averages or tendences
#' for each coordenate at the study area. Longitude coordenates are
#' provided by GFS dataset in 0/360 notation and transformed internaly into
#' -180/180.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.mean}}, \code{\link{wind2raster}},
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
#' library(lubridate)
#' dt <- seq(ymd_hms(paste(2018,1,1,00,00,00, sep="-")),
#'           ymd_hms(paste(2018,1,2,21,00,00, sep="-")),by="3 hours")
#' ww <- wind.dl_2(dt,-10,5,35,45)
#' tidy (ww)
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

    dt <- as_datetime(time)
    # We will store each date and time in a list
    resultados <- vector("list", length(dt))
    names(resultados) <- dt

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
                    tmp <- wind.fit_int(tmp)
                    fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                                   "_",tt_c,".csv", sep="")
                    write.table(tmp, fname, sep = ",", row.names = FALSE,
                                col.names = TRUE, quote = FALSE)
                }
                else{
                    resultados[[id]] <- tmp[, 4:5]
                }
            }

            else {
                url_dir<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")],vgrd10m[(",yyyy_c,"-",mm_c,"-",dd_c,"T",tt_c,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
                tmp <- read.csv(url_dir, header=FALSE, skip=2,
                                colClasses = c("POSIXct", "double", "double", "double", "double"))
                tmp <- wind.fit_int(tmp)
                if (type == "csv"){
                    tmp <- wind.fit_int(tmp)
                    fname <- paste("wind_",yyyy_c,"_",mm_c,"_",dd_c,
                                   "_",tt_c,".csv", sep="")
                    write.table(tmp, fname, sep = ",", row.names = FALSE,
                                col.names = TRUE, quote = FALSE)
                }
                else{
                    resultados[[id]] <- tmp[, 4:5]
                }
            }
        },
        error=function(e){cat("ERROR: database not found. Please, check server
                          connection, date or geographical ranges \n")},
        warning=function(w){cat("ERROR: database not found. Please, check server
                            connection, date or geographical ranges  \n")}
        )

    }

    if(type == "csv") return(NULL)
    attr(resultados, "lat_lon") <- tmp[,2:3]
    class(resultados) <-  c("rWind_series", "list")
    return(resultados)
}


#' @rdname wind.dl_2
#' @param x object from which to extract element(s).
#' @param i indices specifying elements to extract.
#' @param exact Controls possible partial matching (not used yet).
#' @export
"[[.rWind_series" <- function(x, i, exact=TRUE){
    tt <- as_datetime(names(x)[i])
    tmp <- cbind(tt, attr(x, "lat_lon"), unclass(x)[[i]])
    tmp <- wind.fit_int(tmp)
    class(tmp) <-  c("rWind", "data.frame")
    tmp
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
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.mean}},
#' \code{\link{wind2raster}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~wind ~gfs
#' @importFrom lubridate ymd_hms
#' @rdname wind.fit_int
#' @keywords internal
wind.fit_int <- function (tmpx) {
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

#' Transform U and V components in direction and speed and vice versa
#'
#'
#' @param u U component.
#' @param v U component.
#' @return "uv2ds" returns a matrix with direction and speed values
#' @note Multiple U and V values can be procesed.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.mean}}, \code{\link{wind2raster}}
#' @keywords ~wind
#' @examples
#'
#' ( ds <- uv2ds(c(1,1,3,1), c(1,1.7,3,1)) )
#' ds2uv(ds[,1], ds[,2])
#'
#'
#' @rdname uv2ds
#' @export uv2ds

uv2ds <- function (u,v) {
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


#' @param d direction (degrees).
#' @param s speed (m/s).
#' @return "ds2uv" returns a matrix with U and V values
#' @rdname uv2ds
#' @export ds2uv

ds2uv <- function(d,s){
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
#' @param x an object of class \code{rWind}
#' @return A raster file representing wind direction, wind speed or both of the
#' study area.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind2raster}}
#' @keywords ~gfs ~wind
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
#' wind direction for an object of \code{rWind}.
#' Latitude and logitude values are used to locate raster file and to create
#' raster using rasterFromXYZ function from raster package. If the input file is
#' a list of wind data created by wind.dl, a list of raster stacks will be
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
  if(inherits(x, "rWind_series"))X <- lapply(x , wind2raster_int)
  else return (wind2raster_int(x))
  X
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
#'
#' \dontrun{
#' # Now, you can plot wind direction with Arrowhead function from shapes package
#' # Load "shape package
#' require(shape)
#' plot(wind.data$lon, wind.data$lat, type="n")
#' Arrowhead(wind.data$lon, wind.data$lat, angle=alpha,
#'           arr.length = 0.1, arr.type="curved")
#' }
#'
#' @export arrowDir
arrowDir <- function(W){
  if(inherits(W, "rWind_series")){
    if(length(W)>1) message("W contained a time series, just took first time point!")
    W <- W[[1]]
  }
  aDir <- (360 - W$dir) + 90
  return(aDir)
}


# Cost computation following Muñoz et al., 2004; Felicísimo et al., 2008
#' @rdname flow.dispersion
#' @export
cost.FMGS <- function(wind.direction, wind.speed, target, type="active"){
    dif <- (abs(wind.direction - target))
    dif[dif > 180] <- 360 - dif[dif > 180]
    if (type=="passive"){
        dif[dif >= 90] <- Inf # check
        dif[dif < 90] <- 2 * dif[dif < 90]
        dif[dif==0] <- 0.1
    }
    else {
        dif[dif < 90] <- 2 * dif[dif < 90]
        dif[dif==0] <- 0.1
    }
    dif / wind.speed
}


#' Compute flow-based cost or conductance
#'
#' flow.dispersion_int computes movement conductance through a flow either, sea
#' or wind currents. It implements the formula described in Felícisimo et al.
#' 2008:
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' being HorizontalFactor a "function that incrementaly penalized angular
#' deviations from the wind direction" (Felicísimo et al. 2008).
#'
#' @param stack RasterStack object with layers obtained from wind2raster
#' function ("rWind" package) with direction and speed flow values.
#' @param fun A function to compute the cost to move between cells. The default
#' is \code{cost.FMGS} from Felicísimo et al. (2008), see details.
#' @param output This argument allows to select diferent kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @param ... Further arguments passed to or from other methods.
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
#' data(wind.data)
#' wind <- wind2raster(wind.data)
#' Conductance <- flow.dispersion(wind, type="passive")
#'
#' \dontrun{
#' require(gdistance)
#' transitionMatrix(Conductance)
#' image(transitionMatrix(Conductance))
#' }
#' @importClassesFrom raster RasterLayer
#' @importFrom raster ncell
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @keywords internal
flow.dispersion_int <- function(stack, fun=cost.FMGS, output="transitionLayer",
                                ...){

    output <- match.arg(output, c("raw", "transitionLayer"))

    DL <- as.matrix(stack$wind.direction)
    SL <- as.matrix(stack$wind.speed)
    M <- matrix(as.integer(1:ncell(stack$wind.direction)),
                nrow = nrow(stack$wind.direction), byrow=TRUE)
    nr <- nrow(M)
    nc <- ncol(M)

    ###################################################################

    directions <- c(315 ,0, 45, 270, 90, 225, 180,135 )

    ###################################################################

    # Go Nortwest

    north.west.from <- as.vector(M[-1,-1])
    north.west.to <- as.vector(M[-nr,-nc])
    north.west.cost <- fun(DL[-1,-1], SL[-1,-1], directions[1], ...)

    ###################################################################

    # Go North

    north.from <- as.vector(M[-1,])
    north.to <- as.vector(M[-nr,])
    north.cost <- as.vector( fun(DL[-1,], SL[-1,], directions[2], ...) )

    ###################################################################

    # Go Norteast

    north.east.from <- as.vector(M[-1,-nc])
    north.east.to <- as.vector(M[-nr,-1])
    north.east.cost <- as.vector( fun(DL[-1,-nc], SL[-1,-nc], directions[3], ...) )

    ###################################################################

    # Go West

    west.from <- as.vector(M[,-1])
    west.to <- as.vector(M[,-nc])
    west.cost <- as.vector( fun(DL[,-1], SL[,-1], directions[4], ...) )

    ###################################################################

    # Go East

    east.from <- as.vector(M[,-nc])
    east.to <- as.vector(M[,-1])
    east.cost <- as.vector( fun(DL[,-nc], SL[,-nc], directions[5], ...) )

    ###################################################################

    # Go Southwest

    south.west.from <- as.vector(M[-nr,-1])
    south.west.to <- as.vector(M[-1,-nc])
    south.west.cost <- as.vector( fun(DL[-nr,-1], SL[-nr,-1], directions[6], ...) )

    ###################################################################

    # Go South

    south.from <- as.vector(M[-nr,])
    south.to <- as.vector(M[-1,])
    south.cost <- as.vector( fun(DL[-nr,], SL[-nr,], directions[7], ...) )

    ###################################################################

    # Go Southeast

    south.east.from <- as.vector(M[-nr,-nc])
    south.east.to <- as.vector(M[-1,-1])
    south.east.cost <- as.vector( fun(DL[-nr,-nc], SL[-nr,-nc], directions[8], ...)  )

    ###################################################################

    ii <- c(north.west.from, north.from, north.east.from, west.from, east.from, south.west.from, south.from, south.east.from)
    jj <- c(north.west.to, north.to, north.east.to, west.to, east.to, south.west.to, south.to, south.east.to)
    xx <- c(north.west.cost, north.cost, north.east.cost, west.cost, east.cost, south.west.cost, south.cost, south.east.cost)

    tl <- sparseMatrix(i=ii, j=jj, x=xx)
    if(output == "raw") return(tl)
    if(output == "transitionLayer") {
        tmp <- transition(stack$wind.direction, transitionFunction=function(x) 0, directions=8)
        transitionMatrix(tmp)<-sparseMatrix(i=ii, j=jj, x= 1 / xx)
        return(tmp)
    }
    return(NULL)
}



#' Compute flow-based cost or conductance
#'
#' \code{flow.dispersion} computes movement conductance through a flow either, sea
#' or wind currents. It implements the formula described in Felícisimo et al.
#' 2008:
#'
#' Cost=(1/Speed)*(HorizontalFactor)
#'
#' being HorizontalFactor a "function that incrementaly penalized angular
#' deviations from the wind direction" (Felicísimo et al. 2008).
#'
#'
#' @param x RasterStack object with layers obtained from wind2raster
#' function ("rWind" package) with direction and speed flow values.
#' @param fun A function to compute the cost to move between cells. The default
#' is \code{cost.FMGS} from Felicísimo et al. (2008), see details.
#' @param output This argument allows to select diferent kinds of output. "raw"
#' mode creates a matrix (class "dgCMatrix") with transition costs between all
#' cells in the raster. "transitionLayer" creates a TransitionLayer object with
#' conductance values to be used with "gdistance" package.
#' @param ... Further arguments passed to or from other methods.
#' @param wind.direction A vector or skalar containing wind directions.
#' @param wind.speed A vector or skalar containing wind speeds.
#' @param target direction of the target cell
#' @param type Could be either "passive" or "active".In "passive" mode,
#' movement against flow direction is not allowed (deviations from the wind
#' direction higher than 90). In "active" mode, the movement can go against flow
#' direction, by increasing the cost.
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
#' require(gdistance)
#'
#' data(wind.data)
#'
#' wind <- wind2raster(wind.data)
#'
#' Conductance<-flow.dispersion(wind, type="passive")
#'
#' transitionMatrix(Conductance)
#' image(transitionMatrix(Conductance))
#'
#' @importClassesFrom raster RasterLayer
#' @importFrom raster ncell
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#' @export flow.dispersion
flow.dispersion <- function(x, fun = cost.FMGS, output = "transitionLayer", ...) {
    if(inherits(x, "RasterStack")){
        return(flow.dispersion_int(x, fun=fun, output=output, ...))
    }
    lapply(x, flow.dispersion_int, fun=fun, output=output, ...)
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
#' mean_speed <- tidy(wind.series) %>% group_by(lat, lon) %>%
#'     summarise(speed=mean(speed))
#' wind_average2 <- wind.mean(wind.series)
#' all.equal(wind_average2$speed, mean_speed$speed)
#' }
#' @rdname tidy.rWind_series
#' @export tidy
tidy <- function (x, ...) UseMethod("tidy")

#' @rdname tidy.rWind_series
#' @export
tidy.rWind_series <- function(x, ...){
    l <- length(x)
    res <- x[[1]]
    if(l>1)for(i in 2:l)res <- rbind(res, x[[i]])
    res
}





#' Wind-data mean
#'
#' wind.mean computes the mean (average) wind speed and wind direction of a time
#' series dataset of winds of the same region.
#' Summaries of time series are not trivial to compute. We compute the
#' arithmetic mean for the wind speed.
#' The direction as the circular mean, see
#' \url{https://en.wikipedia.org/wiki/Mean_of_circular_quantities}
#' for more details. The U and V componenats are afterwards transformed from
#' these values.
#' @param x An object of class \code{rWind_series}
#' @return An object of class \code{rWind}, which is a \code{data.frame}
#' @note For large time series, it could take a while.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~kwd1 ~kwd2
#' @examples
#' data(wind.series)
#' wind_average<- wind.mean(wind.series)
#'
#'
#' @export wind.mean
wind.mean <- function(x){
    if(!inherits(x, "rWind_series")) stop("x needs to be of class rWind_series")

    tt <- as_datetime(names(x)[1])
    res <- cbind(tt, attr(x, "lat_lon"))

    x <- unclass(x)
    l <- length(x)
    tmpD <- tmpS <- matrix(0, nrow(x[[1]]), l)
    for(i in seq_len(l)){
        tmp <- uv2ds(x[[i]][,1], x[[i]][,2])
        tmpD[,i] <- tmp[,1]
        tmpS[,i] <- tmp[,2]
    }
    smean <- apply(tmpS, 1, mean)
    dmean <- apply(tmpD, 1, circ.mean)
    res <- cbind(res, ds2uv(dmean, smean), dmean, smean)

    colnames(res) <- c("time", "lat", "lon", "ugrd10m", "vgrd10m", "dir",
                       "speed")
    class(res) <-  c("rWind", "data.frame")
    return(res)
}


###############################################################################
# Some new and experimental functions to download OSCAR Sea Surface Velocity data
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplOscar_LonPM180.html
# https://coastwatch.pfeg.noaa.gov/erddap/info/jplOscar_LonPM180/index.html
# This is a beta version, please use it carefully

oscar.fit_int <- function (tmpx) {
  tmpx <- cbind(tmpx[,1],tmpx[,3:6])
  ###### DIRECTION
  direction <- atan2(tmpx[,4], tmpx[,5])
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED
  speed <- sqrt( (tmpx[,4] * tmpx[,4]) + (tmpx[,5] * tmpx[,5]))

  ######
  names(tmpx)<- c("time", "lat","lon", "u", "v")
  res <- cbind(tmpx, dir=direction, speed=speed)
  res <- res[with(res, order(-lat)), ]
  res[,1] <- ymd_hms(res[,1], truncated = 3)
  return(res)
}

#' OSCAR Sea currents data download
#'
#' seaOscar.dl downloads wind data from the Ocean Surface Current Analyses Real-time (OSCAR)
#' (https://coastwatch.pfeg.noaa.gov/erddap/info/jplOscar_LonPM180/index.html).
#' Geospatial resolution is 0.33 degrees and sea currents are calculated for
#' 15 m depth.
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
#' directory named "sea_yyyy_mm_dd.csv".
#' @param trace if trace = 1 (by default) track downloaded files
#' @return "rWind" and "data.frame" class object or .csv file with U and V
#' vector  components and sea current direction and speed for each coordenate
#' in the study area defined by lon1/lon2 and lat1/lat2.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl_2}}, \code{\link{wind2raster}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' https://coastwatch.pfeg.noaa.gov/erddap/info/jplOscar_LonPM180/index.html
#' @keywords ~currents ~sea
#' @examples
#'
#' # Download sea currents for Galapagos Islands
#' \dontrun{
#'
#' seaOscar.dl(2015,1,1,-93,-88,2,-3)
#'
#' }

#' @importFrom utils write.table read.csv download.file
#' @importFrom lubridate ymd year month day hour
#' @rdname seaOscar.dl
#' @export seaOscar.dl

seaOscar.dl <- function(yyyy, mm, dd, lon1, lon2, lat1, lat2, type = "read-data", trace = 1) {
  type <- match.arg(type, c("read-data", "csv"))
  mm <- sprintf("%02d", mm)
  dd <- sprintf("%02d", dd)
  dt <- ymd(paste(yyyy, mm, dd, sep = "-"))
  yyyy_c <- year(dt)
  mm_c <- sprintf("%02d", month(dt))
  dd_c <- sprintf("%02d", day(dt))
  #tt_c <- sprintf("%02d", hour(dt))
  testDate <- paste(yyyy_c, "-", mm_c, "-", dd_c, sep = "")
  print(testDate)
  if (trace)
    print(paste(ymd(paste(yyyy_c, mm_c, dd_c, sep = "-")),
                "downloading...", sep = " "))
  tryCatch({
    as.Date(testDate)
    url_dir <- paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplOscar_LonPM180.csv?u[(",
                     yyyy_c,"-",mm_c,"-",dd_c,"T00:00:00Z):1:(",yyyy_c,"-",mm_c,"-",dd_c,
                     "T00:00:00Z)][(15.0):1:(15.0)][(",lat1,"):1:(", lat2,
                     ")][(",lon1,"):1:(",lon2, ")],v[(",yyyy_c,"-",mm_c,"-",dd_c,
                     "T00:00:00Z):1:(",yyyy_c,"-",mm_c,"-",dd_c,"T00:00:00Z)][(15.0):1:(15.0)][(",
                     lat1,"):1:(",lat2,")][(", lon1, "):1:(", lon2, ")],um[(",
                     yyyy_c,"-",mm_c,"-", dd_c, "T00:00:00Z):1:(", yyyy_c, "-", mm_c,
                     "-", dd_c, "T00:00:00Z)][(15.0):1:(15.0)][(", lat1, "):1:(",
                     lat2,")][(", lon1, "):1:(", lon2, ")],vm[(", yyyy_c, "-", mm_c,
                     "-", dd_c,"T00:00:00Z):1:(", yyyy_c, "-", mm_c, "-", dd_c,
                     "T00:00:00Z)][(15.0):1:(15.0)][(", lat1, "):1:(", lat2,
                     ")][(" , lon1, "):1:(", lon2,")]", sep="")

    tmp <- read.csv(url_dir, header = FALSE, skip = 2,
                    stringsAsFactors = FALSE)
    tmp <- oscar.fit_int(tmp)
    if (type == "csv") {
      fname <- paste("oscar_", yyyy_c, "_", mm_c, "_",
                     dd_c, "_", ".csv", sep = "")
      write.table(tmp, fname, sep = ",", row.names = FALSE,
                  col.names = TRUE, quote = FALSE)
    }
  }, error = function(e) {
    cat("ERROR: database not found. Please, check server\n                      connection, date or geographical ranges \n")
  }, warning = function(w) {
    cat("ERROR: database not found. Please, check server\n                        connection, date or geographical ranges  \n")
  })
  class(tmp) <- c("rWind", "data.frame")
  return(tmp)
}
