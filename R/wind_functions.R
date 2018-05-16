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
#' @param yyyy Selected year from 2011 to current.
#' @param mm Selected month
#' @param dd Selected day
#' @param tt Selected time. There are currently several options at the GFS
#' database: 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00
#' (UTC)
#' @param lon1 Western longitude
#' @param lon2 Eastern longitude
#' @param lat1 Southern latitude
#' @param lat2 Northern latitude
#' @param type Output type. "read-data" is selected by default, creating an R
#' object. If you choose "csv", wind.dl create a a CSV file in your work
#' directory named "wind_yyyy_mm_dd_tt.csv".
#' @return "rWind data.frame" class object or .csv file with U and V vector
#' components for each coordenate in the study area defined by lon1/lon2 and
#' lat1/lat2 and a column with time and date.
#' @note U and V vector components allow you to create wind averages or
#' tendences for each coordenate at the study area. Longitude coordenates are
#' provided by GFS dataset in 0/360 notation. Note that if you want to obtain
#' wind direction and speed from U and V vectors and a -180/180 longitude
#' notation, you should use wind.fit function from "rWind" package.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.fit}}, \code{\link{wind.mean}}
#' @references
#' http://www.digital-geography.com/cloud-gis-getting-weather-data/#.WDOWmbV1DCL
#'
#' http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.graph
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' \dontrun{
#' wind.dl(2015,2,12,0,-10,5,35,45)
#' }
#'
#' @importFrom utils write.table read.csv download.file
#' @rdname wind.dl
#' @export wind.dl
wind.dl<- function (yyyy,mm,dd,tt,lon1,lon2,lat1,lat2,type="read-data"){
  type <- match.arg(type, c("read-data", "csv"))
  mm<-sprintf("%02d", mm)
  dd<-sprintf("%02d", dd)
  tt<-sprintf("%02d", tt)

  testDate <- paste(yyyy,"-",mm,"-",dd, sep="")

  tryCatch({
    as.Date(testDate)
    if (lon1 < 0){
      lon1<-360-(abs(lon1))
    }
    if (lon2 < 0){
      lon2<-360-(abs(lon2))
    }

    if (lon1 > 180 && lon2 <180){
      url_west<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)],vgrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(359.5)]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
      url_east<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")],vgrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(0.0):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
      header <- readLines(url_west, n=2)
      tmp<-rbind(read.csv(url_west, header=FALSE, skip=2),read.csv(url_east, header=FALSE, skip=2))
      if (type == "csv"){
        fname <- paste("wind_",yyyy,"_",mm,"_",dd,"_",tt,".csv", sep="")
        writeLines(header, fname)
        write.table(tmp, fname, append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
      else{
        blub <- strsplit(header, ",")
        header <- paste(blub[[1]], paste0("(", blub[[2]], ")"))
        colnames(tmp) <- header
        class(tmp) <-  c("rWind", "data.frame")
        return(tmp)
      }
    }
    else {
      url_dir<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")],vgrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
      if (type == "csv"){
        download.file(url_dir, paste("wind_",yyyy,"_",mm,"_",dd,"_",tt,".csv", sep=""))
      }
      else{
        header <- readLines(url_dir, n=2)
        blub <- strsplit(header, ",")
        header <- paste(blub[[1]], paste0("(", blub[[2]], ")"))
        tmp<-read.csv(url_dir, header=FALSE, skip=2)
        colnames(tmp) <- header
        class(tmp) <-  c("rWind", "data.frame")
        return(tmp)
      }
    }
  },
  error=function(e){cat("ERROR: database not found. Please, check server connection, date or geographical ranges \n")},
  warning=function(w){cat("ERROR: database not found. Please, check server connection, date or geographical ranges  \n")}
  )

}




#' Wind-data fit
#'
#' wind.fit is used to transform a data.frame obtained from wind.dl. wind.fit
#' applies trigonometry tools to transform U and V vector wind components in
#' wind direction and speed features. It also transforms 0-360 longitude
#' notation obtained from GFS data into -180/180 longitude notation to be used
#' in GIS (Geographic Information Systems) or for R plots. Moreover, it cleans
#' column names obtained from wind.dl, deletes "time" column and sorts the data
#' by latitude.
#'
#'
#' @param X data.frame generated by wind.dl function from "rWind" package.
#' @return "rWind data.frame" class object prepared to be plotted by R or to be
#' exported as a CSV file to be used with a GIS software.
#' @note If you want to calculate wind average from multiple wind datasets
#' using wind.mean function, you should use U and V vector components rather
#' than speed and direction values. Please, perform the calculations through
#' wind.mean directly from the result of wind.dl function before of running
#' wind.fit
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.mean}},
#' \code{\link{wind2raster}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~wind ~gfs
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' data(wind_data)
#'
#' # Fit downloaded dataset to be plotted
#' wind_fitted_data <- wind.fit(wind_data)
#'
#' # Export as CSV format in your work directory
#' # write.csv(wind_fitted_data, "wind_data_2015_11_2_Iberian_Peninsula.csv")
#'
#' @rdname wind.fit
#' @export wind.fit
wind.fit <- function(X){
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  X <- X[,2:ncol(X)]
  l <- nrow(X)

  ###### LONGITUDE

  X[,2] <- X[,2] %% 360
  X[X[,2]>=180,2] <- X[X[,2]>=180,2] - 360
  names(X)<- c("lat","lon", "ugrd10m", "vgrd10m")

  ###### DIRECTION

  direction <- atan2(X[,"ugrd10m"], X[,"vgrd10m"])
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED

  speed <- sqrt( (X[,"ugrd10m"] * X[,"ugrd10m"]) + (X[,"vgrd10m"] * X[,"vgrd10m"]))

  res <- cbind(X[,c("lat","lon")], dir=direction, speed=speed)
  res <- res[with(res, order(-lat)), ]
  class(res) <-  c("rWind", "data.frame")
  return(res)
}



#' Wind-data to raster file
#'
#' wind2raster crates a raster file (gridded) from a data.frame created by
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
#' @param type Feature represented by raster file. "dir" (wind direction) is
#' selected by default. If you choose "speed", wind speed will be represented
#' by raster file. If "stack" is selected, a raster stack with both layers is
#' returned
#' @return A raster file representing wind direction, wind speed or both of the
#' study area.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.fit}}
#' @keywords ~gfs ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' data(wind_data)
#'
#' # Fit downloaded dataset to be plotted
#' wind_fitted_data <- wind.fit(wind_data)
#'
#' # Create raster file from the data.frame created by wind.fit, representing wind speed.
#' wind2raster(wind_fitted_data, type="speed")
#'
#' @importFrom raster rasterFromXYZ stack
#'
#' @rdname wind2raster
#' @export wind2raster
wind2raster<- function(x, type="stack"){
#  pts_d<-data.frame(cbind(W$lon,W$lat))
  ras <- rasterFromXYZ(x[,c("lon","lat")], crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  if (type == "dir"){
  ras[] <- x$dir
  return(ras)}
  if (type == "speed"){
    ras[] <- x$speed
    return(ras)}
  if (type == "stack") {
    ras2 <- ras
    ras[] <- x$dir
    ras2[] <- x$speed
    tmp <- stack (ras, ras2)
    names(tmp)<- c("wind.direction", "wind.speed")
    return(tmp)
  }
}



#' Wind-data mean
#'
#' wind.mean computes the mean (average) of a time series dataset of winds in
#' the same region. To do this, wind.mean uses U and V vector components of
#' several wind data.frames stored in a list. Note that, if you want to perform
#' wind direction and speed average, first you should calculate the mean of U
#' and V components and then transform it to direction and speed using wind.fit
#' function from "rWind" package.
#'
#'
#' @param wind_series A list of data.frames downloaded by wind.dl function.
#' @return A data.frame with a similar format as resulted by wind.dl, prepared
#' to be transformed by wind.fit.
#' @note For large time series, it could take a while.
#' @author Javier Fernández-López (jflopez@@rjb.csic.es)
#' @seealso \code{\link{wind.dl}}, \code{\link{wind.fit}}
#' @references https://en.wikipedia.org/wiki/Cross_product
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' # First, you should create an empty list to store all the data
#'
#' # wind_series<- list()
#'
#' # Then, you can use a wind.dl inside a for-in loop to download and store wind data of
#' # the January 3rd 2015 at several hours around New Zealand.
#'
#' # t<-c(00 , 03 , 06 , 09 , 12 , 15 , 18 , 21)
#' # for (tt in 1:8){
#' #  w<-wind.dl(2015,1,3,t[tt],164,179,-48,-33)
#' #  wind_series[[tt]]<-w
#' # }
#'
#' data(wind_series)
#'
#' # Finally, you can implement wind.mean and wind.fit to compute the average of all winds
#' # datasets in the list:
#'
#' wind_average<- wind.mean(wind_series)
#'
#' # wind.fit(wind_average)
#'
#'
#' @export wind.mean
wind.mean <- function(wind_series){
    wind_mean <- cbind(data.frame(wind_series[[1]][,1]), data.frame(wind_series[[1]][,2]), data.frame(wind_series[[1]][,3]))
    l <- length(wind_series)
    row_mean_matrix <- matrix(NA, nrow(wind_series[[1]]), l)
    for (h in 1:l) row_mean_matrix[,h] <- wind_series[[h]][,4]
    umean <- apply(row_mean_matrix, 1, mean)
    row_mean_matrix[] <- NA
    for (h in 1:l) row_mean_matrix[,h] <- wind_series[[h]][,5]
    vmean <- apply(row_mean_matrix, 1, mean)
    wind_mean<-cbind(wind_mean,umean,vmean)
    names(wind_mean)<-c("time","latitude","longitude","ugrd10m","vgrd10m")
    return(wind_mean)
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
#' @seealso \code{\link{wind.fit}}
#' @references Karline Soetaert (2017). shape: Functions for Plotting Graphical
#' Shapes, Colors. R package version 1.4.3.
#' https://CRAN.R-project.org/package=shape
#' @keywords ~wind
#' @examples
#'
#' # Download wind for Iberian Peninsula region at 2015, February 12, 00:00
#' # wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)
#'
#' data(wind_data)
#'
#' # Fit downloaded dataset to be plotted
#' wind_fitted_data <- wind.fit(wind_data)
#'
#' # Create a vector with wind direction (angles) adapted
#' alpha=arrowDir(wind_fitted_data)
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
#' flow.dispersion computes movement conductance through a flow either, sea or
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
#' @seealso \code{\link{wind.fit}}, \code{\link{wind2raster}}
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
#'  require(gdistance)
#'
#'  w<-wind.dl(2015,2,12,0,-10,5,35,45)
#'
#'  data(wind_data)
#'  w<-wind.fit(wind_data)
#'
#'  wind <- wind2raster(w, type="stack")
#'
#'  Conductance<-flow.dispersion(wind,"passive", "transitionLayer")
#'
#' # transitionMatrix(Conductance)
#' # image(transitionMatrix(Conductance))
#'
#' @importClassesFrom raster RasterLayer
#' @importFrom raster ncell
#' @importMethodsFrom raster as.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom gdistance transition transitionMatrix<-
#'
#' @export flow.dispersion
flow.dispersion <-function(stack, type="passive", output="raw"){

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

