wind.dl<- function (yyyy,mm,dd,tt,lon1,lon2,lat1,lat2,type="read-data"){
  type <- match.arg(type, c("read-data", "csv"))
  mm<-sprintf("%02d", mm)
  dd<-sprintf("%02d", dd)
  tt<-sprintf("%02d", tt)

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
        return(tmp)
    }
  }
}

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
  return(res)
}

wind2raster<- function(W, type="dir"){
#  pts_d<-data.frame(cbind(W$lon,W$lat))
  ras <- rasterFromXYZ(W[,c("lon","lat")], crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  if (type == "dir"){
  ras[] <- W$dir}
  else {
  ras[] <- W$speed
  }
  return(ras)
}


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

arrowDir <- function(W){
  aDir<-(360-W$dir) + 90
  return(aDir)
}

flow.dispersion <-function(dl, sl, type="passive", output="raw"){

    output <- match.arg(output, c("raw", "transitionLayer"))

    DL <- as.matrix(dl)
    SL <- as.matrix(sl)
    M <- matrix(as.integer(1:ncell(dl)), nrow = nrow(dl), byrow=TRUE)
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
        tmp <- transition(dl, transitionFunction=function(x) 0, directions=8)
        transitionMatrix(tmp)<-sparseMatrix(i=ii, j=jj, x= 1 / xx)
#        transitionMatrix(tl)<-replace(transitionMatrix(tl), is.infinite(transitionMatrix(tl)), 0)
        return(tmp)
    }

    return(NULL)
}


sea.dl<- function (yyyy, mm, dd, lon1, lon2, lat1, lat2, type = "read-data", ...) {

  mm <- sprintf("%02d", mm)
  dd <- sprintf("%02d", dd)


  url_dir <- paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeo1day_LonPM180.csv?u_current[(",
                   yyyy,"-",mm,"-",dd,"T12:00:00Z)][(0.0)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,
                   ")],v_current[(",yyyy,"-",mm,"-",dd,"T12:00:00Z)][(0.0)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,
                   ")]&.draw=vectors&.vars=longitude%7Clatitude%7Cu_current%7Cv_current&.color=0x000000&.bgColor=0xffccccff",
                   sep = "")
  if (type == "csv") {
    download.file(url_dir, paste("wind_", yyyy, "_",
                                 mm, "_", dd, "_", tt, ".csv", sep = ""))
  }
  else {
    return(read.csv(url_dir, ...))
  }
}

sea.fit<- function(X){

  rad2deg <- function(rad) {
    (rad * 180)/(pi)
  }

  bruto <- X
  bruto <- data.frame(bruto[2:nrow(bruto), ])
  bruto <- bruto[, 3:ncol(bruto)]
  indx <- sapply(bruto, is.factor)
  bruto[indx] <- lapply(bruto[indx], function(x) as.numeric(as.character(x)))
  big_bruto <- data.frame(1:nrow(bruto))

  names(bruto) <- c("lat", "lon", "ugrd10m", "vgrd10m")
  big_bruto <- cbind(big_bruto, bruto)
  big_bruto_dir <- data.frame(1:nrow(bruto))
  names(big_bruto_dir) <- "V1"
  big_bruto_speed <- data.frame(1:nrow(bruto))
  names(big_bruto_speed) <- "V1"
  for (g in 1:1) {
    u <- "ugrd10m"
    v <- "vgrd10m"
    for (t in 1:nrow(big_bruto)) {
      nugget <- atan2(big_bruto[t, u], big_bruto[t, v])
      nugget <- rad2deg(nugget)
      if (is.nan(nugget)) { big_bruto_dir[t,g] <- NaN}
      else {if (nugget < 0) {
        nugget <- 360 + nugget
      }
      }
      big_bruto_dir[t, g] <- nugget
    }
  }
  for (q in 1:1) {
    u <- "ugrd10m"
    v <- "vgrd10m"
    for (w in 1:nrow(big_bruto)) {
      mcpollo <- sqrt((big_bruto[w, u] * big_bruto[w, u]) +
                        (big_bruto[w, v] * big_bruto[w, v]))
      big_bruto_speed[w, q] <- mcpollo
    }
  }
  big_bruto <- cbind(big_bruto[, 2:3], big_bruto_dir, big_bruto_speed)
  big_bruto <- big_bruto[with(big_bruto, order(-lat)), ]
  names(big_bruto) <- c("lat", "lon", "dir", "speed")
  big_bruto$lon<-round(big_bruto$lon,6)
  big_bruto<-big_bruto[big_bruto$lon != -0.025,]
  big_bruto$lon[big_bruto$lon < 0] <-(big_bruto$lon[big_bruto$lon < 0]) + 0.025

  return(big_bruto)

}

sea2raster<-function(SC, type="dir"){
  coordinates(SC) = ~lon+lat
  SC$speed[is.nan(SC$speed)==TRUE]<--9999
  SC$dir[is.nan(SC$dir)==TRUE]<--9999
  rast <- raster(ext=extent(SC), nrows=length(SC$lon[SC$lon == SC$lon[1]]) , ncols=length(SC$lat[SC$lat == SC$lat[1]]))

  if (type == "dir"){
    r<-rasterize(SC, rast, SC$dir) }
  else {
    r<-rasterize(SC, rast, SC$speed)
  }
  r[r[]==-9999]<-NA
  return(r)
}
