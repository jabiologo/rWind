wind.dl<- function (yyyy,mm,dd,tt,lon1,lon2,lat1,lat2,type="read-data"){
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
    c<-rbind(read.csv(url_west),read.csv(url_east)[2:nrow(read.csv(url_east)),])
    if (type == "csv"){
      write.csv(c,paste("wind_",yyyy,"_",mm,"_",dd,"_",tt,".csv", sep=""),row.names = FALSE)
    }
    else{
      return(c)
    }
  }
  else {
    url_dir<- paste("http://oos.soest.hawaii.edu/erddap/griddap/NCEP_Global_Best.csv?ugrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")],vgrd10m[(",yyyy,"-",mm,"-",dd,"T",tt,":00:00Z)][(",lat1,"):(",lat2,")][(",lon1,"):(",lon2,")]&.draw=vectors&.vars=longitude|latitude|ugrd10m|vgrd10m&.color=0x000000",sep="")
    if (type == "csv"){
      download.file(url_dir, paste("wind_",yyyy,"_",mm,"_",dd,"_",tt,".csv", sep=""))
    }
    else{
      return(read.csv(url_dir))
    }
  }
}

wind.fit<-function(X){
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  bruto<-X

  bruto<-data.frame(bruto[2:nrow(bruto),])
  bruto<-bruto[,2:ncol(bruto)]
  indx <- sapply(bruto, is.factor)
  bruto[indx] <- lapply(bruto[indx], function(x) as.numeric(as.character(x)))

  big_bruto<-data.frame(1:nrow(bruto))

  ###### LONGITUDE
  for (q in 1:nrow(bruto)){
    if (bruto[q,2]%%360 < 180) {bruto[q,2]<- bruto[q,2]%%360 }
    else {bruto[q,2]<- (((bruto[q,2] - 180) %%360)-180)}
  }
  names(bruto)<- c("lat","lon", "ugrd10m", "vgrd10m")

  big_bruto<-cbind(big_bruto, bruto)

  ###### DIRECTION
  big_bruto_dir<-data.frame(1:nrow(bruto))
  names(big_bruto_dir)<-"V1"
  big_bruto_speed<-data.frame(1:nrow(bruto))
  names(big_bruto_speed)<-"V1"

  for (g in 1:1){
    u<-"ugrd10m"
    v<-"vgrd10m"

    for (t in 1:nrow(big_bruto)){
      nugget<-atan2(big_bruto[t,u],big_bruto[t,v])
      nugget<-rad2deg(nugget)
      if (nugget < 0) {nugget<- 360 + nugget }
      big_bruto_dir[t,g]<-nugget
    }
  }
  ###### SPEED
  for (q in 1:1){
    u<-"ugrd10m"
    v<-"vgrd10m"
    for (w in 1:nrow(big_bruto)){
      mcpollo<-sqrt((big_bruto[w,u]*big_bruto[w,u]) + (big_bruto[w,v]*big_bruto[w,v]
      ))
      big_bruto_speed[w,q]<-mcpollo
    }
  }

  big_bruto<-cbind(big_bruto[,2:3],big_bruto_dir,big_bruto_speed)
  big_bruto<-big_bruto[with(big_bruto, order(-lat)), ]
  names(big_bruto)<-c("lat","lon","dir","speed")

  return(big_bruto)
}

wind2raster<- function(W, type="dir"){
  pts_d<-data.frame(cbind(W$lon,W$lat))
  ras=rasterFromXYZ(pts_d,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
  if (type == "dir"){
  ras[] <- W$dir}
  else {
  ras[] <- W$speed
  }
  return(ras)
}

wind.mean<-function(wind_serie){

  wind_mean<<-cbind(data.frame(wind_serie[[1]][1:nrow(wind_serie[[1]]),1]),data.frame(wind_serie[[1]][1:nrow(wind_serie[[1]]),2]),data.frame(wind_serie[[1]][1:nrow(wind_serie[[1]]),3]))
  umean=data.frame(1:(nrow(wind_serie[[1]])-1))
  for (n in 2:nrow(wind_serie[[1]])){
    row_mean_list=vector()
    for (h in 1:length(wind_serie)){
      u=as.numeric(as.character(wind_serie[[h]][n,4]))
      row_mean_list[h]<-u
      row_mean=mean(row_mean_list)
    }
    umean[n,1]<-row_mean
  }
  vmean=data.frame(1:(nrow(wind_serie[[1]])-1))
  for (n in 2:nrow(wind_serie[[1]])){
    row_mean_list=vector()
    for (h in 1:length(wind_serie)){
      v=as.numeric(as.character(wind_serie[[h]][n,5]))
      row_mean_list[h]<-v
      row_mean=mean(row_mean_list)
    }
    vmean[n,1]<-row_mean
  }
  wind_mean<-cbind(wind_mean,umean,vmean)
  names(wind_mean)<-c("time","latitude","longitude","ugrd10m","vgrd10m")
  return(wind_mean)
}

arrowDir <- function(W){
  aDir<-(360-W$dir) + 90
  return(aDir)
}

flow.dispersion<-function(dl,sl,type){

  ###################################################################
  # Cost computation following Muñoz et al., 2004; Felicísimo et al., 2008

  cost.Felicisimo<- function(wind,celda,type="passive"){
    dif=(abs(wind-celda))
    if (dif > 180){
      dif = 360 - dif
    }
    if (type=="passive"){
      if (dif > 90){return(0)}
      else{

        if (dif==0){return(0.1)}
        else {return(dif*2)}
      }
    }
    else {
      if (dif==0){return(0.1)}
      else {return(dif*2)}
    }
  }

  ###################################################################

  directions <- c(315 ,0, 45, 270, 90, 225, 180,135 )

  ###################################################################

  tl <- transition(dl, transitionFunction=function(x) 0, directions=8)

  ###################################################################

  matrix_dir<-list()

  north.west <- matrix(c( 1, NA, NA,
                          NA, 0,  NA,
                          NA, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[1]]<-north.west

  north <- matrix(c(NA, 1,  NA,
                    NA, 0,  NA,
                    NA, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[2]]<-north

  north.east <- matrix(c(NA, NA,  1,
                         NA, 0,  NA,
                         NA, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[3]]<-north.east

  east <-  matrix(c(NA, NA, NA,
                    NA, 0,   1,
                    NA, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[4]]<-east

  south.east <- matrix(c(NA, NA, NA,
                         NA, 0,  NA,
                         NA, NA,  1), ncol=3, byrow=TRUE)
  matrix_dir[[5]]<-south.east

  south <- matrix(c(NA, NA, NA,
                    NA, 0,  NA,
                    NA, 1,  NA), ncol=3, byrow=TRUE)
  matrix_dir[[6]]<-south

  south.west <- matrix(c(NA, NA, NA,
                         NA, 0,  NA,
                         1, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[7]]<-south.west

  west <-  matrix(c(NA, NA, NA,
                    1, 0,  NA,
                    NA, NA, NA), ncol=3, byrow=TRUE)
  matrix_dir[[8]]<-west

  ###################################################################

  adj_north<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[2]], sorted=TRUE)
    adj_north[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[2]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_north[[trow]][1])==FALSE){
      tl[adj_north[[trow]][1],adj_north[[trow]][2]]<-cost.Felicisimo(dl[adj_north[[trow]][1]],directions[2], type) / sl[adj_north[[trow]][1]]
    }
  }

  ###################################################################

  adj_south<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[6]], sorted=TRUE)
    adj_south[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[6]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_south[[trow]][1])==FALSE){
      tl[adj_south[[trow]][1],adj_south[[trow]][2]]<-cost.Felicisimo(dl[adj_south[[trow]][1]],directions[7], type) / sl[adj_south[[trow]][1]]
    }
  }

  ###################################################################

  adj_east<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[4]], sorted=TRUE)
    adj_east[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[4]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_east[[trow]][1])==FALSE){
      tl[adj_east[[trow]][1],adj_east[[trow]][2]]<-cost.Felicisimo(dl[adj_east[[trow]][1]],directions[5], type) / sl[adj_east[[trow]][1]]
    }
  }

  ###################################################################

  adj_west<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[8]], sorted=TRUE)
    adj_west[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[8]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_west[[trow]][1])==FALSE){
      tl[adj_west[[trow]][1],adj_west[[trow]][2]]<-cost.Felicisimo(dl[adj_west[[trow]][1]],directions[4], type) / sl[adj_west[[trow]][1]]
    }
  }

  ###################################################################

  adj_north.east<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[3]], sorted=TRUE)
    adj_north.east[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[3]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_north.east[[trow]][1])==FALSE){
      tl[adj_north.east[[trow]][1],adj_north.east[[trow]][2]]<-cost.Felicisimo(dl[adj_north.east[[trow]][1]],directions[3], type) / sl[adj_north.east[[trow]][1]]
    }
  }

  ###################################################################

  adj_north.west<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[1]], sorted=TRUE)
    adj_north.west[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[1]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_north.west[[trow]][1])==FALSE){
      tl[adj_north.west[[trow]][1],adj_north.west[[trow]][2]]<-cost.Felicisimo(dl[adj_north.west[[trow]][1]],directions[1], type) / sl[adj_north.west[[trow]][1]]
    }
  }

  ###################################################################

  adj_south.west<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[7]], sorted=TRUE)
    adj_south.west[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[7]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_south.west[[trow]][1])==FALSE){
      tl[adj_south.west[[trow]][1],adj_south.west[[trow]][2]]<-cost.Felicisimo(dl[adj_south.west[[trow]][1]],directions[6], type) / sl[adj_south.west[[trow]][1]]
    }
  }

  ###################################################################

  adj_south.east<-list()

  for (c in 1:ncell(dl)){
    aa<-adjacent(dl, cells = 1, directions=matrix_dir[[5]], sorted=TRUE)
    adj_south.east[[c]] <- adjacent(dl, cells = c, directions=matrix_dir[[5]], sorted=TRUE)
  }

  for (trow in 1:ncell(dl)){
    if (is.na(adj_south.east[[trow]][1])==FALSE){
      tl[adj_south.east[[trow]][1],adj_south.east[[trow]][2]]<-cost.Felicisimo(dl[adj_south.east[[trow]][1]],directions[8], type) / sl[adj_south.east[[trow]][1]]
    }
  }

  ###################################################################

  transitionMatrix(tl)<-replace(transitionMatrix(tl), is.na(transitionMatrix(tl)), 0)
  transitionMatrix(tl)<-Matrix(1/transitionMatrix(tl), sparse=TRUE)
  transitionMatrix(tl)<-replace(transitionMatrix(tl), is.infinite(transitionMatrix(tl)), 0)

  return(tl)
}
