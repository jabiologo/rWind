

#' Download, edit and ese wind data from GFS
#' 
#' rWind contain tools for downloading, editing and transforming wind data from
#' Global Forecast System (GFS). It also allows to use wind data to compute the
#' minimum cost path taking into account wind speed and direction to perform
#' connectivity analysis.
#' 
#' The complete list of functions can be displayed with \code{library(help =
#' rWind)}. For more information, please check:
#' http://allthiswasfield.blogspot.com.es/
#' 
#' @name rWind-package
#' @aliases rWind-package rWind
#' @docType package
#' @author Javier Fernández-López
#' 
#' Klaus Schliep
#' 
#' Maintainer: Javier Fernández-López <jflopez@@rjb.csic.es>
#' @keywords package
NULL





#' Wind data example
#' 
#' This is an example of wind data obtained with wind.dl function for the
#' Iberian Peninsula coordetanes on 12/February/2015 at 00:00 (UTC)
#' 
#' 
#' This data set is the result of:
#' 
#' \code{wind_data<-wind.dl(2015,2,12,0,-10,5,35,45)}
#' 
#' @name wind_data
#' @docType data
#' @format A data.frame with 652 observations on the following 5 variables:
#' \describe{ \item{list("time (UTC)")}{a factor with selected time of wind
#' data } \item{list("latitude (degrees_north)")}{a factor with latitude values
#' } \item{list("longitude (degrees_east)")}{a factor with longitude values }
#' \item{list("ugrd10m (m s-1)")}{a factor with U component of wind data }
#' \item{list("vgrd10m (m s-1)")}{a factor with V component of wind data } }
#' @references
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#' @source
#' 
#' http://allthiswasfield.blogspot.com.es/2016/12/rwind-r-package-released.html
#' @keywords datasets, wind, download
#' @examples
#' 
#' data(wind_data)
#' str(wind_data)
#' head(wind_data)
#' 
NULL





#' Wind series example
#' 
#' This is an example of a wind series data obtained with a loop and wind.dl
#' function for New Zealand area on 3/January/2015 at all the available times:
#' 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00 (UTC)
#' 
#' You can find the code used to build this data-set in the help file of
#' wind.mean function
#' 
#' @name wind_series
#' @docType data
#' @format The format is a list of 8 data.frame. Each data.frame contain 652
#' observations on the following 5 variables:
#' 
#' \describe{ \item{list("time (UTC)")}{a factor with selected time of wind
#' data } \item{list("latitude (degrees_north)")}{a factor with latitude values
#' } \item{list("longitude (degrees_east)")}{a factor with longitude values }
#' \item{list("ugrd10m (m s-1)")}{a factor with U component of wind data }
#' \item{list("vgrd10m (m s-1)")}{a factor with V component of wind data } }
#' @references
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#' @source
#' http://allthiswasfield.blogspot.com.es/2016/12/rwind-r-package-released.html
#' @keywords datasets
#' @examples
#' 
#' data(wind_series)
#' str(wind_series)
#' wind_average<- wind.mean(wind_series)
#' 
NULL



