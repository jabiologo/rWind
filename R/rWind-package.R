

#' Download, edit and include wind data in ecological and evolutionary analysis
#'
#' rWind contain tools for downloading, editing and transforming wind data from
#' Global Forecast System (GFS). It also allows to use wind data to compute the
#' minimum cost path from wind speed and direction to perform
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
#' Maintainer: Javier Fernández-López <jflopez.bio@@gmail.com>
#' @keywords package
NULL

#' Wind data example
#'
#' This is an example of wind data obtained with wind.dl function for the
#' Iberian Peninsula coordenates on 12/February/2015 at 00:00 (UTC)
#'
#'
#' This data set is the result of:
#'
#' \code{wind.data <- wind.dl(2015,2,12,0,2015,2,12,0,-10,5,35,45)}
#'
#' @name wind.data
#' @docType data
#' @format A list with one data.frame with 651 observations on the following 7
#' variables:
#' \describe{ \item{list("time (UTC)")}{a numeric with selected time of wind
#' data } \item{list("latitude (degrees_north)")}{a numeric with latitude values
#' } \item{list("longitude (degrees_east)")}{a numeric with longitude values }
#' \item{list("ugrd10m (m s-1)")}{a numeric with U component of wind data }
#' \item{list("vgrd10m (m s-1)")}{a numeric with V component of wind data }
#' \item{list("dir")}{a numeric with direction of wind data }
#' \item{list("speed")}{a numeric with speed of wind data } }
#' @references
#' http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
#' @source
#'
#' http://allthiswasfield.blogspot.com.es/2016/12/rwind-r-package-released.html
#' @keywords datasets, wind, download
#' @examples
#'
#' data(wind.data)
#' str(wind.data)
#' head(wind.data[[1]])
#'
NULL


