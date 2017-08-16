\name{wind_serie}
\alias{wind_serie}
\docType{data}
\title{
Wind serie example
}
\description{
This is an example of a wind serie data obtained with a loop and wind.dl function for New Zealand area on 3/January/2015 at all the available times: 00:00 - 03:00 - 06:00 - 09:00 - 12:00 - 15:00 - 18:00 - 21:00 (UTC)
}
\usage{data("wind_serie")}
\format{
  The format is a list of 8 data.frame. Each data.frame contain 652 observations on the following 5 variables:

   \describe{
    \item{\code{time (UTC)}}{a factor with selected time of wind data }
    \item{\code{latitude (degrees_north)}}{a factor with latitude values }
    \item{\code{longitude (degrees_east)}}{a factor with longitude values }
    \item{\code{ugrd10m (m s-1)}}{a factor with U component of wind data }
    \item{\code{vgrd10m (m s-1)}}{a factor with V component of wind data }
  }
}
\details{
You can find the code used to build this data-set in the help file of wind.mean function
}
\source{
http://allthiswasfield.blogspot.com.es/2016/12/rwind-r-package-released.html
}
\references{
http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
}
\examples{
data(wind_serie)
str(wind_serie)
wind_average<- wind.mean(wind_serie)
}
\keyword{datasets}
