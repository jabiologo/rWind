\name{wind_data}
\alias{wind_data}
\docType{data}
\title{
Wind data example
}
\description{
This is an example of wind data obtained with wind.dl function for the Iberian Peninsula coordetanes on 12/February/2015 at 00:00 (UTC)}
\usage{data("wind_data")}
\format{
  A data.frame with 652 observations on the following 5 variables:
  \describe{
    \item{\code{time (UTC)}}{a factor with selected time of wind data }
    \item{\code{latitude (degrees_north)}}{a factor with latitude values }
    \item{\code{longitude (degrees_east)}}{a factor with longitude values }
    \item{\code{ugrd10m (m s-1)}}{a factor with U component of wind data }
    \item{\code{vgrd10m (m s-1)}}{a factor with V component of wind data }
  }
}
\details{

 This data set is the result of:

 \code{wind_data<-wind.dl(2015,2,12,0,-10,5,35,45)}

}
\source{

http://allthiswasfield.blogspot.com.es/2016/12/rwind-r-package-released.html

}
\references{
http://oos.soest.hawaii.edu/erddap/info/NCEP_Global_Best/index.html
}
\examples{
data(wind_data)
str(wind_data)
head(wind_data)
}
\keyword{datasets, wind, download}
