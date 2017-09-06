\name{wind2raster}
\alias{wind2raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wind-data to raster file
}
\description{
wind2raster crates a raster file (gridded) from a data.frame created by wind.fit function from rWind package. Latitude and logitude values are used to locate raster file and create raster resolution using rasterFromXYZ function from raster package. As raster files only can store one field of information, you should choose between direction (by default, type="dir") and speed (type="speed") to be represented by the new raster file.
}
\usage{
wind2raster(W, type="dir")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W}{
a data.frame obtained by wind.fit
}
\item{type}{
Feature represented by raster file. "dir" (wind direction) is selected by default. If you choose "speed", wind speed will be represented by raster file.
}
}
\details{
WGS84 datum (non-projected) CRS is selected by default to build the raster file.
}
\value{
A raster file representing wind direction or wind speed of the study area.
}

\author{
Javier Fernández-López (jflopez@rjb.csic.es)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wind.dl}}, \code{\link{wind.fit}}
}
\examples{
# Download wind for Iberian Peninsula region at 2015, February 12, 00:00
# wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)

data(wind_data)

# Fit downloaded dataset to be plotted
wind_fitted_data <- wind.fit(wind_data)

# Create raster file from the data.frame created by wind.fit, representing wind speed.
wind2raster(wind_fitted_data, type="speed")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~gfs }
\keyword{ ~wind }% __ONLY ONE__ keyword per line
