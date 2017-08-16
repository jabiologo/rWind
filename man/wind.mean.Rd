\name{wind.mean}
\alias{wind.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wind-data mean
}
\description{
wind.mean computes the mean (average) of a time serie dataset of winds in the same region. To do this, wind.mean uses U and V vector components of several wind data.frames stored in a list. Note that, if you want to perform wind direction and speed average, first you should calculate the mean of U and V components and then transform it to direction and speed using wind.fit function from rWind package.
}
\usage{
wind.mean(wind_serie)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wind_serie}{
A list of data.frames downloaded by wind.dl function.
}
}

\value{
A data.frame with a similar format as resulted by wind.dl, prepared to be transformed by wind.fit.
}
\references{
https://en.wikipedia.org/wiki/Cross_product
}
\author{
Javier Fernández-López (jflopez@rjb.csic.es)
}
\note{
For large time series, it could take a while.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wind.dl}}, \code{\link{wind.fit}}
}
\examples{
# First, you should create an empty list to store all the data

wind_serie<- list()

# Then, you can use a wind.dl inside a for-in loop to download and store wind data of
# the first 2 days of February 2015 at 00:00 around Spain. It could take a while...

for (d in 1:2){
  w<-wind.dl(2015,2,d,0,-9,4,36,44)
  wind_serie[[d]]<-w
}

# Finally, you can implement wind.mean and wind.fit to compute the average of all winds
# datasets in the list:

wind_average<- wind.mean(wind_serie)

# wind.fit(wind_average)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
