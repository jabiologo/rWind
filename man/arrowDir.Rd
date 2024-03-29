% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind_functions2.R
\name{arrowDir}
\alias{arrowDir}
\title{Arrow direction fitting for Arrowhead function from "shape" package}
\usage{
arrowDir(W)
}
\arguments{
\item{W}{An object of class \code{rWind} or a data.frame which should content
a column named "dir".}
}
\value{
A vector with angles for each arrow to be plotted by Arrowhead.
}
\description{
arrowDir adapts wind direction value to be used by Arrowhead function from
"shape" package to plot wind direction for each coordinate.
}
\details{
Angle argument of Arrowhead function from "shape" package needs to be fed
in an anti-clockwise way, relative to x-axis, in degrees 0,360. arrowDir
function adapts wind direction provided by wind.fit (clockwise, relative to
y-axis ) to requirements of Arrowhead.
}
\note{
arrowDir function works always together with Arrowhead function from
"shape" package.
}
\examples{
data(wind.data)

# Create a vector with wind direction (angles) adapted
alpha <- arrowDir(wind.data)
\dontrun{
# Now, you can plot wind direction with Arrowhead function from shapes package
# Load "shape package
require(shape)
plot(wind.data$lon, wind.data$lat, type = "n")
Arrowhead(wind.data$lon, wind.data$lat,
  angle = alpha,
  arr.length = 0.1, arr.type = "curved"
)
}

}
\references{
Karline Soetaert (2017). shape: Functions for Plotting Graphical
Shapes, Colors. R package version 1.4.3.
https://CRAN.R-project.org/package=shape
}
\seealso{
\code{\link{wind.dl}}
}
\author{
Javier Fernández-López
}
\keyword{~wind}
