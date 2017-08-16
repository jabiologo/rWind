\name{arrowDir}
\alias{arrowDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Arrow direction fitting for Arrowhead function
}
\description{
arrowDir adapts wind direction value to be used by Arrowhead function from "shape" package to plot wind direction for each coordinate.

}
\usage{
arrowDir(W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{W}{
A data.frame obtained by function wind.fit. It should content a column named "dir".
}
}
\details{
Angle argument of Arrowhead function from "shape" package, needs to be fed in an anti-clockwise way, relative to x-axis, in degrees [0,360]. arrowDir function adapts wind direction provided by wind.fit (clockwise, relative to y-axis ) to requirements of Arrowhead.
}
\value{
A vector with angles for each arrow to be plotted by Arrowhead.
}
\references{
"R Package shape:  functions for plotting graphical shapes, colors..." Karline Soetaert
}
\author{
Javier Fernández-López
}
\note{
arrowDir function works always together with Arrowhead function from "shape" package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wind.fit}}, \code{\link{Arrowhead}}
}
\examples{
# Download wind for Iberian Peninsula region at 2015, February 12, 00:00
# wind_data <- wind.dl(2015,2,12,0,-10,5,35,45)

data(wind_data)

# Fit downloaded dataset to be plotted
wind_fitted_data <- wind.fit(wind_data)

# Create a vector with wind direction (angles) adapted
alpha=arrowDir(wind_fitted_data)

# Now, you can plot wind direction with Arrowhead function from shapes package
#Load "shape package
#require(shape)
# plot(wind_data$lon, wind_data$lat, type="n")
# Arrowhead(wind_data$lon, wind_data$lat, angle=alpha, arr.length = 0.1, arr.type="curved")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Arrowhead }
\keyword{ ~wind }% __ONLY ONE__ keyword per line
