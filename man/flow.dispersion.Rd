\name{flow.dispersion}
\alias{flow.dispersion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flow dispersion
}
\description{
flow.dispersion computes movement conductance through a flow either, sea or wind currents. It implements the formula described in Felicisimo et al. 2008:

                      Cost=(1/Speed)*(HorizontalFactor)

being HorizontalFactor a "function that incrementaly penalized angular deviations from the wind direction" (Felicisimo et al. 2008). The function returns conductance values (1/cost) to move betwen all cells in a raster having into account flow speed and direction obtained from wind.fit function("rWind" package). As wind or sea currents implies directionality, flow.dispersion produces an anisotropic conductance matrix (asimetric). Conductance values are used later to built a TransitionLayer object from "gdistance" package.
}
\usage{
flow.dispersion(dl,sl,type="passive", output=c("raw", "transitionLayer", "igraph"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dl}{
Direction layer. Raster layer obtained from wind2raster function ("rWind"" package) with flow direction values.
}
\item{sl}{
Speed layer. Raster layer obtained from wind2raster function ("rWind"" package) with flow speed values.
}
\item{type}{
Could be either "passive" or "active".In "passive" mode, movement cannot go against flow direction (it doesn't allow HorizontalFactor higher than 120). In "active" mode, the movement can go against flow direction, by increasing the cost.
}
\item{output}{ 
BLA BLA
}
}
\details{
flow.dispersion creates a sparse Matrix with conductance values (1/cost).
}
\value{
A TransitionLayer object from "gdistance" package that could be used to compute least cost paths.
}
\references{

Felicísimo, Á. M., Muñoz, J., & González-Solis, J. (2008). Ocean surface winds drive dynamics of transoceanic aerial movements. PLoS One, 3(8), e2928.

van Etten, J. (2012). gdistance: Distances and routes on geographical grids. R package version 1.1–4. Available a t CRAN. R-project. org/package= gdistance.
}
\author{
Javier Fernández-López (jflopez@rjb.csic.es);
Jacob van Etten
}
\note{
Note that for large data sets, it could take a while. For large study areas is strongly adviced perform the analysis in a remote computer or a cluster.
}

\seealso{
\code{\link{wind.fit}}, \code{\link{wind2raster}}, \code{\link{TransitionLayer}}
}
\examples{

require(gdistance)

w<-wind.dl(2015,2,12,0,-9,-7,35,37)
w<-wind.fit(w)

dl<-wind2raster(w)
sl<-wind2raster(w, type="speed")

Conductance<-flow.dispersion(dl,sl,"passive", "tr")

transitionMatrix(Conductance)
image(transitionMatrix(Conductance))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~anisotropy }
\keyword{ ~conductance }% __ONLY ONE__ keyword per line
