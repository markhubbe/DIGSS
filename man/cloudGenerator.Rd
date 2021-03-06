% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CloudGenerator.R
\name{cloudGenerator}
\alias{cloudGenerator}
\title{Cloud Generator}
\usage{
cloudGenerator(
  density,
  a,
  b,
  angle,
  center.x,
  center.y,
  type = "uniform",
  precision = 30,
  plot = FALSE
)
}
\arguments{
\item{density}{dots (artifacts) per m^2}

\item{a}{ellipse (site) long axis in km}

\item{b}{ellipse (site) short axis in km}

\item{angle}{ellipse (site) angle of rotation in radians}

\item{center.x}{center of ellipse in x axis}

\item{center.y}{center of ellipse in y axis}

\item{type}{type of density distribution. Choose from:

\code{uniform}: dots are uniformly distributed within ellipse

\code{linear}: density of dots decreases linearly from center

\code{spherical}: density of dots decreases following elliptical function (abrupt drop near margins)

\code{sinusoidal}: density of dots decreases following sinusoidal equation}

\item{precision}{how many slices of the distribution will be made (more = much slower run times). Default = \code{30}}

\item{plot}{if function should plot results. \strong{function does not work outside \code{SurveySim()}}}
}
\description{
Creates a cloud of dots inside ellipsoid sites
}
\details{
\code{cloudGenerator} creates a cloud of point inside an ellipsoid site of predefined
size and shape, to represent the locations of artifacts in a site. The function can build artifact scatters
with different densities profiles. The function uses an "onion-layer" approach to approximate the density of points from the center.
In practice, it means that each site is composed of N ellipse slices surrounding the previous slice, with each slice
having a different artifact density depending on the density function selected. This approach also makes
the \code{SurveySim} function more efficient, since it will search for artifact hits only on the slices that intersect the
survey pits.
}
\examples{
   #create a small site with low density uniform distribution
   uni.site<-cloudGenerator(0.1,0.1,0.05,pi/4,0.5,0.5,type="u")

   #plot a site with uniform artifact distribution through SurveySim
   SiteParameters<-ParametersExample
   SiteParameters$simulations=1
   SiteParameters$site.density=1
   SiteParameters$obj.density=0.1
   SiteParameters$obj.distribution = "u"
   SurveySim(SiteParameters,plot.artifacts = TRUE)

   #plot a site with sinusoidal artifact distribution through SurveySim
   SiteParameters$obj.distribution = "si"
   SurveySim(SiteParameters,plot.artifacts = TRUE)

}
