#' Cloud Generator
#'
#' Creates a cloud of dots inside ellipsoid sites
#'
#' @details `cloudGenerator` creates a cloud of point inside an ellipsoid site of predefined
#' size and shape, to represent the locations of artifacts in a site. The function can build artifact scatters
#' with different densities profiles. The function uses an "onion-layer" approach to approximate the density of points from the center.
#' In practice, it means that each site is composed of N ellipse slices surrounding the previous slice, with each slice
#' having a different artifact density depending on the density function selected. This approach also makes
#' the `surveySim` function more efficient, since it will search for artifact hits only on the slices that intersect the
#' survey pits.
#' @param  density dots (artifacts) per \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}}
#' @param a ellipse (site) long axis in km
#' @param b ellipse (site) short axis in km
#' @param angle ellipse (site) angle of rotation in radians
#' @param center.x center of ellipse in x axis
#' @param center.y center of ellipse in y axis
#' @param type type of density distribution. Choose from:
#'\itemize{
#'  \item `uniform`: dots are uniformly distributed within ellipse
#'  \item `linear`: density of dots decreases linearly from center
#'  \item `spherical`: density of dots decreases following elliptical function (abrupt drop near margins)
#'  \item `sinusoidal`: density of dots decreases following sinusoidal equation
#'}
#' @param precision how many slices of the distribution will be made (more = much slower run times). Default = `30`
#' @param plot if function should plot results. **function does not work outside `surveySim()`**
#'
#' @return A list with two objects: \tabular{ll}{
#'    \code{coords} \tab A list of the bands (N=`precision`) that represent the site, each populated with the
#'  X and Y coordinates for all artifacts generates inside them. \cr
#'    \tab \cr
#'    \code{info} \tab A list with the number of pieces created (`$total.pieces`), area of the site (`$total.area`), and
#'  artifact density in the site (`$actual.density`)\cr
#'  }
#'
#' @examples
#'    #create a small site with low density uniform distribution
#'    uni.site<-cloudGenerator(0.1,0.1,0.05,pi/4,0.5,0.5,type="u")
#'
#'    #plot a site with uniform artifact distribution through surveySim
#'    SiteParameters<-parametersExample
#'    SiteParameters$simulations=1
#'    SiteParameters$site.density=1
#'    SiteParameters$obj.density=0.1
#'    SiteParameters$obj.distribution = "u"
#'    surveySim(SiteParameters,plot.artifacts = TRUE)
#'
#'    #plot a site with sinusoidal artifact distribution through surveySim
#'    SiteParameters$obj.distribution = "si"
#'    surveySim(SiteParameters,plot.artifacts = TRUE)
#'
#' @importFrom grDevices rgb
#' @importFrom graphics axis box lines plot.new plot.window points polygon text title
#' @importFrom stats runif
#'
#' @export
#
cloudGenerator<-function(density,a,b,angle,center.x,center.y,type="uniform",precision=30, plot=FALSE){

  #first, we do some control checks.
  TYPE<-c("uniform","linear","spherical","sinusoidal")

  type<-pmatch(type,TYPE)

  if (is.na(type)){
    stop(cat("ERROR: invalid cloud distribution type.","Valid types are: uniform,linear,spherical,sinusoidal",sep="\n"))
  }

  #2. get the apropriate piece density for each slice.

  density.vector<-rep(NA,precision)

  #we need to convert density to km^2, to bring it to the scale of the other functions.
  density<-density*1e6

  if(type==1){#uniform
    density.vector<-rep(density,precision)
  }

  if(type==2){#linear
    linear.x<-seq(0,precision,length=precision+1)
    #we need to calculate what is the proportion of the area in each band, to get the piece density
    area.x<-(pi*(linear.x/precision*a)*(linear.x/precision*b))/(pi*a*b)*precision

    tmpdensity<-2*density-(2*density/precision)*area.x

    density.vector<-(tmpdensity[1:precision]+tmpdensity[2:(precision+1)])/2
  }

  if(type==3){#spherical
    linear.x<-seq(0,precision,length=precision+1)
    area.x<-(pi*(linear.x/precision*a)*(linear.x/precision*b))/(pi*a*b)*precision
    densitymax<-4*density/pi
    tmpdensity<-densitymax*(1-area.x^2/precision^2)^0.5

    density.vector<-(tmpdensity[1:precision]+tmpdensity[2:(precision+1)])/2
  }

  if(type==4){#sinusoidal
    linear.x<-seq(0,precision,length=precision+1)
    area.x<-(pi*(linear.x/precision*a)*(linear.x/precision*b))/(pi*a*b)*precision

    tmpdensity<-density*cos((pi/precision)*area.x)+density

    density.vector<-(tmpdensity[1:precision]+tmpdensity[2:(precision+1)])/2
  }

  colors.scale<-viridis::plasma(100)#colors to test the plotting

  if(type==1){
    colors.plot<-rep(rgb(0,0,1,0.5),precision)
  }else{
    colors.plot<-colors.scale[floor(density.vector/max(density.vector)*100)]
  }

  results<-list(coords=NA,info=NA)
  results$coords<-list()
  results$info<-list()
  total.pieces<-0

  #3.Start the loop for each slice
  for (i in 1:precision){
    #4.Get the random dots, cut them inside the ellipsis and save them
    ndots<-density.vector[i]*2*((i/precision)*a)*2*((i/precision)*b)
    xvals.tmp<-runif(ndots,-1*(i/precision)*a,(i/precision)*a)
    yvals.tmp<-runif(ndots,-1*(i/precision)*b,(i/precision)*b)

    trim.index<-which(xvals.tmp^2/((i/precision)*a)^2+yvals.tmp^2/((i/precision)*b)^2<=1)

    band.area<-pi*((i/precision)*a)*((i/precision)*b)

    if(i>1){
      trim.index.minus<-which(xvals.tmp^2/(((i-1)/precision)*a)^2+yvals.tmp^2/(((i-1)/precision)*b)^2>1)
      trim.index<-intersect(trim.index,trim.index.minus)
      band.area<-band.area-pi*(((i-1)/precision)*a)*(((i-1)/precision)*b)
    }

    xvals<-xvals.tmp[trim.index]
    yvals<-yvals.tmp[trim.index]

    xvals.rot<-(xvals*cos(angle)-yvals*sin(angle))+center.x
    yvals.rot<-(yvals*cos(angle)+xvals*sin(angle))+center.y

    xvals<-xvals.rot
    yvals<-yvals.rot

    total.pieces<-total.pieces+length(xvals)

    results$coords[[i]]<-cbind(xvals,yvals)
  }

  #if plot = TRUE, print plot.
  if(plot==TRUE){

    for(i in 1:precision){
      points(results$coords[[i]][,1],results$coords[[i]][,2],pch=".",col=colors.plot[i])
    }
  }

  results$info<-list(total.pieces=total.pieces,total.area=as.numeric(a*b*pi),actual.density=as.numeric((total.pieces/(a*b*pi))/1e6))

  return(results)
}
