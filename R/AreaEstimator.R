#' Area Estimator
#'
#' Estimate the area of multiple overlapping ellipses
#'
#' @details
#' This function will estimate the area occupied by sites (ellipses) in a rectangular
#' field, taking into consideration the fact that sites can overlap. It is formatted to be used inside
#' `fieldMap()`.
#'
#' This function is a cookie-cutter area estimator, given the complexities of
#' calculating the real areas of overlapping ellipses. It projects N x N equally spaced dots in the survey field
#'  and calculates the ratio of how many of them fall inside at least one site (ellipse). Using a precision of 1000 x 1000 dots,
#'  it approximates area to within 0.1% of real area.
#'
#'@param sitemap a matrix with sites per row and columns:
#'`site_number`, `site_area`, `eccentricity`, `angle`, `center.x`, `center.y`, `ellipse.a`,`ellipse.b`
#'@param fieldarea vector with dimensions of field surveyed in km: `c(x_size,y_size)`
#'@param precision how many dots will be projected of field. Total dots equal `precision * precision`.
#'Default value = `1000` (1 million dots projected)
#'
#'@return The rate of points that are inside at least one ellipse divided by all points projected in the area.
#'
#'@examples
#'  #create a matrix with 2 sites randomly located using `fieldMap()`
#'  site.example<-fieldMap(c(1,1),2,250000,plot=TRUE)
#'
#'  #define size of field
#'  field.area<-c(1,1)
#'
#'  #calculate area
#'  areaEstimator(site.example$site.frame,field.area)
#'
#'@export

areaEstimator<-function(sitemap, fieldarea, precision=1000){

  nsites<-nrow(sitemap)
  site.frame<-sitemap

  dotsx<-seq(0,fieldarea[1],length=precision)
  dotsy<-seq(0,fieldarea[2],length=precision)

  areamat<-matrix(FALSE,length(dotsx),length(dotsy))

  for(a in 1:nsites){
    for(b in 1:length(dotsx)){

      tmpcol<-((dotsx[b]-site.frame[a,5])*cos(site.frame[a,4])+(dotsy-site.frame[a,6])*sin(site.frame[a,4]))^2/(site.frame[a,7])^2+
        ((dotsx[b]-site.frame[a,5])*sin(site.frame[a,4])-(dotsy-site.frame[a,6])*cos(site.frame[a,4]))^2/(site.frame[a,8])^2<=1

      areamat[b,which(tmpcol==TRUE)]<-TRUE
    }
  }

  siteArea<-1-(nrow(areamat)*ncol(areamat)-sum(areamat))/(nrow(areamat)*ncol(areamat))

  siteArea<-siteArea*fieldarea[1]*fieldarea[2]

  return(siteArea)
}
