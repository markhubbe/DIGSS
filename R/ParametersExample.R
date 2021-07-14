#'Parameters Example
#'
#'A test of parameters for `SurveySim()`
#'
#' @details
#' This is just a test list of parameters to be used as an example in `SurveySim()`.
#' @details The values of this parameters are:
#' @details Width between survey lines (`col.width`) = **50 m**
#' @details Type of survey grid (`grid.type`) = **hexagonal**
#' @details Number of `simulations` = **25**
#' @details Survey `area` = **0.5km x 0.5km**
#' @details Density of sites (`site.density`) = **20**
#' @details Area of sites (`site.area`) = **10,000 m^2**
#' @details Maximum site `overlap` = **0.5**
#' @details Density of artifacts (`obj.density`) = **1/m^2**
#' @details Artifact distribution (`obj.distribution`) = **spherical**
#' @details Survey radius (`survey.radius`) = **0.5m**
#'
#' @usage
#' ParametersExample

#'
#'@export
ParametersExample<-list("col.width"=50,"grid.type"="hexagonal","simulations"=25,"area"=c(0.5,0.5),
                        "site.density"=20,"site.area"=10000,"overlap"=0.5,
                        "obj.density"=1,"obj.distribution"="spherical","survey.radius"=0.5)
class(ParametersExample)<-"SurveySim"
