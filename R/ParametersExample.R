#'Parameters Example
#'
#'A test list of parameters for `surveySim()`
#'
#' @details
#' This is just a test list of parameters to be used as an example in `surveySim()`.
#' @details The values of this parameters are:
#' @details Width between survey lines (`col.width`) = **50 m**
#' @details Type of survey grid (`grid.type`) = **hexagonal**
#' @details Number of `simulations` = **10**
#' @details Survey `area` = **0.5km x 0.5km**
#' @details Density of sites (`site.density`) = **20**
#' @details Area of sites (`site.area`) = **10,000\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}}**
#' @details Maximum site `overlap` = **0.5**
#' @details Density of artifacts (`obj.density`) = **1/\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}}**
#' @details Artifact distribution (`obj.distribution`) = **spherical**
#' @details Survey radius (`survey.radius`) = **0.5m**
#'
#' @usage
#' parametersExample

#'
#'@export
parametersExample<-list("col.width"=50,"grid.type"="hexagonal","simulations"=10,"area"=c(0.5,0.5),
                        "site.density"=20,"site.area"=10000,"overlap"=0.5,
                        "obj.density"=1,"obj.distribution"="spherical","survey.radius"=0.5)
class(parametersExample)<-"surveySim"
