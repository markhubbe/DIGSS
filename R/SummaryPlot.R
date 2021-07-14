#' Plot Survey Summaries
#'
#' Plots the different results from `SurveySim()`
#'
#' @details This function will plot the results of the `SurveySim` Simulations using Kernel Density plots.
#' All the grids that are to be compared should be grouped into 1 list (list(a,b,c,etc...))
#' The plot function allows you to choose different parts of the survey summaries produced by `SurveySim()` you want to plot.
#'
#' @param SummaryList a list of survey summaries, the output of `SurveySim()`
#' @param plot what variable to plot. Options are:
#'
#'   `sites.found` - plots frequency of sites found
#'
#'   `survey.hits` - plots frequency of surveys that hit a site
#'
#'   `success.rate.index` - plots the success rate of surveys, i.e. the ratio of successful surveys over total surveys
#'
#'   `sites.foundARTI` - plots frequency of sites found **based on artifact present in sites**
#'
#'   `survey.hitsARTI` - plots frequency of surveys that found **at least one artifact**
#' @param labels vector with name of each item in list, to be added to the legend. If `NULL`, names will be taken from list
#' @examples
#'  #create 3 Simulations with sites of different sizes:
#'  small.sites<-ParametersExample
#'  small.sites$site.area=500
#'
#'  medium.sites<-ParametersExample
#'  medium.sites$site.area=1000
#'
#'  large.sites<-ParametersExample
#'  large.sites$site.area=2000
#'
#'  #run the 3 simulations
#'  small.survey<-SurveySim(small.sites)
#'  medium.survey<-SurveySim(medium.sites)
#'  large.survey<-SurveySim(large.sites)
#'
#'  #create the comparative plot.
#'  #note that the results go into a list. If labels are not given, legend is built on list names
#'  PlotSurveySumm(list(small.survey,medium.survey,large.survey),plot="sites.found",labels=c("Small sites","Medium sites","Large sites"))
#'
#' @export
PlotSurveySumm<-function(SummaryList,plot="sites.found",labels=NULL){

  #1.Define the variable to be plotted
  if(plot=="sites.found"){
    targetmatrix=2
    targetcol=5
    MainTitle="Frequency of sites discovered"
  }

  if(plot=="survey.hits"){
    targetmatrix=2
    targetcol=6
    MainTitle="Frequency of surveys that located sites"
  }

  if(plot=="success.rate.index"){
    targetmatrix=2
    targetcol=10
    MainTitle="Survey Success Rate Index"
  }

  if(plot=="sites.foundARTI"){
    targetmatrix=3
    targetcol=5
    MainTitle="Frequency of sites discovered by artifact"
  }

  if(plot=="survey.hitsARTI"){
    targetmatrix=3
    targetcol=6
    MainTitle="Frequency of surveys that located sites by artifact"
  }

  #2.Create the data.frame that will be passed to ggplot
  plotlabels = rep("",length(SummaryList))
  plotdata=data.frame(matrix(NA,0,2))
  means = data.frame(matrix(NA,length(SummaryList),2))

  for(a in 1:length(SummaryList)){
    if(is.null(labels)==TRUE){
      if(is.null(names(SummaryList))==TRUE){
        plotlabels[a]=paste("Summary",a)
      }else{
        plotlabels[a]=names(SummaryList)[a]
      }
    }else{
      plotlabels[a]=labels[a]
    }

    tmp.dataframe<-data.frame(matrix(NA,nrow(SummaryList[[a]][[targetmatrix]]),2))

    tmp.dataframe[,1]=rep(plotlabels[a],nrow(SummaryList[[a]][[targetmatrix]]))
    tmp.dataframe[,2]=SummaryList[[a]][[targetmatrix]][,targetcol]
    plotdata = rbind(plotdata,tmp.dataframe)

    means[a,1] = plotlabels[a]
    means[a,2] = mean(tmp.dataframe[,2])

  }

  colnames(plotdata)=c("groups","data")
  colnames(means)=c("groups","mean")
  plotdata[,1]<-factor(plotdata[,1])
  means[,1]<-factor(means[,1])

  #3.Create the plot
  ggplot2::ggplot(plotdata,aes(x=data,color=groups,fill=groups))+
    ggplot2::geom_density(alpha=0.5)+
    ggplot2::geom_vline(data=means,aes(xintercept=mean, color=groups), linetype="dashed", size=1)+
    ggplot2::ggtitle(MainTitle)+
    ggplot2::labs(x=ifelse(plot=="success.rate.index","Success Rate Index","frequency"),
                  color = "Summaries",fill="Summaries")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
    ggplot2::scale_color_brewer(palette="Dark2")+
    ggplot2::scale_fill_brewer(palette = "Dark2")


}


