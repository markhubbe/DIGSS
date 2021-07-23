#' Survey Loops
#'
#' Perform multiple survey simulations changing values on one variable and plot the results
#'
#' @details
#' `surveyLoops` will run a series of simulations along one variable with values
#'  provided by user. Through this function, the user can simulate and evaluate the changes
#'  in efficiency and efficacy of specific variables, while holding every other value constant. The function
#'  runs multiple instances of `surveySim` using values of `surveyParameters` and replacing one of them with a sequence of values offered by the user.
#'
#' @param surveyParameters list of parameters (object class `surveySim`)
#'
#' @param loopVariable variable to be looped. Can be any of the variables that exist in `surveyParameters` \itemize{
#' \item `col.width` - vector of numbers with distances between STP rows
#' \item `grid.type` - vector of strings with names of grid types
#' \item `simulations` - vector of numbers with numbers of simulations
#' \item `area` - **list** with 2 vectors, one for x and one for y of area (vectors MUST be same length)
#' \item `site.density` - vector with numbers of site density OR **list** with 2 vectors, one with minimum site densities, one with maximum (vectors MUST be same length)
#' \item `site.area` - vector with numbers of site areas OR **list** with 4 vectors, one with minimums, one with maximums, one with means, one with st.devs (vectors MUST be same length)
#' \item `overlap` - vector of numbers with varying overlaps
#' \item `obj.density` - vector with varying artifact density OR **list** with 2 vectors, one with minimum artifact densities, one with maximum (vectors MUST be same length)
#' \item `obj.distribution` - vector of names of artifact distribution
#' \item `survey.radius` vector with varying survey radii
#' }
#' @param loopSequence object with varying values, as defined above
#' @param plotResult which results from the summary table will be plotted:\itemize{
#' \item `surveysPerSim` - plots the total number of surveys created in each simulation
#' \item `sitesFound` - plots the frequency of sites found
#' \item `sitesFoundOnArtifacts` - plots the frequency of sites found based on surveys finding at least one artifact
#' \item `artifactsPerSurvey` - plots the average number of artifacts found per survey in each simulation
#' \item `successRateIndex` - plots the success rate index (ratio of survey pits that found a site over total number of survey pits made)
#' }
#'
#' @return A list with five objects: \tabular{ll}{
#'    \code{surveysPerSim} \tab A matrix with the number of survey pits done in each simulation.\cr
#'    \tab \cr
#'    \code{sitesFound} \tab A matrix with the summary statistics about frequency of sites found in each simulation. \cr
#'    \tab \cr
#'    \code{sitesFoundOnArtifacts} \tab A matrix with the summary statistics about frequency of sites detected based on artifacts found in survey pits in each simulation. \cr
#'    \tab \cr
#'    \code{artifactsPerSurver} \tab A matrix with the summary statistics about artifacts found per survey pit in each simulation. \cr
#'    \tab \cr
#'    \code{succesRateIndex} \tab A matrix with the summary statistics about success rate (number of succesful survey pits/total survey pits) in each simulation.\cr
#'  }
#'
#' @examples
#' #Loop the impact of increasing distances between survey rows
#' width.loop<-surveyLoops(parametersExample,"col.width",c(50,75,100,125,150),"sitesFound")
#'
#' #Loop the impact of different artifact distributions on surveys
#' distr.loop<-surveyLoops(
#'                parametersExample,
#'                "obj.distribution",
#'                c("uniform","linear","spherical","sinusoidal"),
#'                "sitesFoundOnArtifacts")
#'
#' @importFrom grDevices rainbow rgb
#' @importFrom graphics axis box lines plot.new plot.window points polygon text title
#'
#' @export

surveyLoops<-function(surveyParameters,loopVariable,loopSequence,plotResult){

  #1.we do a series of tests to allow for partial text matching and checking for errors

  LOOPVARIABLE<-c("col.width","grid.type","simulations","area","site.density","site.area",
                  "overlap", "obj.density", "obj.distribution", "survey.radius")

  loopVariable<-pmatch(loopVariable,LOOPVARIABLE)

  if(is.na(loopVariable)==TRUE){
    stop("ERROR: loopVariable not valid. Chose a valid variable to loop.\n")
  }

  PLOTRESULT<-c("surveysPerSim","sitesFound","sitesFoundOnArtifacts","artifactsPerSurvey","successRateIndex")

  plotResult<-pmatch(plotResult,PLOTRESULT)

  if(is.na(plotResult)==TRUE){
    stop("ERROR: plotResult is not valid. Chose a valid variable to plot results.\n")
  }

  if(is.list(loopSequence)==TRUE){
    same.size=TRUE
    for (a in 1:(length(loopSequence)-1)){
      if(length(loopSequence[[a]])!=length(loopSequence[[a+1]])){
        same.size=TRUE
      }
    }
    if(same.size==FALSE){
      stop("ERROR: Vectors inside list in loopSequence are not the same size.\n")
    }
  }

  if(loopVariable==4 & is.list(loopSequence)==FALSE){#if Area input is not a list
    stop("ERROR: loopSequence for Area must be a list with 2 vectors (mins and maxs).\n")
  }

  print(loopVariable)
  #2. Here we start with creating the basics and results object

  if(is.list(loopSequence)==TRUE){
    nloops<-length(loopSequence[[1]])
  }else{
    nloops<-length(loopSequence)
  }

  results<-list(surveysPerSim = matrix(NA,nloops,1),
                sitesFound = matrix(NA,nloops,6),
                sitesFoundOnArtifacts = matrix(NA,nloops,6),
                artifactsPerSurvey = matrix(NA,nloops,6),
                successRateIndex = matrix(NA,nloops,6))

  colnames(results[[1]])<-"N"
  colnames(results[[2]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[3]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[4]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[5]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")

  if(is.list(loopSequence)==TRUE){
    row.names(results[[1]])<-loopSequence[[1]]
    row.names(results[[2]])<-loopSequence[[1]]
    row.names(results[[3]])<-loopSequence[[1]]
    row.names(results[[4]])<-loopSequence[[1]]
    row.names(results[[5]])<-loopSequence[[1]]
  }else{
    row.names(results[[1]])<-loopSequence
    row.names(results[[2]])<-loopSequence
    row.names(results[[3]])<-loopSequence
    row.names(results[[4]])<-loopSequence
    row.names(results[[5]])<-loopSequence
  }
  #3. We start the loop here, and get all the parameters we need for each run. Lots of ifs...

  tmp.surveyParameters<-surveyParameters
  for(a in 1:nloops){
    if(loopVariable==1){ #col.width
      tmp.surveyParameters[[1]]<-loopSequence[a]
    }

    if(loopVariable==2){ #grid.type
      tmp.surveyParameters[[2]]<-loopSequence[a]
    }

    if(loopVariable==3){ #simulations
      tmp.surveyParameters[[3]]<-loopSequence[a]
    }

    if(loopVariable==4){ #area
      tmp.surveyParameters[[4]]<-c(loopSequence[[1]][a],loopSequence[[2]][a])
    }

    if(loopVariable==5){ #site.density
      if(is.list(loopSequence)==TRUE){
        tmp.surveyParameters[[5]]<-c(loopSequence[[1]][a],loopSequence[[2]][a])
      }else{
        tmp.surveyParameters[[5]]<-loopSequence[a]
      }
    }

    if(loopVariable==6){ #site.area
      if(is.list(loopSequence)==TRUE){
        tmp.surveyParameters[[6]]<-c(loopSequence[[1]][a],loopSequence[[2]][a],loopSequence[[3]][a],loopSequence[[4]][a])
      }else{
        tmp.surveyParameters[[6]]<-loopSequence[a]
      }
    }

    if(loopVariable==7){ #overlap
      tmp.surveyParameters[[7]]<-loopSequence[7]
    }

    if(loopVariable==8){ #obj.density
      if(is.list(loopSequence)==TRUE){
        tmp.surveyParameters[[8]]<-c(loopSequence[[1]][a],loopSequence[[2]][a])
      }else{
        tmp.surveyParameters[[8]]<-loopSequence[a]
      }
    }

    if(loopVariable==9){ #obj.distribution
      tmp.surveyParameters[[9]]<-loopSequence[a]
    }

    if(loopVariable==10){ #survey.radius
      tmp.surveyParameters[[10]]<-loopSequence[a]
    }

    #4.Here we call the surveySim and run the loop iteration
    cat(paste("Iteration ",a," of ",nloops,".\n",sep=""))

    tmp.results<-surveySim(tmp.surveyParameters,plot=FALSE)

    results[[1]][a,1]<-tmp.results[[1]][1,1]
    results[[2]][a,]<-tmp.results[[1]][2,]
    results[[3]][a,]<-tmp.results[[1]][3,]
    results[[4]][a,]<-tmp.results[[1]][4,]
    results[[5]][a,]<-tmp.results[[1]][5,]
  }

  #5.Here we create the plot.
  #a. get some fancy colors
  colorsolid<-rainbow(10)[loopVariable]
  coloralpha<-rainbow(10,alpha=0.3)[loopVariable]

  #b.get the title names for variables and sequence to plot
  if(loopVariable==1){ #col.width
    label = "Distance between survey columns"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Distance (m) in each iteration"
  }

  if(loopVariable==2){ #grid.type
    label = "Grid Types"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Grid Types"
  }

  if(loopVariable==3){ #simulations
    label = "Number of simulations"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Simulations in each iteration"
  }

  if(loopVariable==4){ #area
    label = "Area Sizes"
    xmin<-1
    xmax<-length(loopSequence[[1]])
    xlabels<-loopSequence[[1]]*loopSequence[[2]]
    xlab<-"area (km^2) in each iteration"
  }

  if(loopVariable==5){ #site.density
    label = "Site Densities"
    if(is.list(loopSequence)==TRUE){
      xmin<-1
      xmax<-length(loopSequence[[1]])
      xlabels<-loopSequence[[1]]*loopSequence[[2]]/2
      xlab<-"Average site density in each iteration"
    }else{
      xmin<-1
      xmax<-length(loopSequence)
      xlabels<-loopSequence
      xlab<-"Site density in each iteration"
    }
  }

  if(loopVariable==6){ #site.area
    label = "Site areas"
    if(is.list(loopSequence)==TRUE){
      xmin<-1
      xmax<-length(loopSequence[[1]])
      xlabels<-loopSequence[[3]]
      xlab<-"Average site area in each iteration"
    }else{
      xmin<-1
      xmax<-length(loopSequence)
      xlabels<-loopSequence
      xlab<-"Site area in each iteration"
    }
  }

  if(loopVariable==7){ #overlap
    label = "Site overlaps"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Maximum overlap permitted in each iteration"
  }

  if(loopVariable==8){ #obj.density
    label = "Artifact densities"
    if(is.list(loopSequence)==TRUE){
      xmin<-1
      xmax<-length(loopSequence[[1]])
      xlabels<-loopSequence[[1]]*loopSequence[[2]]/2
      xlab<-"Average artifact density in each iteration"
    }else{
      xmin<-1
      xmax<-length(loopSequence)
      xlabels<-loopSequence
      xlab<-"Artifact density in each iteration"
    }
  }

  if(loopVariable==9){ #obj.distribution
    label = "Artifacts distribution"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Artifact distribution in each iteration"
  }

  if(loopVariable==10){ #survey.radius
    label = "Survey radii"
    xmin<-1
    xmax<-length(loopSequence)
    xlabels<-loopSequence
    xlab<-"Survey radii in each iteration"
  }

  #c. get the y values
  ymin=0

  if(plotResult==1){ #"surveysPerSim"
    ymax<-max(results[[1]])
    ylab<-"Number of Surveys"
  }

  if(plotResult==2){ #"sitesFound"
    ymax<-max(results[[2]])
    ylab<-"Frequency of sites found"
  }

  if(plotResult==3){ #"sitesFoundOnArtifacts"
    ymax<-max(results[[3]])
    ylab<-"Frquency of sites found by artifacts"
  }

  if(plotResult==4){ #"artifactsPerSurvey"
    ymax<-max(results[[4]])
    ylab<-"Average number of artifacts"
  }

  if(plotResult==5){ #"successRateIndex"
    ymax<-max(results[[5]])
    ylab<-"Success Rate Index"
  }

  #d.getting to plotting
  plot.new()
  plot.window(c(xmin,xmax),c(ymin,ymax))

  axis(1,at=xmin:xmax,labels=xlabels)
  axis(2)
  box()

  title(main=label,xlab=xlab,ylab=ylab)

  if(plotResult!=1){
    polygon(c(1:xmax,xmax:1),
            c(results[[plotResult]][,3],results[[plotResult]][xmax:1,4]),
            col=coloralpha,border = NA)

    polygon(c(1:xmax,xmax:1),
            c(results[[plotResult]][,1]-results[[plotResult]][,2],results[[plotResult]][xmax:1,1]+results[[plotResult]][xmax:1,2]),
            col=coloralpha,border=NA)

    lines(1:xmax,results[[plotResult]][,1]-results[[plotResult]][,2],
          col=colorsolid,lty=2,lwd=1.5)

    lines(1:xmax,results[[plotResult]][,1]+results[[plotResult]][,2],
          col=colorsolid,lty=2,lwd=1.5)

  }

  lines(1:xmax,results[[plotResult]][,1],
        col=colorsolid,lwd=2)

  return(results)
}
