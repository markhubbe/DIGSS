#' Survey Loops
#'
#' Perform multiple survey simulations changing values on one variable and plot the results
#'
#' @details
#' `SurveyLoops` will run a series of simulations along one variable with values
#'  provided by user. Through this function, the user can simulate and evaluate the changes
#'  in efficiency and efficacy of specific variables, while holding every other value constant. The function
#'  runs multiple instances of `SurveySim` using values of `SurveyParameters` and replacing one of them with a sequence of values offered by the user.
#'
#' @param SurveyParameters list of parameters (object class `SurveySim`)
#'
#' @param LoopVariable variable to be looped. Can be any of the variables that exist in `SurveyParameters`
#'
#' `col.width` - vector of numbers with distances between STP rows
#'
#' `grid.type` - vector of strings with names of grid types
#'
#' `simulations` - vector of numbers with numbers of simulations
#'
#' `area` - **list** with 2 vectors, one for x and one for y of area (vectors MUST be same length)
#'
#' `site.density` - vector with numbers of site density OR **list** with 2 vectors, one with minimum site densities, one with maximum (vectors MUST be same length)
#'
#' `site.area` - vector with numbers of site areas OR **list** with 4 vectors, one with minimums, one with maximums, one with means, one with st.devs (vectors MUST be same length)
#'
#' `overlap` - vector of numbers with varying overlaps
#'
#' `obj.density` - vector with varying artifact density OR **list** with 2 vectors, one with minimum artifact densities, one with maximum (vectors MUST be same length)
#'
#' `obj.distribution` - vector of names of artifact distribution
#'
#' `survey.radius` vector with varying survey radii
#' @param LoopSequence object with varying values, as defined above
#' @param PlotResult which results from the summary table will be plotted:
#' `SurveysPerSim` - plots the total number of surveys created in each simulation
#'
#' `SitesFound` - plots the frequency of sites found
#'
#' `SitesFoundOnArtifacts` - plots the frequency of sites found based on surveys finding at least one artifact
#'
#' `ArtifactsPerSurvey` - plots the average number of artifacts found per survey in each simulation
#'
#' `SuccessRateIndex` - plots the success rate index (ratio of survey pits that found a site over total number of survey pits made)
#' @examples
#' #Loop the impact of increasing distances between survey rows
#' width.loop<-SurveyLoops(ParametersExample,"col.width",c(25,50,75,100,125,150),"SitesFound")
#'
#' #Loop the impact of different artifact distributions on surveys
#' distr.loop<-SurveyLoops(ParametersExample,"obj.distribution",c("uniform","linear","spherical","sinusoidal"),"SitesFoundOnArtifacts")
#'
#' @export

SurveyLoops<-function(SurveyParameters,LoopVariable,LoopSequence,PlotResult){

  #1.we do a series of tests to allow for partial text matching and checking for errors

  LOOPVARIABLE<-c("col.width","grid.type","simulations","area","site.density","site.area",
                  "overlap", "obj.density", "obj.distribution", "survey.radius")

  LoopVariable<-pmatch(LoopVariable,LOOPVARIABLE)


  if(is.na(LoopVariable)==TRUE){
    stop("ERROR: LoopVariable not valid. Chose a valid variable to loop.\n")
  }

  PLOTRESULT<-c("SurveysPerSim","SitesFound","SitesFoundOnArtifacts","ArtifactsPerSurvey","SuccessRateIndex")

  PlotResult<-pmatch(PlotResult,PLOTRESULT)


  if(is.na(PlotResult)==TRUE){
    stop("ERROR: PlotResult is not valid. Chose a valid variable to plot results.\n")
  }

  if(is.list(LoopSequence)==TRUE){
    same.size=TRUE
    for (a in 1:(length(LoopSequence)-1)){
      if(length(LoopSequence[[a]])!=length(LoopSequence[[a+1]])){
        same.size=TRUE
      }
    }
    if(same.size==FALSE){
      stop("ERROR: Vectors inside list in LoopSequence are not the same size.\n")
    }
  }

  if(LoopVariable==4 & is.list(LoopSequence)==FALSE){#if Area input is not a list
    stop("ERROR: LoopSequence for Area must be a list with 2 vectors (mins and maxs).\n")
  }

  #2. Here we start with creating the basics and results object

  if(is.list(LoopSequence)==TRUE){
    nloops<-length(LoopSequence[[1]])
  }else{
    nloops<-length(LoopSequence)
  }

  results<-list(SurveysPerSim = matrix(NA,nloops,1),
                SitesFound = matrix(NA,nloops,6),
                SitesFoundOnArtifacts = matrix(NA,nloops,6),
                ArtifactsPerSurvey = matrix(NA,nloops,6),
                SuccessRateIndex = matrix(NA,nloops,6))

  colnames(results[[1]])<-"N"
  colnames(results[[2]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[3]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[4]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")
  colnames(results[[5]])<-c("Mean","StDev","Min","Max","Quant2.5%","Quant97.5%")

  if(is.list(LoopSequence)==TRUE){
    row.names(results[[1]])<-LoopSequence[[1]]
    row.names(results[[2]])<-LoopSequence[[1]]
    row.names(results[[3]])<-LoopSequence[[1]]
    row.names(results[[4]])<-LoopSequence[[1]]
    row.names(results[[5]])<-LoopSequence[[1]]
  }else{
    row.names(results[[1]])<-LoopSequence
    row.names(results[[2]])<-LoopSequence
    row.names(results[[3]])<-LoopSequence
    row.names(results[[4]])<-LoopSequence
    row.names(results[[5]])<-LoopSequence
  }
  #3. We start the loop here, and get all the parameters we need for each run. Lots of ifs...

  tmp.SurveyParameters<-SurveyParameters
  for(a in 1:nloops){
    if(LoopVariable==1){ #col.width
      tmp.SurveyParameters[[1]]<-LoopSequence[a]
    }

    if(LoopVariable==2){ #grid.type
      tmp.SurveyParameters[[2]]<-LoopSequence[a]
    }

    if(LoopVariable==3){ #simulations
      tmp.SurveyParameters[[3]]<-LoopSequence[a]
    }

    if(LoopVariable==4){ #area
      tmp.SurveyParameters[[4]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
    }

    if(LoopVariable==5){ #site.density
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[5]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
      }else{
        tmp.SurveyParameters[[5]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==6){ #site.area
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[6]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a],LoopSequence[[3]][a],LoopSequence[[4]][a])
      }else{
        tmp.SurveyParameters[[6]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==7){ #overlap
      tmp.SurveyParameters[[7]]<-LoopSequence[7]
    }

    if(LoopVariable==8){ #obj.density
      if(is.list(LoopSequence)==TRUE){
        tmp.SurveyParameters[[8]]<-c(LoopSequence[[1]][a],LoopSequence[[2]][a])
      }else{
        tmp.SurveyParameters[[8]]<-LoopSequence[a]
      }
    }

    if(LoopVariable==9){ #obj.distribution
      tmp.SurveyParameters[[9]]<-LoopSequence[a]
    }

    if(LoopVariable==10){ #survey.radius
      tmp.SurveyParameters[[10]]<-LoopSequence[a]
    }

    #4.Here we call the SurveySim and run the loop iteration
    cat(paste("Iteration ",a," of ",nloops,".\n",sep=""))

    tmp.results<-SurveySim(tmp.SurveyParameters,plot=FALSE)

    results[[1]][a,1]<-tmp.results[[1]][1,1]
    results[[2]][a,]<-tmp.results[[1]][2,]
    results[[3]][a,]<-tmp.results[[1]][3,]
    results[[4]][a,]<-tmp.results[[1]][4,]
    results[[5]][a,]<-tmp.results[[1]][5,]
  }

  #5.Here we create the plot.
  #a. get some fancy colors

  colorsolid<-rainbow(10)[LoopVariable]
  coloralpha<-rainbow(10,alpha=0.3)[LoopVariable]

  #b.get the title names for variables and sequence to plot

  if(LoopVariable==1){ #col.width
    label = "Distance between survey columns"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Distance (m) in each iteration"
  }

  if(LoopVariable==2){ #grid.type
    label = "Grid Types"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Grid Types"
  }

  if(LoopVariable==3){ #simulations
    label = "Number of simulations"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Simulations in each iteration"
  }

  if(LoopVariable==4){ #area
    label = "Area Sizes"
    xmin<-1
    xmax<-length(LoopSequence[[1]])
    xlabels<-LoopSequence[[1]]*LoopSequence[[2]]
    xlab<-"area (km^2) in each iteration"

  }

  if(LoopVariable==5){ #site.density
    label = "Site Densities"
    if(is.list(LoopSequence)==TRUE){
      xmin<-1
      xmax<-length(LoopSequence[[1]])
      xlabels<-LoopSequence[[1]]*LoopSequence[[2]]/2
      xlab<-"Average site density in each iteration"
    }else{
      xmin<-1
      xmax<-length(LoopSequence)
      xlabels<-LoopSequence
      xlab<-"Site density in each iteration"
    }
  }

  if(LoopVariable==6){ #site.area
    label = "Site areas"
    if(is.list(LoopSequence)==TRUE){
      xmin<-1
      xmax<-length(LoopSequence[[1]])
      xlabels<-LoopSequence[[3]]
      xlab<-"Average site area in each iteration"
    }else{
      xmin<-1
      xmax<-length(LoopSequence)
      xlabels<-LoopSequence
      xlab<-"Site area in each iteration"
    }
  }

  if(LoopVariable==7){ #overlap
    label = "Site overlaps"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Maximum overlap permitted in each iteration"
  }

  if(LoopVariable==8){ #obj.density
    label = "Artifact densities"
    if(is.list(LoopSequence)==TRUE){
      xmin<-1
      xmax<-Length(LoopSequence[[1]])
      xlabels<-LoopSequence[[1]]*LoopSequence[[2]]/2
      xlab<-"Average artifact density in each iteration"
    }else{
      xmin<-1
      xmax<-length(LoopSequence)
      xlabels<-LoopSequence
      xlab<-"Artifact density in each iteration"
    }
  }

  if(LoopVariable==9){ #obj.distribution
    label = "Artifacts distribution"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Artifact distribution in each iteration"
  }

  if(LoopVariable==10){ #survey.radius
    label = "Survey radii"
    xmin<-1
    xmax<-length(LoopSequence)
    xlabels<-LoopSequence
    xlab<-"Survey radii in each iteration"
  }

  #c. get the y values
  ymin=0

  if(PlotResult==1){ #"SurveysPerSim"
    ymax<-max(results[[1]])
    ylab<-"Number of Surveys"
  }

  if(PlotResult==2){ #"SitesFound"
    ymax<-max(results[[2]])
    ylab<-"Frequency of sites found"
  }

  if(PlotResult==3){ #"SitesFoundOnArtifacts"
    ymax<-max(results[[3]])
    ylab<-"Frquency of sites found by artifacts"
  }

  if(PlotResult==4){ #"ArtifactsPerSurvey"
    ymax<-max(results[[4]])
    ylab<-"Average number of artifacts"
  }

  if(PlotResult==5){ #"SuccessRateIndex"
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

  if(PlotResult!=1){
    polygon(c(1:xmax,xmax:1),
            c(results[[PlotResult]][,3],results[[PlotResult]][xmax:1,4]),
            col=coloralpha,border = NA)

    polygon(c(1:xmax,xmax:1),
            c(results[[PlotResult]][,1]-results[[PlotResult]][,2],results[[PlotResult]][xmax:1,1]+results[[PlotResult]][xmax:1,2]),
            col=coloralpha,border=NA)

    lines(1:xmax,results[[PlotResult]][,1]-results[[PlotResult]][,2],
          col=colorsolid,lty=2,lwd=1.5)

    lines(1:xmax,results[[PlotResult]][,1]+results[[PlotResult]][,2],
          col=colorsolid,lty=2,lwd=1.5)

  }

  lines(1:xmax,results[[PlotResult]][,1],
        col=colorsolid,lwd=2)

  return(results)

}
