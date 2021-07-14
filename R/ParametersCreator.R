#'Parameters creator
#'
#'Creates the List of Parameters for SurveySim
#'
#'@details
#'This function will create a user text interface to define all the arguments required
#'by the SurveySim function. It generates a list of class `SurveySim()`.
#'All parameters are loaded inside the function.
#'
#'@param col.width the space between columns in the grid IN METERS
#'@param grid.type options are: "square","rectangle","staggered","hexagonal","arbitrary.staggered", following Kintigh 1988
#'@param simulations number of random maps to be created and contrasted with the grids
#'@param Area vector with horizontal and vertical size of area surveyed in km
#'OBS: Sites will all be ellipses with radii not too different and random angles
#'@param SiteDensity measured as number of sites/km^2. Can be either one value or a vector with 2 values (min and max) to create a range of densities
#'@param site.area can be one of two options: 1. one value indicating the area of all sites, in meter^2;
#' or 2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2
#'@param overlap maximum overlap of site area, ranging from 0 = no overlap allowed to 1 = complete overlap possible
#'@param obj.density artifacts per m^2. Can be a single value (uniform for all sites) or a range of values defined as min and max
#'@param obj.distribution type of cloud distribution for artifacts inside sites. Choose from: 'uniform', 'linear', 'spherical', 'sinusoidal'
#'@param survey.radius the radius of the survey pit (assumed to be a circle)
#'@references
#'Kintigh (1988) The Effectiveness of Subsurface Testing: A Simulation Approach.
#'American Antiquity, 53:686-707.
#'@examples
#'ParametersCreator()

#'@export
ParametersCreator<-function(){

  #1.Start checking that the list exists. If it does not, create one
  if(exists("SurveyParameters")==FALSE){
    cat("SurveyParameters not found.\nPlease rename Parameters list to SurveyParameters to edit it or create new one.")
    Answer<-readline("Do you want to create a new list? y/n:")
    if(Answer == "y" | Answer == "Y"){

      SurveyParameters<-list("col.width"=NA,"grid.type"=NA,"simulations"=NA,"area"=NA,
                             "site.density"=NA,"site.area"=NA,"overlap"=NA,
                             "obj.density"=NA,"obj.distribution"=NA,"survey.radius"=NA)
      class(SurveyParameters)<-"SurveySim"
      assign("SurveyParameters",SurveyParameters,globalenv())
      listExists=TRUE
    }else{
      cat("Parameter list creation aborted.")

    }
  }else{
    listExists=TRUE
  }
  #2. Give the user a list of options to edit
  if(listExists==TRUE){
    continue=TRUE
    while(continue==TRUE){
      cat("\nLIST OF OPTIONS:\nall - Edit all\n1.Edit col.width       2.Edit grid.type        3.Edit simulations\n4.Edit area            5.Edit site.density     6.Edit site.area\n7.Edit overlap         8.Edit obj.density    9.Edit obj.distrib    \n10.Edit survey.radius\nend - Finish editing   help-Print definition of variables")
      Answer = readline("Select an option:")

      #this finishes the loop
      if(substr(Answer,1,1)=="e"){
        continue=FALSE
        return(cat("Editing completed"))
      }

      #this will print the help (definitions of variables)
      if(substr(Answer,1,1)=="h"){
        cat(paste("HELP - Definitions of variables:",
                  "The list SurveyParameters is  created with the following variables:",
                  "col.width<- the space between columns in the grid IN METERS.",
                  "grid.type<- options are: 'square','rectangle','staggered','hexagonal,'arbitrary.staggered', following Kintigh 1988",
                  "simulations<- number of random map to be created and contrasted with the grids.",
                  "area: vector with horizontal and vertical size of area surveyed in km.",
                  "site.density: measures as number of sites/km2 can be either one value or a vector with 2 values (min and max) to create a range of densities.",
                  "site.area: it will be one of two options:",
                  "  1. one value indicating the area of all sites, in meter^2 ",
                  "  2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2. ",
                  "  OBS: Sites will all be ellipses with radii not too different and random angles.",
                  "overlap: maximum overlap of site area, ranging from 0 as no overlap to 1 as complete overlap.",
                  "obj.density = artifcats per m^2 in sites. Can be a range of values, defined as min and max",
                  "obj.distribution = type of cloud distribution for objects inside sites. Choose from:",
                  "  'uniform'<-artifacts are uniformily distributed within site",
                  "  'linear'<-artifcats decline  linearly from center of site",
                  "  'spherical'<-artifacts decline form center in a ellipsoid distribution",
                  "  'sinusoidal'<-artifacts decline form center in a sinusoidal  distribution",
                  "survey.radius = the radius of the survey pit (assumed to be a circle for now)\n\n",sep="\n"))
      }

      #Here we create a vector of booleans to see which items will be changed
      Items<-rep(FALSE,10)

      #if "all" is selected, we will adit the whole list
      if(substr(Answer,1,1)=="a"){
        Items[1:length(Items)]=TRUE
        continue=FALSE
      }

      #else, we will just edit the element selected
      if(is.na(suppressWarnings(as.numeric(Answer)))==FALSE){
        Items<-rep(FALSE,10)
        Items[as.numeric(Answer)]=TRUE
      }

      #3.Here we start the long list of changing the values in each item of the list.
      #the process is slow, because in some cases the format of item in the list may vary.

      if(Items[1]==TRUE){
        Item1=TRUE
        cat("\nINPUT THE NEW VALUE FOR COL.WIDTH\n")
        cat("col.width is the space between columns in the grid IN METERS.\n")

        while(Item1==TRUE){
          cat(paste("Current value is:",get("SurveyParameters",envir=.GlobalEnv)$col.width))
          col.width<-readline("New col.width value:")
          if(is.na(suppressWarnings(as.numeric(col.width)))==FALSE){
            col.width<-as.numeric(col.width)
            SurveyParameters$col.width<<-col.width
            Item1=FALSE
          }else{
            cat("\nERROR: col.width must be numeric!\n")
          }
        }
      }

      if(Items[2]==TRUE){
        Item2=TRUE
        cat("\nSELECT THE GRID TYPE:\n")
        cat("1. Square\n2. Rectangle\n3. Staggered\n4. Hexagonal\n5. Arbitrary staggered\n")

        while(Item2==TRUE){
          cat(paste("Current grid type is:",get("SurveyParameters",envir=.GlobalEnv)$grid.type))
          grid.type<-readline("New grid.type value:")
          if(is.na(suppressWarnings(as.numeric(grid.type)))==FALSE){
            if(as.numeric(grid.type)==1){grid.typelbl="square"}
            else if(as.numeric(grid.type)==2){grid.typelbl="rectangle"}
            else if(as.numeric(grid.type)==3){grid.typelbl="staggered"}
            else if(as.numeric(grid.type)==4){grid.typelbl="hexagonal"}
            else if(as.numeric(grid.type)==5){grid.typelbl="arbitrary.staggered"}
            else{cat("\nERROR: Select a valid grid type number\n")}


            if(as.numeric(grid.type)>=1&as.numeric(grid.type)<=5){
              SurveyParameters$grid.type<<-grid.typelbl
              Item2=FALSE
            }
          }else{
            cat("\nERROR: Select a grid type by its number\n")
          }
        }
      }

      if(Items[3]==TRUE){
        Item3=TRUE
        cat("\nINPUT THE NUMBER OF SIMULATIONS TO BE RUN\n")
        cat("simulations is number of random maps to be created and contrasted with the grids.\n")

        while(Item3==TRUE){
          cat(paste("Current value is:",get("SurveyParameters",envir=.GlobalEnv)$simulations))
          sims<-readline("New simulations value:")
          if(is.na(suppressWarnings(as.numeric(sims)))==FALSE){
            sims<-as.numeric(sims)
            SurveyParameters$simulations<<-sims
            Item3=FALSE
          }else{
            cat("\nERROR: simulations must be numeric!\n")
          }
        }
      }

      if(Items[4]==TRUE){
        Item4a=TRUE
        Item4b=TRUE
        cat("\nINPUT THE NEW VALUE FOR AREA\n")
        cat("Area is vector with horizontal and vertical size of area surveyed in km.\n")

        while(Item4a==TRUE | Item4b==TRUE){
          cat("Current value is:\n")
          print(get("SurveyParameters",envir=.GlobalEnv)$area)
          area<-matrix(0,1,2)
          colnames(area)<-c("area.x","area.y")
          row.names(area)<-"km"
          area.x<-readline("New value for x:")
          if(is.na(suppressWarnings(as.numeric(area.x)))==FALSE){
            area[1,1]<-as.numeric(area.x)
            Item4a=FALSE
          }else{
            cat("\nERROR: x value of area  must be numeric!\n")
            Item4a=TRUE
          }
          area.y<-readline("New value for y:")
          if(is.na(suppressWarnings(as.numeric(area.y)))==FALSE){
            area[1,2]<-as.numeric(area.y)
            Item4b=FALSE
          }else{
            cat("\nERROR: y value of area  must be numeric!\n")
            Item4b=TRUE
          }
          if(Item4a==FALSE &Item4b==FALSE){
            SurveyParameters$area<<-area
          }


        }
      }

      if(Items[5]==TRUE){
        Item5=TRUE
        cat("\nINPUT THE NEW VALUE FOR SITE DENSITY\n")
        cat("site.density is the  number of sites/km2. It can be either one value or a vector with 2 values (min and max) to create a range of densities.\n")

        while(Item5==TRUE){
          cat("Current value is:\n")
          print(get("SurveyParameters",envir=.GlobalEnv)$site.density)
          cat("\nSELECT AN OPTION:\n1. Uniform site density\n2.Site density range")
          density.option<-readline("Make selection: ")
          if(density.option==1){
            site.density<-readline("New site.density value:")
            if(is.na(suppressWarnings(as.numeric(site.density)))==FALSE){
              site.density<-as.numeric(site.density)
              SurveyParameters$site.density<<-site.density
              Item5=FALSE
            }else{
              cat("\nERROR: site.density must be numeric!\n")
            }

          }else if(density.option==2){
            site.density<-rep(NA,2)
            site.density[1]<-readline("New MIN VALUE for site.density:")
            site.density[2]<-readline("New MAX VALUE for site.density:")
            if(is.na(suppressWarnings(as.numeric(site.density[1])))==FALSE&is.na(suppressWarnings(as.numeric(site.density[2])))==FALSE){
              site.density<-as.numeric(site.density)
              SurveyParameters$site.density<<-site.density
              Item5=FALSE
            }else{
              cat("\nERROR: One or more site.density values is not numeric!\n")
            }
          }else{
            cat("\nERROR: Choose a valid option.\n")
          }

        }
      }

      if(Items[6]==TRUE){
        Item6=TRUE
        cat("\nINPUT THE NEW VALUE FOR SITE AREA\n")
        cat("site.area: it will be one of two options:\n1. one value indicating the area of all sites, in meter^2\n2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2.\n")

        while(Item6==TRUE){
          cat("Current value is:\n")
          print(get("SurveyParameters",envir=.GlobalEnv)$site.area)
          cat("\nSELECT AN OPTION:\n1. Uniform site area\n2.Site area range")
          area.option<-readline("Make selection: ")
          if(area.option==1){
            site.area<-readline("New site.area value:")
            if(is.na(suppressWarnings(as.numeric(site.area)))==FALSE){
              site.area<-as.numeric(site.area)
              SurveyParameters$site.area<<-site.area
              Item6=FALSE
            }else{
              cat("\nERROR: site.area must be numeric!\n")
            }

          }else if(area.option==2){
            site.area<-matrix(NA,1,4)
            row.names(site.area)="area values"
            colnames(site.area)=c("Min","Max","Mean","St.Dev")
            area1<-readline("New MIN VALUE for site.area:")
            area2<-readline("New MAX VALUE for site.area:")
            area3<-readline("New MEAN/MEDIAN VALUE for site.area:")
            area4<-readline("New ST DEV VALUE for site.area:")
            if(is.na(suppressWarnings(as.numeric(area1)))==FALSE&is.na(suppressWarnings(as.numeric(area2)))==FALSE&is.na(suppressWarnings(as.numeric(area3)))==FALSE&is.na(suppressWarnings(as.numeric(area4)))==FALSE){
              site.area[1,1]<-as.numeric(area1)
              site.area[1,2]<-as.numeric(area2)
              site.area[1,3]<-as.numeric(area3)
              site.area[1,4]<-as.numeric(area4)
              SurveyParameters$site.area<<-site.area
              Item6=FALSE
            }else{
              cat("\nERROR: One or more site.area values is not numeric!\n")
            }
          }else{
            cat("\nERROR: Choose a valid option.\n")
          }

        }
      }

      if(Items[7]==TRUE){
        Item7=TRUE
        cat("\nINPUT THE NEW VALUE FOR SITE OVERLAP\n")
        cat("overlap is the maximum possible overlap between sites, ranging from 0 as no overlap to 1 as complete overlap.\n")

        while(Item7==TRUE){
          cat(paste("Current value is:",get("SurveyParameters",envir=.GlobalEnv)$overlap))
          overlap<-readline("New overlap value:")
          if(is.na(suppressWarnings(as.numeric(overlap)))==FALSE){
            overlap<-as.numeric(overlap)
            if(overlap>=0&overlap<=1){
              SurveyParameters$overlap<<-overlap
              Item7=FALSE
            }else{
              cat("\nERROR: overlap must be between 0 and 1!\n")
            }

          }else{
            cat("\nERROR: overlap must be numeric!\n")
          }
        }
      }

      if(Items[8]==TRUE){
        Item8=TRUE
        cat("\nINPUT THE NEW VALUE FOR OBJECT DENSITY\n")
        cat("Object density: it will be one of two options:\n1. one value indicating the area of all sites, in meter^2\n2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2.\n")

        while(Item8==TRUE){
          cat("Current value is:\n")
          print(get("SurveyParameters",envir=.GlobalEnv)$obj.density)
          cat("\nSELECT AN OPTION:\n1. Uniform artifact density\n2.Artifact density range")
          density.option<-readline("Make selection: ")
          if(density.option==1){
            obj.density<-readline("New object.density value:")
            if(is.na(suppressWarnings(as.numeric(obj.density)))==FALSE){
              obj.density<-as.numeric(obj.density)
              SurveyParameters$obj.density<<-obj.density
              Item8=FALSE
            }else{
              cat("\nERROR: object.density must be numeric!\n")
            }

          }else if(density.option==2){
            obj.density<-rep(NA,2)
            obj.density[1]<-readline("New MIN VALUE for object.density:")
            obj.density[2]<-readline("New MAX VALUE for object.density:")
            if(is.na(suppressWarnings(as.numeric(obj.density[1])))==FALSE&is.na(suppressWarnings(as.numeric(obj.density[2])))==FALSE){
              obj.density<-as.numeric(obj.density)
              SurveyParameters$obj.density<<-obj.density
              Item8=FALSE
            }else{
              cat("\nERROR: One or more object.density values is not numeric!\n")
            }
          }else{
            cat("\nERROR: Choose a valid option.\n")
          }

        }
      }

      if(Items[9]==TRUE){
        Item9=TRUE
        cat("\nINPUT THE NEW VALUE FOR ARTIFACT DISTRIBUTION\n")
        cat("obj.distribution = type of cloud distribution for artifacts inside sites.\n")

        while(Item9==TRUE){
          cat(paste("Current distribution type is:",get("SurveyParameters",envir=.GlobalEnv)$obj.distribution))
          cat("\nSELECT AN OPTION:\n1. Uniform distribution\n2. Linear distribution\n3. Spherical distribution\n4. Sinusoidal distribution")
          obj.distribution<-readline("New artifact distribution type:")
          if(is.na(suppressWarnings(as.numeric(obj.distribution)))==FALSE){
            if(as.numeric(obj.distribution)==1){obj.distlbl="uniform"}
            else if(as.numeric(obj.distribution)==2){obj.distlbl="linear"}
            else if(as.numeric(obj.distribution)==3){obj.distlbl="spherical"}
            else if(as.numeric(obj.distribution)==4){obj.distlbl="sinusoidal"}
            else{cat("\nERROR: Select a valid distribution type\n")}


            if(as.numeric(obj.distribution)>=1&as.numeric(obj.distribution)<=4){
              SurveyParameters$obj.distribution<<-obj.distlbl
              Item9=FALSE
            }
          }else{
            cat("\nERROR: Select an artifact distribution by its number\n")
          }
        }

      }

      if(Items[10]==TRUE){
        Item10=TRUE
        cat("\nINPUT THE NEW VALUE FOR SURVEY PIT RADIUS\n")
        cat("survey.radius = the radius IN METERS of the survey pit (assumed to be a circle for now)\n")

        while(Item10==TRUE){
          cat(paste("Current value is:",get("SurveyParameters",envir=.GlobalEnv)$survey.radius, "m.\n"))
          if(is.na(get("SurveyParameters",envir=.GlobalEnv)$survey.radius)==FALSE){
            cat(paste("This represents an area of:",round(get("SurveyParameters",envir=.GlobalEnv)$survey.radius^2*pi,2), "m^2.\n"))
          }
          survey.radius<-readline("New survey.radius value:")
          if(is.na(suppressWarnings(as.numeric(survey.radius)))==FALSE){
            survey.radius<-as.numeric(survey.radius)
            SurveyParameters$survey.radius<<-survey.radius
            Item10=FALSE
          }else{
            cat("\nERROR: survey.radius must be numeric!\n")
          }
        }
      }


    }
  }
}
