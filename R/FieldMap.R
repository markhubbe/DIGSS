#' Field Map
#'
#' Creates randomly placed ellipsoid sites in a rectangular field.
#'
#' @details `FieldMap` creates and plots randomly placed ellipses representing
#' archaeological sites. The sites are created inside a user-defined rectangle, with random positions
#' and random rotations. It allows also to control how of overlap between sites.
#'
#' @param  Area vector with horizontal and vertical size `(hor,ver)` of area surveyed in km.
#' @param  site.density number of sites/km^2. Can be one constant value or vector with two values `(min, max)` to
#' create a range of densities between simulations.
#' @param site.area either:
#'
#' One values with uniform area for all sites, or
#'
#' Vector with 4 values `(min, max, mean, st dev)`, to create variable areas. Areas in this case
#' are normally distributed based on mean and stdev, but within the range of min and max.
#' @param overlap proportion of overlap possible between sites: from (0 = no overlap allowed to 1 = sites can occupy same space)
#' @param areaprecision value passed to `AreaEstimator`. Defines precision of area calculation. Default value (`1000`), returns area within 0.1% of real area occupied by sites
#'
#' @examples
#' #example of map with 8 sites or variable areas and partial overlap
#' field.example<-FieldMap(Area=c(1,1),site.density=8,site.area=c(50000,250000,150000,50000),overlap=0.5,plot=TRUE)
#'
#' @export

FieldMap<-function(Area,site.density,site.area,overlap=0.50,plot=FALSE,areaprecision=1000){

  #1. First we create the data.frame that will store the information for all sites.
  if(length(site.density)>1){
    nsites=ceiling(sample(site.density[1]:site.density[2],1)*Area[1]*Area[2])
  }else{
    nsites<-ceiling(site.density*Area[1]*Area[2])
  }

  #this will bring the site area to km2

  site.area<-site.area/1e6

  site.frame<-matrix(0,nsites,8)

  colnames(site.frame)=c("Site","Area","Eccentricity","Angle","center.x","center.y","ellipse.a","ellipse.b")

  #We will create a seed of random positions and at each loop we will

  temp.x<-runif(nsites,0,Area[1])
  temp.y<-runif(nsites,0,Area[2])

  #then we check if sites overlap. If they do, we move them around.

  #2.We fill the data.frame
  for(a in 1:nsites){
    #site number in col 1
    site.frame[a,1]<-a

    #site area in col 2. Depends on input (uniform areas or areas normally distributed)
    if(length(site.area)>1){
      while(site.frame[a,2]<site.area[1]||site.frame[a,2]>site.area[2]){
        site.frame[a,2]<-rnorm(1,site.area[3],site.area[4])
      }
    }else{
      site.frame[a,2]<-site.area
    }
    #eccentricity in col 3. Arbitrarily limited to 0.85 at this point. 0=circle, 1=line
    site.frame[a,3]<-runif(1,0,0.85)# eccentricity. Limit to 0.85, to get sites not too squashed. I have no math reason why 0.85 though.

    #inclination in col 4
    site.frame[a,4]<-runif(1,0,pi)

    #Site center coords in cols 5 and 6
    site.frame[a,5]<-temp.x[a]
    site.frame[a,6]<-temp.y[a]

    #Long and short axis length in cols 7 and 8
    site.frame[a,7]<-(site.frame[a,2]/(pi*(1-site.frame[a,3]^2)^0.5))^0.5
    site.frame[a,8]<-((site.frame[a,2]*(1-site.frame[a,3]^2)^0.5)/pi)^0.5

    #Here is were we remove from the seed vector all points within the ellipsis defined in the loop.
    no.overlaps=FALSE
    cycle.counter=0

    while(no.overlaps==FALSE & cycle.counter<=1000){
      if(a!=1){
        #we calculate here which sites' major axis are

        #this is all from Cara's equations. We calculate the distance of center to edge for eache ellipse and then add them up
        #t=atan(g/f)
        angle.a2others<-atan((site.frame[1:(a-1),6]-site.frame[a,6])/(site.frame[1:(a-1),5]-site.frame[a,5]))


        #d<-sqrt(1/((cos(t-k)^2/a^2+sin(t-k))^2/b^2))

        tmpdist.a2others<-sqrt(1/(((cos(angle.a2others-site.frame[a,4])^2/site.frame[a,7]^2)
                                   +(sin(angle.a2others-site.frame[a,4]))^2/site.frame[a,8]^2)))

        angle.others2a<-atan((site.frame[a,6]-site.frame[1:(a-1),6])/(site.frame[a,5]-site.frame[1:(a-1),5]))

        tmpdist.others2a<-sqrt(1/(((cos(angle.a2others-site.frame[1:(a-1),4])^2/site.frame[1:(a-1),7]^2)
                                   +(sin(angle.a2others-site.frame[1:(a-1),4]))^2/site.frame[1:(a-1),8]^2)))

        tmp.distances<-((site.frame[a,5]-site.frame[1:(a-1),5])^2+(site.frame[a,6]-site.frame[1:(a-1),6])^2)^0.5

        tmp.maxdistances<-(tmpdist.others2a+tmpdist.a2others)*(1-overlap)

        overlap.index<-tmp.distances<=tmp.maxdistances

        if(sum(overlap.index)>0){
          site.frame[a,5]<-runif(1,0,Area[1])
          site.frame[a,6]<-runif(1,0,Area[2])
          cycle.counter<-cycle.counter+1
        }else{
          no.overlaps=TRUE
        }

      }else{
        no.overlaps=TRUE
      }
    }

    if(cycle.counter>1000){
      warning(paste("Could not find a random position for site",a,"after 1000 tries"))
    }

    sites.created<-a

  }


  #3. Here we plot, if plot = TRUE

  if(plot==TRUE){

    plot.new()
    plot.window(c(0,Area[1]),c(0,Area[2]),asp=Area[1]/Area[2])

    axis(1)
    axis(2)
    box()

    for(a in 1:sites.created){
      text(site.frame[a,5],site.frame[a,6],site.frame[a,1],pos=3,cex=0.5)
      points(site.frame[a,5],site.frame[a,6],pch=16)

      #1.Get the angles, x and y to plot the ellypses
      angles<-seq(0,2*pi,length=72)

      #x= h + a cos(t)*cos(c)-b*sin(t)*sin(c)
      xcoords<- site.frame[a,5]+site.frame[a,7]*cos(angles)*cos(site.frame[a,4])-site.frame[a,8]*sin(angles)*sin(site.frame[a,4])
      #y= k + b sin(t)*cos(c)+a*cos(t)*sin(c)
      ycoords<- site.frame[a,6]+site.frame[a,8]*sin(angles)*cos(site.frame[a,4])+site.frame[a,7]*cos(angles)*sin(site.frame[a,4])

      polygon(xcoords,ycoords,col=rgb(0,0,1,0.5),border=NA)

    }

  }

  #4.We get some of the overall statistics (Sites created, Total Site Area, accurate area of sites) and put them is a list

  results<-list("site.frame"=site.frame,"TotalArea"=NA,"ActualArea"=NA)

  results$TotalArea<-sum(site.frame[,2])

  #5. Here we get the projected are using AreaEstimator function
  results$ActualArea<-AreaEstimator(site.frame,Area,areaprecision)


  return(results)


}
