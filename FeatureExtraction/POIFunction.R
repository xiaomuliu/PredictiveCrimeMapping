# Crime incident to nearest point of interest distance
Crime2POIdist <- function(CrimeData,Shpfile,Attr,YearList=2001:2014,MonthList=1:12){
  CrimeData[,Attr] <- rep(0,nrow(CrimeData))
  prj <- proj4string(Shpfile)
  # Doing this in loops to prevent from producing a large distance matrix
  for (i in YearList){
    for (j in MonthList){
      CrimeData.sub <- subset(CrimeData,YEAR==i&MONTH==j,select=c("DATEOCC","X_COORD","Y_COORD","INC_CNT"))
      subIdx <- which(CrimeData$YEAR==i&CrimeData$MONTH==j)
      CrimeSub.sp <- SpatialPoints(coords=CrimeData.sub[,c("X_COORD","Y_COORD")])
      proj4string(CrimeSub.sp) <- prj
      
      DistMat <- gDistance(CrimeSub.sp, Shpfile, byid=TRUE)
      
      MinDist <- apply(DistMat, 2, min)
      
      CrimeData[,Attr][subIdx] <- MinDist
    }
  }
  
  return(CrimeData)
}

# Point of inteest nearest pair-wise distance
POIpairDist <- function(Shpfile){
#   POI.coords <- Shpfile@coords
#   POI.DistMat <- as.matrix(dist(POI.coords))
#   diag(POI.DistMat) <- max(POI.DistMat) #set a big value on the diagonal elements so that we can use min function on each row
#   POI.minDist <- apply(POI.DistMat,2,min)
#   return(POI.minDist)
  
  DistMat <- gDistance(Shpfile, Shpfile, byid=TRUE)  
  DistSorted <- apply(DistMat, 2, sort)
  MinDist <- DistSorted[2,]
  return(MinDist)
} 

# Grid to nearest point of interest distance
Grd2POIdist <- function(Grid,Shpfile,Attr,prj=proj4string(Shpfile)){
  Grid[,Attr] <- rep(0,nrow(Grid))

  Grid.sp <- SpatialPoints(coords=Grid[,c("X_COORD","Y_COORD")])
  proj4string(Grid.sp) <- prj
      
  DistMat <- gDistance(Grid.sp, Shpfile, byid=TRUE)      
  MinDist <- apply(DistMat, 2, min)    
  Grid[,Attr] <- MinDist
  
  return(Grid)
}


BuildingDen <- function(Data,Shpfile,extension,Attr="BldgDen"){
  N <- nrow(Data)
  Data[,Attr] <- rep(0,N)
  
  for (i in 1:N){
    grdLoc <- Data[i,]
    within_idx <- with(Shpfile@data, X_COORD>=grdLoc$X_COORD-extension[1] & X_COORD<=grdLoc$X_COORD+extension[1]
                       & Y_COORD>=grdLoc$Y_COORD-extension[2] & Y_COORD<=grdLoc$Y_COORD+extension[2])
    Data[i,Attr] <- sum(within_idx)
  }
  
  return(Data)
}


StreetDen <- function(Data,Shpfile,extension,Attr="StrDen",prj=proj4string(Shpfile)){
  N <- nrow(Data)
  Data[,Attr] <- rep(0,N)
  
  for (i in 1:N){
    grdLoc <- Data[i,]
    rectRegion <- with(grdLoc,cbind(c(X_COORD-extension[1],X_COORD-extension[1],X_COORD+extension[1],
                                      X_COORD+extension[1],X_COORD-extension[1]),
                                    c(Y_COORD-extension[2],Y_COORD+extension[2],Y_COORD+extension[2],
                                      Y_COORD-extension[2],Y_COORD-extension[2])))
    rectRegion <- Polygons(list(Polygon(rectRegion)),"rect")
    rectRegion <- SpatialPolygons(list(rectRegion),proj4string=CRS(prj))
    
    line.inRect <- gIntersects(Shpfile,rectRegion,byid=TRUE)
    Data[i,Attr] <- sum(line.inRect)
  }
  
  return(Data)
}

BuildingType <- function(Data,Shpfile,extension,Attr,BldgType,fun=mean,...){
  N <- nrow(Data)
  Data[,Attr] <- rep(0,N)
  
  for (i in 1:N){
    grdLoc <- Data[i,]
    within_idx <- with(Shpfile@data, X_COORD>=grdLoc$X_COORD-extension[1] & X_COORD<=grdLoc$X_COORD+extension[1]
                       & Y_COORD>=grdLoc$Y_COORD-extension[2] & Y_COORD<=grdLoc$Y_COORD+extension[2])
    if(sum(within_idx)==0){
      Data[i,Attr] <- 0 # no building within the selected region
    }else{
      Data[i,Attr] <- fun(Shpfile@data[within_idx,BldgType],...)
    }    
  }
  
  return(Data)
}

ZoneType <- function(Grid,Shpfile,Attr="ZoneType",prj=proj4string(Shpfile)){
  Grid[,Attr] <- rep(0,nrow(Grid))
  
  Grid.sp <- SpatialPoints(coords=Grid[,c("X_COORD","Y_COORD")])
  proj4string(Grid.sp) <- prj
  
  inWhichPoly <- over(Grid.sp,as(Shpfile,"SpatialPolygons"))      
  Grid[,Attr] <- as.factor(Shpfile@data$ZONE_TYPE[inWhichPoly])
  
  return(Grid)
}


PtsDen <- function(Data,Shpfile,extension,Attr,prj=proj4string(Shpfile)){
  N <- nrow(Data)
  Data[,Attr] <- rep(0,N)
  
  for (i in 1:N){
    grdLoc <- Data[i,]
    rectRegion <- with(grdLoc,cbind(c(X_COORD-extension[1],X_COORD-extension[1],X_COORD+extension[1],
                                      X_COORD+extension[1],X_COORD-extension[1]),
                                    c(Y_COORD-extension[2],Y_COORD+extension[2],Y_COORD+extension[2],
                                      Y_COORD-extension[2],Y_COORD-extension[2])))
    rectRegion <- Polygons(list(Polygon(rectRegion)),"rect")
    rectRegion <- SpatialPolygons(list(rectRegion),proj4string=CRS(prj))
    
    point.inRect <- gWithin(Shpfile,rectRegion,byid=TRUE)
    Data[i,Attr] <- sum(point.inRect)
  }
  
  return(Data)
}

GrdinBndy <- function(Grid,Shpfile,Attr,ShpfileAttr=NA,prj=proj4string(Shpfile)){
  Grid[,Attr] <- rep(0,nrow(Grid))
  
  Grid.sp <- SpatialPoints(coords=Grid[,c("X_COORD","Y_COORD")])
  proj4string(Grid.sp) <- prj
  
  inWhichBndy <- over(Grid.sp, as(Shpfile,"SpatialPolygons"))
  
  if (any(is.na(ShpfileAttr))){
    isInBndy <- !is.na(inWhichBndy)  
    Grid[,Attr] <- isInBndy
  }else{
    Grid[,Attr] <- Shpfile@data[inWhichBndy,ShpfileAttr]
  }

  return(Grid)
}
