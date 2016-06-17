#' Cacluating hit rate
#' 
#' @param Pred a numeric vector of prediction scores.
#' @param prob a numeric scalar or vector indicating the quantile for thresholding.
#' @param CrimeActualPts a data frame containing crime incident locations
#' @param r raster layer object
#' @param isInPoly a logical vector incidicating whether a location is inside the polygon boundary  
#' @return hit rate

HitRate2 <- function(Pred,prob,CrimeActualPts,groupSeq,r,isInPoly,subRegion=rep(TRUE,sum(isInPoly==TRUE)) ){
  Ngroups <- length(groupSeq)
  regionSize <- sum(subRegion==TRUE)
  
  try(if(length(Pred) != Ngroups*regionSize) stop("Dimensions mismatch!"))
  
  threshold <- quantile(Pred,probs=prob)
  selPix <- Pred>=threshold
  ActualCnt <- rep(0,length(Pred))
  
  for (i in 1:Ngroups){
    d <- groupSeq[i]
    subGroup <- subset(CrimeActualPts,GROUP==d,select=c("X_COORD","Y_COORD","INC_CNT"))
    subGroup.raster <- rasterize(subGroup[,c("X_COORD","Y_COORD")], r, subGroup$INC_CNT, fun=sum)
    
    subGroupRaster.df <- as.data.frame(subGroup.raster,xy=TRUE)
    names(subGroupRaster.df) <- c("X_COORD","Y_COORD","VALUE")
    subGroupRaster.df <- subGroupRaster.df[isInPoly,]
    subGroupRaster.df$VALUE[is.na(subGroupRaster.df$VALUE)] <- 0
    ActualCnt[((i-1)*regionSize+1):(i*regionSize)] <- subGroupRaster.df$VALUE[subRegion]
  }
  HitRate <- sum(ActualCnt[selPix])/sum(ActualCnt)
  
  return(HitRate)
}

# Hit rate (TPR) is not calcuated for every time interval but instead for all test examples. The "output" of hotspot models should be 
# normalized to have the summation of 1 for each time interval if appying this evaluation
ROCeval2 <- function(CrimeData,groupSeq,Grd,percentArea=seq(0,1,length.out=21),Pred=NULL,subRegion=rep(TRUE,nrow(Grd)),
                     hotspot=FALSE,Grd.full=NULL,window.t=0,kernel.x=NULL,kernel.y=NULL,period=NULL,r=NULL,isInCity=NULL,prj=NULL){
  Ngrids <- nrow(Grd)
  Ngroups <- length(groupSeq)
  TPR <- rep(0,length=length(percentArea))
  regionSize <- sum(subRegion==TRUE)
  PredVal <- rep(0,length=regionSize*Ngroups)
  
  for (i in 1:Ngroups){
    d <- groupSeq[i]
    
    if (hotspot){
      if (period>window.t){
        # long-term
        CrimeHistPts <- subset(CrimeData,GROUP>=d-period & GROUP<=d-window.t-1,select=c("X_COORD","Y_COORD","INC_CNT"))
      }else{
        #short-term
        CrimeHistPts <- subset(CrimeData,GROUP>=d-period & GROUP<=d-1,select=c("X_COORD","Y_COORD","INC_CNT"))
      }
      
      HotSpot <- SpKernSmCrime(CrimeHistPts,Grd.full,r,kernel.x,kernel.y,isInCity,prj)
      HS <- HotSpot$KernSm.df_inPoly[subRegion,]
      PredVal[((i-1)*regionSize+1):(i*regionSize)] <- HS$KS_VAL/sum(HS$KS_VAL)
    }else{
      PredVal[((i-1)*regionSize+1):(i*regionSize)] <- Pred[((i-1)*Ngrids+1):(i*Ngrids)][subRegion]
    }
  }
  
  CrimeActualPts <- subset(CrimeData,GROUP>=groupSeq[1] & GROUP<=groupSeq[Ngroups],select=c("GROUP","X_COORD","Y_COORD","INC_CNT"))
  
  for (p in 1:length(percentArea)){ 
    TPR[p] <- HitRate2(PredVal,1-percentArea[p],CrimeActualPts,groupSeq,r,isInCity,subRegion=subRegion)
  }
  
  AUC <- integrate.xy(percentArea, TPR, use.spline=TRUE)
  
  return(list(TPR=TPR,AUC=AUC))
}