#' Cacluating hit rate
#' 
#' @param Pred a numeric vector of prediction scores.
#' @param prob a numeric scalar or vector indicating the quantile for thresholding.
#' @param CrimeActualPts a data frame containing crime incident locations
#' @param r raster layer object
#' @param isInPoly a logical vector incidicating whether a location is inside the polygon boundary  
#' @return hit rate

HitRate <- function(Pred,prob,CrimeActualPts,r,isInPoly){
  threshold <- quantile(Pred$VALUE,probs=prob)
  selPix <- Pred$VALUE>=threshold
  
  CrimeActualPts.raster <- rasterize(CrimeActualPts[,c("X_COORD","Y_COORD")], r, CrimeActualPts$INC_CNT, fun=sum)
  
  CrimeRaster.df <- as.data.frame(CrimeActualPts.raster,xy=TRUE)
  names(CrimeRaster.df) <- c("X_COORD","Y_COORD","VALUE")
  CrimeRaster.df <- CrimeRaster.df[isInPoly,]
  CrimeRaster.df$VALUE[is.na(CrimeRaster.df$VALUE)] <- 0
  
  HitRate <- sum(CrimeRaster.df$VALUE[selPix])/nrow(CrimeActualPts)

  return(HitRate)
}

source("SmoothingFunction.R")
library(sfsmisc)
ROCeval <- function(CrimeData,groupSeq,Grd,percentArea=seq(0,1,length.out=21),Pred=NULL,hotspot=FALSE,Grd.full=NULL,
                    window.t=0,kernel.x=NULL,kernel.y=NULL,period=NULL,r=NULL,isInCity=NULL,prj=NULL){
  Ngrids <- nrow(Grd)
  Ngroups <- length(groupSeq)
  TPR <- matrix(NA,nrow=Ngroups,ncol=length(percentArea))
  AUC <- rep(NA,length=Ngroups)
  
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
      PredResults.df <- HotSpot$KernSm.df_inPoly
      PredResults.df$VALUE <- PredResults.df$KS_VAL
    }else{
      PredResults.df<- Grd[,c("X_COORD","Y_COORD")]
      PredResults.df$VALUE <- Pred[((i-1)*Ngrids+1):(i*Ngrids)]
    }
    
    CrimeActualPts <- subset(CrimeData,GROUP==d,select=c("X_COORD","Y_COORD","INC_CNT"))
    for (p in 1:length(percentArea)){ 
      TPR[i,p] <- HitRate(PredResults.df,1-percentArea[p],CrimeActualPts,r,isInCity)
    }
    AUC[i] <- integrate.xy(percentArea, TPR[i,], use.spline=TRUE)
  }
  
  avgTPR<- colMeans(TPR)
  avgAUC <- integrate.xy(percentArea, avgTPR, use.spline=TRUE)
  avgAUC2 <- mean(AUC)
  
  return(list(avgTPR=avgTPR,avgAUC=avgAUC,avgAUC2=avgAUC2,TPR=TPR,AUC=AUC))
}
