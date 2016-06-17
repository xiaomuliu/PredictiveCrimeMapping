hotspots <- c("LT","ST")
period.short <- floor(1/lambda)
period.long <- 5*floor(365/groupSize)
periods <- c(period.long,period.short) 

HSpred <- function(CrimeData,groupSeq,Grd,subRegion=rep(TRUE,nrow(Grd)),Grd.full=NULL,
                   window.t=0,kernel.x=NULL,kernel.y=NULL,period=NULL,r=NULL,isInCity=NULL,prj=NULL){
  Ngrids <- nrow(Grd)
  Ngroups <- length(groupSeq)
  regionSize <- sum(subRegion==TRUE)
  PredVal <- rep(0,length=regionSize*Ngroups)
  
  for (i in 1:Ngroups){
    d <- groupSeq[i]
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
  } 
  return(PredVal)
}

PredMat_HS.test <- matrix(0,nrow=length(Label.test),ncol=length(hotspots))
for (m in 1:length(hotspots)){
  PredMat_HS.test[,m] <- HSpred(CrimeData,groupSeq.test,RegGrd,subRegion=rep(TRUE,nrow(RegGrd)),RegGrd.full,
                                           window.t,kernel.x,kernel.y,periods[m],r,isInCity,prj) 
}

perfList.hs <- vector("list",length(hotspots))
AUCmat.hs <- matrix(0,ncol=length(hotspots),nrow=bootn)

for (m in 1:length(hotspots)){
  bootList <- list(Pred=vector("list",bootn),Label=vector("list",bootn))
  
  PredLabel.hs <- data.frame(Pred=PredMat_HS.test[,m],Label=Label.test)
  for (i in 1:bootn){
    bootList$Pred[[i]] <- PredLabel.hs$Pred[repIdxMat[,i]]
    bootList$Label[[i]] <- PredLabel.hs$Label[repIdxMat[,i]]
  }
  bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,cutoff=rocCutoff)
  perfList.hs[[m]] <- bootObj$perf
  AUCmat.hs[,m] <- bootObj$auc
}