# select regions by thresholding long-term crime density values
SelRegion <- function(CrimeData,startGroup,period,window.t,Grd.full,r,kenerl.x,kernel.y,prj,
                      isInCity,thresholds=NA,probs=NA){
  CrimeHistPts.long <- subset(CrimeData,GROUP>=startGroup-period.long & GROUP<=startGroup-window.t,select=c("X_COORD","Y_COORD","INC_CNT"))  
  KDE.long <- SpKernSmCrime(CrimeHistPts.long,Grd.full,r,kernel.x,kernel.y,isInCity,prj)
  KDE.long_df_inCity <- KDE.long$KernSm.df_inPoly
  KDE.long_df_inCity$KS_VAL <- minmaxScale(KDE.long_df_inCity$KS_VAL)
  
  if (!any(is.na(probs))){
    thresholds <- quantile(KDE.long_df_inCity$KS_VAL,probs=probs)
  }
  SelRegion <- KDE.long_df_inCity$KS_VAL>=thresholds[1]&KDE.long_df_inCity$KS_VAL<=thresholds[2]
  return(SelRegion)
}

LabelFeatureVec <- function(Grd,groupSeq,CrimeData,HistCrimeData,WeatherData,VarName.space=NULL,VarName.census=NULL,VarName.call=NULL,
                            VarName.weather=NULL,VarName.STcrime=NULL,VarName.LTcrime=NULL,CrimeDataNames=NULL,CallDataNames=NULL,
                            Grd.full=NULL,SelRegion=rep(TRUE,nrow(Grd)),window.t=NULL,kenerl.x=NULL,kernel.y=NULL,kernel.t=NULL,
                            period.long=NULL,r=NULL,prj=NULL,isInCity=NULL){
  # include long-term predictor
  VarName <- c(VarName.space,VarName.census,VarName.call,VarName.weather,VarName.STcrime,VarName.LTcrime)
  Ngrids <- nrow(Grd)
  Ngroups <- length(groupSeq)
  FeatureVec <- matrix(NA,nrow=Ngroups*Ngrids,ncol=length(VarName))
  FeatureVec <- as.data.frame(FeatureVec)
  names(FeatureVec) <- VarName
  Label <- rep(0,nrow(FeatureVec))
  
  for (i in 1:Ngroups){
    d <- groupSeq[i]
    
    # Spatial variables
    FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.space] <- Grd[SelRegion,VarName.space] 
    
    # Census variables
    FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.census] <- Grd[SelRegion,VarName.census] 
    
    # 311 calls variables
    if(!is.null(CallDataNames)){
      for (j in 1:length(CallDataNames)){
        eval(parse(text=paste0("CallData.group <- subset(",CallDataNames[j],",GROUP==d,select=c('X_COORD','Y_COORD','CNT'))")))
        CallRaster.df_inCity <- MapCount(CallData.group,"CNT",r,isInCity,fun='count')
        FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.call[j]] <- CallRaster.df_inCity[SelRegion]
      }
    }
    
    # weather variables
    FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.weather] <- apply(subset(WeatherData,GROUP==d,select=-GROUP),MARGIN=2,FUN=mean)
    
    # Kernel-smoothed recent-neighboring-incident variables (short-term effect)
    if(!is.null(CrimeDataNames)){
      for (j in 1:length(CrimeDataNames)){
        eval(parse(text=paste0("KS <-STKernSmCrime(",CrimeDataNames[j],",d,RegGrd.full,
                             r,window.t,kernel.x,kernel.y,kernel.t,isInCity,prj)")))
        FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.STcrime[j]] <-KS$KernSm.df_inPoly$KS_VAL[SelRegion]
      }
    }
   
    # Kernel-smoothed long-term effect
    # the time period that is used to estimate the long-term effect is past "period.long" up to recent "window.t"
    # so that the short-term effects are excluded
    if(!is.null(VarName.LTcrime)){
      CrimeHistPts.long <- subset(HistCrimeData,GROUP>=d-period.long & GROUP<=d-window.t-1,select=c("X_COORD","Y_COORD","INC_CNT"))  
      KDE.long <- SpKernSmCrime(CrimeHistPts.long,Grd.full,r,kernel.x,kernel.y,isInCity,prj)
      FeatureVec[((i-1)*Ngrids+1):(i*Ngrids),VarName.LTcrime] <- KDE.long$KernSm.df_inPoly$KS_VAL[SelRegion]
    }

    # assign labels according to the crime counts
    CrimeData.group <- subset(CrimeData, GROUP==d, select=c("X_COORD","Y_COORD","INC_CNT"))
    CrimeRaster.df_inCity <- MapCount(CrimeData.group,"INC_CNT",r,isInCity,fun=sum) 
    Label[((i-1)*Ngrids+1):(i*Ngrids)] <- CrimeRaster.df_inCity[SelRegion]!=0
  }
  
  # convert skewed variable (e.g. 311 calls) to factors or use Box-Cox transformation to make them "normal-shaped"
  # Or let the algorithms figure out the nonlinearities (e.g. GAM: y = f1(x1)+f2(x2)+...fp(xp), fi() does the nonlinear transformation)
  # ?????
  # ##########
  
  Label <- as.factor(as.numeric(Label))
  return(list(FeatureVec=FeatureVec,Label=Label))
}


Scaling <- function(FeatureVec.train,FeatureVec.test,scaling="zscore"){
  if (scaling == "minmax"){
    ctr <- apply(FeatureVec.train,2,min)
    sc <- apply(FeatureVec.train,2,max) - apply(FeatureVec.train,2,min)
    FeatureVec.train_scale <- as.data.frame(apply(FeatureVec.train,2,minmaxScale))
    FeatureVec.test_scale <- as.data.frame(scale(FeatureVec.test,center=ctr,scale=sc))
  }else if (scaling=="zscore"){
    ctr <- colMeans(FeatureVec.train)
    sc <- apply(FeatureVec.train,2,sd)
    FeatureVec.train_scale <- as.data.frame(scale(FeatureVec.train))
    FeatureVec.test_scale <- as.data.frame(scale(FeatureVec.test,center=ctr,scale=sc)) 
  }else{
    FeatureVec.train_scale <- FeatureVec.train
    FeatureVec.test_scale <- FeatureVec.test
  }
  return(list(ScaledTrain=FeatureVec.train_scale,ScaledTest=FeatureVec.test_scale))
}

AssembleData <- function(FeatureVec.train_scale,Label.train,FeatureVec.test_scale,Label.test,downsample=TRUE){
  TrainData <- cbind(FeatureVec.train_scale,Label.train)
  TestData <- cbind(FeatureVec.test_scale,Label.test)
  
  if (downsample){
    Class0.train <- subset(TrainData,Label.train==0)
    Class1.train <- subset(TrainData,Label.train==1)
    
    SubSample <- sample(1:nrow(Class0.train),size=nrow(Class1.train))
    Class0.train_sub <- Class0.train[SubSample,]
    TrainData.sub <- rbind(Class0.train_sub,Class1.train) 
  }else{
    TrainData.sub <- TrainData
  }
  TestData.sub <- TestData
  
  names(TrainData.sub)[names(TrainData.sub)=="Label.train"] <- "Label"
  names(TestData.sub)[names(TestData.sub)=="Label.test"] <- "Label"
  names(TrainData)[names(TrainData)=="Label.train"] <- "Label"
  names(TestData)[names(TestData)=="Label.test"] <- "Label"
  
  return(list(TrainData.sub=TrainData.sub,TestData.sub=TestData.sub,TrainData=TrainData,TestData=TestData))
}

BoxCoxTrans <- function(x, lambda=1, offset=0){
  if(lambda==0){
    return(log(x+offset))
  }else{
    return(((x+offset)^lambda - 1)/lambda)
  }
}