ExpandCallData <- function(Data){
  Data$CREATE_DATE <- as.Date(Data$CREATE_DATE) 
  Data$COMPLETE_DATE <- as.Date(Data$COMPLETE_DATE) 
  
  ExpandedData <- data.frame(DATE=as.Date(character()),X_COORD=integer(),Y_COORD=integer(),CNT=integer())
  for(i in 1:nrow(Data)){
    if(Data$CREATE_DATE[i]==Data$COMPLETE_DATE[i]){
      DateSpan<-Data$CREATE_DATE[i]
    }else{
      DateSpan <- seq.Date(Data$CREATE_DATE[i],Data$COMPLETE_DATE[i]-1,by=1)
    }
    Dup <- data.frame(DATE=DateSpan,X_COORD=rep(Data$X_COORD[i],length(DateSpan)),
                      Y_COORD=rep(Data$Y_COORD[i],length(DateSpan)),CNT=rep(1,length(DateSpan)))
    ExpandedData <- rbind(ExpandedData,Dup)
  }
  ExpandedData <- ExpandedData[order(ExpandedData$DATE),]
  return(ExpandedData)
}

# 311 call data in the format of "DATE (begin-end) || X_COORD || Y_COORD || CNT (fixing period)" 
setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/FeatureExtraction/")
filePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Data/OtherData/"

ptm <- proc.time()
fileName.load <- "311CALLS_LIGHTS_ALL_OUT_11_14.csv"
LightsAllOut <- read.csv(paste0(filePath,fileName.load))
LightsAllOut.expanded <- ExpandCallData(LightsAllOut)
proc.time()-ptm
fileName.save <- "311CALLS_LIGHTS_ALL_OUT_11_14_expand.csv"
write.csv(LightsAllOut.expanded,paste0(filePath,fileName.save),row.names=FALSE)

ptm <- proc.time()
fileName.load <- "311CALLS_LIGHTS_ONE_OUT_11_14.csv"
LightsOneOut <- read.csv(paste0(filePath,fileName.load))
LightsOneOut.expanded <- ExpandCallData(LightsOneOut)
proc.time()-ptm
fileName.save <- "311CALLS_LIGHTS_ONE_OUT_11_14_expand.csv"
write.csv(LightsOneOut.expanded,paste0(filePath,fileName.save),row.names=FALSE)

ptm <- proc.time()
fileName.load <- "311CALLS_ALLEY_LIGHTS_OUT_11_14.csv"
AlleyLightsOut <- read.csv(paste0(filePath,fileName.load))
AlleyLightsOut.expanded <- ExpandCallData(AlleyLightsOut)
proc.time()-ptm
fileName.save <- "311CALLS_ALLEY_LIGHTS_OUT_11_14_expand.csv"
write.csv(AlleyLightsOut.expanded,paste0(filePath,fileName.save),row.names=FALSE)

### 
fileName.load <- "311CALLS_VACANT_BLDG_10_14.csv"
VacantBldgData <- read.csv(paste0(filePath,fileName.load))
VacantBldgData$DATE <- as.Date(VacantBldgData$DATE)
names(VacantBldgData)[names(VacantBldgData)=="CALL_CNT"] <- "CNT"
save(VacantBldgData,file=paste0(filePath,"VacantBldgData.RData"))

fileName.load <- "311CALLS_LIGHTS_ALL_OUT_11_14_expand.csv"
LightsAllOutData <- read.csv(paste0(filePath,fileName.load))
LightsAllOutData$DATE <- as.Date(LightsAllOutData$DATE)
save(LightsAllOutData,file=paste0(filePath,"LightsAllOutData.RData"))

fileName.load <- "311CALLS_LIGHTS_ONE_OUT_11_14_expand.csv"
LightsOneOutData <- read.csv(paste0(filePath,fileName.load))
LightsOneOutData$DATE <- as.Date(LightsOneOutData$DATE)
save(LightsOneOutData,file=paste0(filePath,"LightsOneOutData.RData"))

fileName.load <- "311CALLS_ALLEY_LIGHTS_OUT_11_14_expand.csv"
AlleyLightsOutData <- read.csv(paste0(filePath,fileName.load))
AlleyLightsOutData$DATE <- as.Date(AlleyLightsOutData$DATE)
save(AlleyLightsOutData,file=paste0(filePath,"AlleyLightsOutData.RData"))