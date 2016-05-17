source("SmoothingFunction.R")
source("PrepareFeatureLabel.R")

VarName.space <- c("StrDen","Dist2Street","Dist2CPDstation","Dist2School","Dist2Park",
                   "Dist2BusStop","Dist2Lstation","BldgDen","GarageDen","AvgStories","AvgUnits",
                   "Dist2CHA","Dist2POD")
VarName.census <- c("PERCENT_OF_HOUSING_CROWDED","PERCENT_HOUSEHOLDS_BELOW_POVERTY","PERCENT_AGED_16_UNEMPLOYED",
                    "PERCENT_AGED_25_WITHOUT_HIGH_SCHOOL_DIPLOMA","PERCENT_AGED_UNDER_18_OR_OVER_64","PER_CAPITA_INCOME","HARDSHIP_INDEX")
VarName.call <- c("VacantBldg","LightsAllOut","LightsOneOut","AlleyLightsOut")
VarName.weather <- c("Tsfc_F_avg","Rh_PCT_avg","Psfc_MB_avg","CldCov_PCT_avg","Tapp_F_avg","Spd_MPH_avg","PcpPrevHr_IN")
if (PredTarget=="Burglary"){
  VarName.STcrime <- c("BurglaryShort","LarcenyShort","VehicleTheftShort")
  VarName.LTcrime <- "BurglaryLong"
}else if (PredTarget=="ViolentCrime"){
  VarName.STcrime <- "VCShort"
  VarName.LTcrime <- "VCLong"
}

VarName <- c(VarName.space,VarName.census,VarName.call,VarName.weather,VarName.STcrime,VarName.LTcrime)

period.long <- 5*floor(365/groupSize)
set.seed(1234)
scaling <- "zscore"
# scaling <- FALSE
AllRegion <- rep(TRUE,nrow(RegGrd))

LabFeatVec <- LabelFeatureVec(RegGrd,groupSeq.train,CrimeData.train,CrimeData,WeatherData.train,VarName.space,VarName.census,VarName.call,
                              VarName.weather,VarName.STcrime,VarName.LTcrime,CrimeDataNames.train,CallDataNames.train,
                              RegGrd.full,AllRegion,window.t,kenerl.x,kernel.y,kernel.t,period.long,r,prj,isInCity)
FeatureVec.train <- LabFeatVec$FeatureVec
Label.train <- LabFeatVec$Label

LabFeatVec <- LabelFeatureVec(RegGrd,groupSeq.test,CrimeData.test,CrimeData,WeatherData.test,VarName.space,VarName.census,VarName.call,
                              VarName.weather,VarName.STcrime,VarName.LTcrime,CrimeDataNames.test,CallDataNames.test,
                              RegGrd.full,AllRegion, window.t,kenerl.x,kernel.y,kernel.t,period.long,r,prj,isInCity)
FeatureVec.test <- LabFeatVec$FeatureVec
Label.test <- LabFeatVec$Label

# scale data
ScaledFeat <- Scaling(FeatureVec.train,FeatureVec.test,scaling=scaling)
FeatureVec.train_scale <- ScaledFeat$ScaledTrain
FeatureVec.test_scale <- ScaledFeat$ScaledTest

# gather data
AssembledData <- AssembleData(FeatureVec.train_scale,Label.train,FeatureVec.test_scale,Label.test,downsample=TRUE)
TrainData.sub <- AssembledData$TrainData.sub
TestData.sub <- AssembledData$TestData.sub

# garbage collection to release memory
rm(list=sapply(CallTypes,paste0,"Data.train",USE.NAMES=FALSE))
rm(list=sapply(CallTypes,paste0,"Data.test",USE.NAMES=FALSE))
rm(list=c("ScaledFeat","LabFeatVec","AssembledData","FeatureVec.train_scale","FeatureVec.test_scale",
          "FeatureVec.train","FeatureVec.test"))
invisible(gc(verbose=FALSE))