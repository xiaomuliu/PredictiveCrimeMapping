startMonth<- 5
endMonth <- 10
startYear.train <- 2011 
endYear.train <- 2011
startYear.test <- 2012 
endYear.test <- 2012

TrainGroup <- subset(CrimeData,YEAR>=startYear.train&YEAR<=endYear.train&MONTH>=startMonth&MONTH<=endMonth,
                     select=c("GROUP"))
TestGroup <- subset(CrimeData,YEAR>=startYear.test&YEAR<=endYear.test&MONTH>=startMonth&MONTH<=endMonth,
                    select=c("GROUP"))

groupSeq.train <- unique(TrainGroup$GROUP)
startGroup.train <- groupSeq.train[1]
endGroup.train <- groupSeq.train[length(groupSeq.train)]
groupSeq.test <- unique(TestGroup$GROUP)
startGroup.test <- groupSeq.test[1]
endGroup.test <- groupSeq.test[length(groupSeq.test)]


# Add *window.t* groups as the buffer so that the kernel smoothed feature would be able to comupted 
CrimeData.train <- subset(CrimeData,GROUP>=startGroup.train-window.t & GROUP<=endGroup.train,
                          select=c("X_COORD","Y_COORD","INC_CNT","GROUP"))
CrimeData.test <- subset(CrimeData,GROUP>=startGroup.test-window.t & GROUP<=endGroup.test,
                         select=c("X_COORD","Y_COORD","INC_CNT","GROUP"))

# load other crime type data
if (PredTarget=="Burglary"){
  CrimeFilePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Data/CrimeData/"
  CrimeTypes <- c("Larceny","MotorVehicleTheft")
  for (i in CrimeTypes){
    load(paste0(CrimeFilePath,i,"Data_portal.RData"))
    eval(parse(text=paste0(i,"Data<-GroupData(",i,"Data,groupSize)")))
    eval(parse(text=paste0(i,"Data<-subset(",i,"Data,select=c(\"GROUP\",\"X_COORD\",\"Y_COORD\",\"INC_CNT\"))")))
    eval(parse(text=paste0(i,"Data.train<-subset(",i,"Data,GROUP>=startGroup.train-window.t & GROUP<=endGroup.train)")))
    eval(parse(text=paste0(i,"Data.test<-subset(",i,"Data,GROUP>=startGroup.test-window.t & GROUP<=endGroup.test)")))
  }
  CrimeDataNames.train <- sapply(CrimeTypes,paste0,"Data.train",USE.NAMES=FALSE)
  CrimeDataNames.test <- sapply(CrimeTypes,paste0,"Data.test",USE.NAMES=FALSE)
  CrimeDataNames.train <- c("CrimeData.train",CrimeDataNames.train)
  CrimeDataNames.test <- c("CrimeData.test",CrimeDataNames.test)
  
  rm(list=sapply(CrimeTypes,paste0,"Data",USE.NAMES=FALSE))
  invisible(gc(verbose=FALSE))  
  
}else if (PredTarget=="ViolentCrime"){
  CrimeDataNames.train <- c("CrimeData.train")
  CrimeDataNames.test <- c("CrimeData.test")
}

# load 311 calls data
CallFilePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Data/OtherData/"
CallTypes <- c("VacantBldg","LightsAllOut","LightsOneOut","AlleyLightsOut")
for (i in CallTypes){
  load(paste0(CallFilePath,i,"Data.RData"))
  eval(parse(text=paste0(i,"Data<-MatchCallData(",i,"Data,CrimeData)")))
  eval(parse(text=paste0(i,"Data<-subset(",i,"Data,select=c(\"GROUP\",\"X_COORD\",\"Y_COORD\",\"CNT\"))")))
  eval(parse(text=paste0(i,"Data.train<-subset(",i,"Data,GROUP>=startGroup.train-window.t & GROUP<=endGroup.train)")))
  eval(parse(text=paste0(i,"Data.test<-subset(",i,"Data,GROUP>=startGroup.test-window.t & GROUP<=endGroup.test)")))
}

CallDataNames.train <- sapply(CallTypes,paste0,"Data.train",USE.NAMES=FALSE)
CallDataNames.test <- sapply(CallTypes,paste0,"Data.test",USE.NAMES=FALSE)

rm(list=sapply(CallTypes,paste0,"Data",USE.NAMES=FALSE))
invisible(gc(verbose=FALSE))

# load weather data
# source("WeatherFunction.R")
WeatherFilePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Data/WeatherData/"
startDate.weather <- "01/01/2001"
endDate.weather <- "12/31/2014"
filename <- paste(WeatherFilePath,'WeatherData_Daily_',as.character(as.Date(startDate.weather, "%m/%d/%Y")),
                        '_',as.character(as.Date(endDate.weather, "%m/%d/%Y")),'.csv',sep='')
WeatherData <- read.csv(filename)
WeatherData$Date <- as.Date(WeatherData$Date)

WeatherVar <- c("Tsfc_F_avg","Rh_PCT_avg","Psfc_MB_avg","CldCov_PCT_avg","Tapp_F_avg","Spd_MPH_avg","PcpPrevHr_IN")
# Match DATE in weather data and crime data
WeatherData <- subset(WeatherData,Date>=CrimeData$DATEOCC[1]&Date<=CrimeData$DATEOCC[nrow(CrimeData)])
WeatherData <- GroupData(WeatherData,groupSize,startDate=WeatherData$Date[1],endDate=WeatherData$Date[nrow(WeatherData)])
WeatherData <- subset(WeatherData,select=c("GROUP",WeatherVar))
WeatherData.train <- subset(WeatherData,GROUP>=startGroup.train-window.t & GROUP<=endGroup.train)
WeatherData.test <- subset(WeatherData,GROUP>=startGroup.test-window.t & GROUP<=endGroup.test)

rm(WeatherData)
invisible(gc(verbose=FALSE))
