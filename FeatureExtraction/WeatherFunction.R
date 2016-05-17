ConvertToDailyWeatherData <- function(filename){
  
  WeatherData.raw = read.csv(filename)

  keep <- c('DateHrLwt','Tsfc_F','Tdew_F','Rh_PCT','Psfc_MB','CldCov_PCT','Tapp_F','PcpPrevHr_IN','Spd_MPH')
  WeatherData.raw <- subset(WeatherData.raw, select=keep)
  WeatherData.raw$DateHrLwt <- as.Date(WeatherData.raw$DateHrLwt)  
  
  sub.df <- subset(WeatherData.raw,select=-c(PcpPrevHr_IN,DateHrLwt))
  WeatherMean <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=mean,na.rm=TRUE)
  WeatherMax <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=max,na.rm=TRUE)
  WeatherMin <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=min,na.rm=TRUE)
  # calculate the total amount of daily precipitation
  Tot_pcp <- aggregate(PcpPrevHr_IN~DateHrLwt,data=WeatherData.raw,FUN=sum,na.rm=TRUE)
  names(Tot_pcp)[1]<-'Date'
  
  names(WeatherMean) <- c('Date',sapply(names(sub.df),paste,'_avg',sep=''))
  names(WeatherMax) <- c('Date',sapply(names(sub.df),paste,'_max',sep=''))
  names(WeatherMin) <- c('Date',sapply(names(sub.df),paste,'_min',sep=''))
  
  WeatherData.daily <- merge(merge(merge(WeatherMean,WeatherMax,all=TRUE),WeatherMin,all=TRUE),Tot_pcp,all=TRUE)
  return(WeatherData.daily)              
}

DailyWeatherDiff<- function(WeatherData.daily){
  # calculate one-day and two-day differences of each weather variables
  diff1 <- apply(WeatherData.daily[,-1],2,FUN=diff,lag=1)
  diff2 <- apply(WeatherData.daily[,-1],2,FUN=diff,lag=2)
  colnames(diff1) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff1',sep='')
  colnames(diff2) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff2',sep='')
  WeatherData.daily_diff <- cbind(Date=WeatherData.daily$Date[3:length(WeatherData.daily$Date)],
                                  as.data.frame(cbind(diff1[2:nrow(diff1),],diff2)))
}