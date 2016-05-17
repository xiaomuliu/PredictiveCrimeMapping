GroupData <- function(Data,groupSize,startDate=Data$DATEOCC[1],endDate=Data$DATEOCC[nrow(Data)]){  
  if("Date" %in% colnames(Data)){names(Data)[names(Data)=="Date"] <- "DATEOCC"}
  
  Data$GROUP <- rep(NA,nrow(Data))
  groupNo <- 1
  DaySeq <- seq.Date(startDate,endDate,by=1) 
  groupSeq <- seq(1,length(DaySeq),by=groupSize)
  for (i in groupSeq){   
    TargetDays <- seq.Date(DaySeq[i],DaySeq[i]+groupSize-1,by=1)
    Data$GROUP[Data$DATEOCC %in% TargetDays] <- groupNo
    groupNo <- groupNo+1
  }
  Data$GROUP[is.na(Data$GROUP)] <- groupNo 
  return(Data)
}

MatchCallData <- function(CallData,CrimeData){
  dateList <- unique(CallData$DATE)
  CallData$GROUP <- rep(NA,nrow(CallData))
  for (i in dateList){
    CallData$GROUP[CallData$DATE==i] <- CrimeData$GROUP[CrimeData$DATEOCC==i][1]
  }
  return(CallData)
}