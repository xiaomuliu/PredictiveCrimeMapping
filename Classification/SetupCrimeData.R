if (PredTarget=="Burglary"){
  load("Burglary_DataPortal.RData")
  CrimeData <- BurglaryData
  rm(BurglaryData) 
}else if (PredTarget=="ViolentCrime"){
  load("ViolentCrime_DataPortal.RData") 
  CrimeData <- ViolentCrimeData
  rm(ViolentCrimeData) 
}

source("GroupData.R")
groupSize <- 7
CrimeData <- GroupData(CrimeData,groupSize) 
