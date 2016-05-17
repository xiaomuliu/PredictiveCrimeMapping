setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Code/FeatureExtraction/")
filePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictivaMapping/Data/OtherData/"

#All 311 calls for open and vacant buildings reported to the City of Chicago since January 1, 2010.
#The information is updated daily with the previous day's calls added to the records. 
#The data set provides the date of the 311 service request and the unique Service Request No. attached to each request.
#For each request, the following information (as reported by the 311 caller) is available: 
#address location of building; whether building is vacant or occupied; whether the building is open or boarded; 
#entry point if building is open; whether non-residents are occupying or using the building, 
#if the building appears dangerous or hazardous and if the building is vacant due to a fire.

fileName.load <- "311_Service_Requests_Vacant_and_Abandoned_Buildings_Reported.csv"
file <- paste(filePath,fileName.load,sep="")
VacantBldgData <- read.csv(file)

names(VacantBldgData) <- gsub("\\.","_",names(VacantBldgData))
names(VacantBldgData) <- gsub("_+","_",names(VacantBldgData))
names(VacantBldgData) <- gsub("_$","",names(VacantBldgData))

KeepAttr <- c("DATE_SERVICE_REQUEST_WAS_RECEIVED","X_COORDINATE","Y_COORDINATE")
VacantBldgData <- VacantBldgData[,KeepAttr]
colnames(VacantBldgData) <- c("DATE","X_COORD","Y_COORD")

# convert DATE to date class
VacantBldgData$DATE <- as.Date(strptime(VacantBldgData$DATE,"%m/%d/%Y"))
VacantBldgData <- VacantBldgData[order(VacantBldgData$DATE),]

# remove imcomplete date entries
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2014-12-31")
VacantBldgData <- subset(VacantBldgData,DATE>=startDate&DATE<=endDate)
VacantBldgData <- VacantBldgData[complete.cases(VacantBldgData),]

# add attribute 'Year','Month'
Year <- as.numeric(format(VacantBldgData$DATE,"%Y"))
Month <- as.numeric(format(VacantBldgData$DATE, "%m"))
VacantBldgData <- cbind(VacantBldgData,YEAR=Year,MONTH=Month)

VacantBldgData$CALL_CNT <- rep(1,nrow(VacantBldgData))

# save as a csv file
fileName.save <- "311CALLS_VACANT_BLDG_10_14.csv"
write.csv(VacantBldgData,paste(filePath,fileName.save,sep=""),row.names=FALSE)


#All open reports of "Street Lights - All Out" (an outage of 3 or more lights) made to 311 and all requests completed since January 1, 2011.
#The Chicago Department of Transportation (CDOT) oversees approximately 250,000 street lights that illuminate arterial and residential streets in Chicago. 
#CDOT performs repairs and bulb replacements in response to residents’ reports of street light outages.
#Whenever CDOT receives a report of an "All Out" the electrician assigned to make the repair looks at all the lights
#in that circuit (each circuit has 8-16 lights) to make sure that they are all working properly. 
#If a second request of lights out in the same circuit is made within four calendar days of the original request, 
#the newest request is automatically given the status of "Duplicate (Open)." 
#Since CDOT's electrician will be looking at all the lights in a circuit to verify that they are all working, 
#any "Duplicate (Open)" address will automatically be observed and repaired.
#Once the street lights are repaired, the status in CSR will read "Completed" for the original request 
#and "Duplicate (Closed)" for any duplicate requests. A service request also receives the status of "Completed" 
#when the reported lights are inspected but found to be in good repair and functioning; 
#when the service request is for a non-existent address; or when the lights are maintained by a contractor. Data is updated daily.
fileName.load <- "311_Service_Requests_Street_Lights_All_Out.csv"
file <- paste(filePath,fileName.load,sep="")
LightsAllOutData <- read.csv(file)

names(LightsAllOutData) <- gsub("\\.","_",names(LightsAllOutData))

KeepAttr <- c("Creation_Date","Status","Completion_Date","X_Coordinate","Y_Coordinate")
LightsAllOutData <- LightsAllOutData[,KeepAttr]
colnames(LightsAllOutData) <- c("CREATE_DATE","STATUS","COMPLETE_DATE","X_COORD","Y_COORD")

# remove duplicated requests
LightsAllOutData <- subset(LightsAllOutData,STATUS %in% c("Open","Completed"),select=-STATUS)

# convert DATE to date class
LightsAllOutData$CREATE_DATE <- as.Date(strptime(LightsAllOutData$CREATE_DATE,"%m/%d/%Y"))
LightsAllOutData$COMPLETE_DATE <- as.Date(strptime(LightsAllOutData$COMPLETE_DATE,"%m/%d/%Y"))
LightsAllOutData <- LightsAllOutData[order(LightsAllOutData$CREATE_DATE),]

# remove imcomplete date entries
startDate <- as.Date("2011-01-01")
endDate <- as.Date("2014-12-31")
LightsAllOutData <- subset(LightsAllOutData, CREATE_DATE>=startDate&CREATE_DATE<=endDate)
LightsAllOutData <- LightsAllOutData[complete.cases(LightsAllOutData),]

LightsAllOutData$CALL_CNT <- rep(1,nrow(LightsAllOutData))

# save as a csv file
fileName.save <- "311CALLS_LIGHTS_ALL_OUT_11_14.csv"
write.csv(LightsAllOutData,paste(filePath,fileName.save,sep=""),row.names=FALSE)

#This dataset contains all open 311 reports of one or two lights out on metal poles on a residential or arterial street and all completed requests since January 1, 2011. 
#Whenever CDOT receives a report of a street light outage, the electrician assigned to make the repair looks at all the lights 
#in a group (circuit) to make sure that they are working properly. 
#If two requests regarding the same group are made within 30 calendar days of each other, 
#the newest CSR is automatically given the status of "Duplicate (Open)." 
#Since the electrician will be looking at all the lights in a group to verify that they are all working the "Duplicate (Open)" address will also be observed and repaired. 
#Once the street lights are repaired, the CSR status will read "Completed" for the original request and "Duplicate (Closed)" for any duplicate requests. 
#Data is updated daily.
fileName.load <- "311_Service_Requests_Street_Lights_One_Out.csv"
file <- paste(filePath,fileName.load,sep="")
LightsOneOutData <- read.csv(file)

names(LightsOneOutData) <- gsub("\\.","_",names(LightsOneOutData))

KeepAttr <- c("Creation_Date","Status","Completion_Date","X_Coordinate","Y_Coordinate")
LightsOneOutData <- LightsOneOutData[,KeepAttr]
colnames(LightsOneOutData) <- c("CREATE_DATE","STATUS","COMPLETE_DATE","X_COORD","Y_COORD")

# remove duplicated requests
LightsOneOutData <- subset(LightsOneOutData,STATUS %in% c("Open","Completed"),select=-STATUS)

# convert DATE to date class
LightsOneOutData$CREATE_DATE <- as.Date(strptime(LightsOneOutData$CREATE_DATE,"%m/%d/%Y"))
LightsOneOutData$COMPLETE_DATE <- as.Date(strptime(LightsOneOutData$COMPLETE_DATE,"%m/%d/%Y"))
LightsOneOutData <- LightsOneOutData[order(LightsOneOutData$CREATE_DATE),]

# remove imcomplete date entries
startDate <- as.Date("2011-01-01")
endDate <- as.Date("2014-12-31")
LightsOneOutData <- subset(LightsOneOutData, CREATE_DATE>=startDate&CREATE_DATE<=endDate)
LightsOneOutData <- LightsOneOutData[complete.cases(LightsOneOutData),]

LightsOneOutData$CALL_CNT <- rep(1,nrow(LightsOneOutData))

# save as a csv file
fileName.save <- "311CALLS_LIGHTS_ONE_OUT_11_14.csv"
write.csv(LightsOneOutData,paste(filePath,fileName.save,sep=""),row.names=FALSE)


#This dataset contains all open 311 reports of one or more lights out on a wooden pole in the alley 
#and all completed requests since January 1, 2011. 
#If two requests regarding the same address are made within 30 calendar days of each other, 
#the newest CSR is automatically given the status of "Duplicate (Open)". 
#Once the alley light is repaired, the CSR status will read "Completed" for the original request and "Duplicate (Closed)"
#for any duplicate requests.Data is updated daily.
fileName.load <- "311_Service_Requests_Alley_Lights_Out.csv"
file <- paste(filePath,fileName.load,sep="")
AlleyLightsOutData <- read.csv(file)

names(AlleyLightsOutData) <- gsub("\\.","_",names(AlleyLightsOutData))

KeepAttr <- c("Creation_Date","Status","Completion_Date","X_Coordinate","Y_Coordinate")
AlleyLightsOutData <- AlleyLightsOutData[,KeepAttr]
colnames(AlleyLightsOutData) <- c("CREATE_DATE","STATUS","COMPLETE_DATE","X_COORD","Y_COORD")

# remove duplicated requests
AlleyLightsOutData <- subset(AlleyLightsOutData,STATUS %in% c("Open","Completed"),select=-STATUS)

# convert DATE to date class
AlleyLightsOutData$CREATE_DATE <- as.Date(strptime(AlleyLightsOutData$CREATE_DATE,"%m/%d/%y"))
AlleyLightsOutData$COMPLETE_DATE <- as.Date(strptime(AlleyLightsOutData$COMPLETE_DATE,"%m/%d/%y"))
AlleyLightsOutData <- AlleyLightsOutData[order(AlleyLightsOutData$CREATE_DATE),]

# remove imcomplete date entries
startDate <- as.Date("2011-01-01")
endDate <- as.Date("2014-12-31")
AlleyLightsOutData <- subset(AlleyLightsOutData, CREATE_DATE>=startDate&CREATE_DATE<=endDate)
AlleyLightsOutData <- AlleyLightsOutData[complete.cases(AlleyLightsOutData),]

AlleyLightsOutData$CALL_CNT <- rep(1,nrow(AlleyLightsOutData))

# save as a csv file
fileName.save <- "311CALLS_ALLEY_LIGHTS_OUT_11_14.csv"
write.csv(AlleyLightsOutData,paste(filePath,fileName.save,sep=""),row.names=FALSE)