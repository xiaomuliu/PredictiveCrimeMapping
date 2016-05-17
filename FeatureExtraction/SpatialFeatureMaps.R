setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Code/FeatureExtraction/")

library(sp)
library(rgeos)
library(rgdal)
library(raster)

Path.GIS <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Data/GISData/"
Path.city <- paste0(Path.GIS,"City_Boundary")
city.shp <- readOGR(Path.city,"City_Boundary") 
cellsize.x <- 660
cellsize.y <- 660
X_range <- city.shp@bbox[1,]
Y_range <- city.shp@bbox[2,]
grd.full <- expand.grid(list(X_COORD=seq(X_range[1],X_range[2],by=cellsize.x),
                             Y_COORD=seq(Y_range[1],Y_range[2],by=cellsize.y)))
coordinates(grd.full) = ~X_COORD+Y_COORD # convert to SpatialPoints

prj <- paste("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999",
             "+x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(grd.full) <- prj

# rasterize the city spatial polygon to get a grid template
grd.full <- SpatialPixels(grd.full)
r <- raster(ncol=grd.full@grid@cells.dim[1],nrow=grd.full@grid@cells.dim[2],
            xmn=grd.full@bbox[1,1],xmx=grd.full@bbox[1,2],ymn=grd.full@bbox[2,1],ymx=grd.full@bbox[2,2],crs=CRS(prj))

city.raster <- rasterize(city.shp,r,0)
city.df_full <- as.data.frame(city.raster,xy=TRUE)
city.df_full <- city.df_full[,1:2]
names(city.df_full) <- c("X_COORD","Y_COORD")
RegGrd.full <- city.df_full

coordinates(city.df_full) <- c("X_COORD", "Y_COORD") 
proj4string(city.df_full) <- prj
BoundedOverFullGrd <- over(city.df_full, city.shp)
isInCity <- !is.na(BoundedOverFullGrd$OBJECTID)
RegGrd <- RegGrd.full[isInCity,]

Path.street_line <- paste0(Path.GIS,"Street_Center_Line")
Path.major_street <- paste0(Path.GIS,"Major_Streets")
Path.cpd_station <- paste0(Path.GIS,"police_stations_poly")
Path.school <- paste0(Path.GIS,"School_Grounds")
Path.park <- paste0(Path.GIS,"Parks_Aug2012")
Path.hospital <- paste0(Path.GIS,"Hospitals")
Path.library <- paste0(Path.GIS,"Libraries")
Path.CTA_stop <-paste0(Path.GIS,"CTA_BusStops")
Path.CTA_route <- paste0(Path.GIS,"CTA_Routes")
Path.CTA_rail <- paste0(Path.GIS,"CTA_RailLines")
# Path.building <- paste0(Path.GIS,"Buildings")
Path.CHA <- paste0(Path.GIS,"CHA")
Path.GangBndy <- paste0(Path.GIS,"Gang_bndy")
Path.GangConflict <- paste0(Path.GIS,"Gang_conflicts")
Path.POD <- paste0(Path.GIS,"PODs")
Path.community <- paste0(Path.GIS,"Community_bndy")

street_line.shp <- readOGR(Path.street_line,"Transportation")
major_street.shp <- readOGR(Path.major_street,"Major_Streets")
cpd_station.shp <- readOGR(Path.cpd_station,"police_stations")
school.shp <- readOGR(Path.school,"School_Grounds")
park.shp <- readOGR(Path.park,"Parks_Aug2012")
hospital.shp <- readOGR(Path.hospital,"Hospitals")
library.shp <- readOGR(Path.library,"Libraries")
CTA_stop.shp <- readOGR(Path.CTA_stop,"CTA_BusStops")
CTA_stop.shp <- spTransform(CTA_stop.shp, CRSobj=prj)
CTA_route.shp <- readOGR(Path.CTA_route,"CTA_Routes")
CTA_rail.shp <- readOGR(Path.CTA_rail,"CTA_RailLines")
# building.shp <- readOGR(Path.building,"Buildings")  # ~3 GB file!
# building.spdf <- building.shp
# building.spdf@data <- subset(building.spdf@data,select=c("BLDG_ID","BLDG_STATU","STORIES","NO_OF_UNIT","NO_STORIES",
#                                                          "NON_STANDA","YEAR_BUILT","BLDG_SQ_FO","BLDG_CONDI","VACANCY_ST",
#                                                          "X_COORD","Y_COORD","SHAPE_AREA","SHAPE_LEN"))
load("BuildingShp.RData")

# rename the attribute "NO_STORIES" as "UG_STORIES" (number of stories below ground) to make it less misleading
names(building.spdf@data)[names(building.spdf@data)=="NO_STORIES"] <- "UG_STORIES"
#remove "non-standard" building: 8500, CTAPLAT, MONUMNET, OTHER, keep only residential garage and NA's
building.spdf <- subset(building.spdf, NON_STANDA=="RSGARAGE" | is.na(NON_STANDA))
# remove 95 "proposed", 1 "demolished" and 1 "NA" in BLDG_STATU entries
building.spdf <- subset(building.spdf,BLDG_STATU=="ACTIVE",select=-c(BLDG_STATU))
#remove three entries where x,y coordinates are zeros
building.spdf <- subset(building.spdf, X_COORD!=0 & Y_COORD!=0)

CHA.shp <- readOGR(Path.CHA,"cha_locations")
GangBndy.shp <- readOGR(Path.GangBndy,"gang_bndy")
GangConflict.shp <- readOGR(Path.GangConflict,"gang_conflicts")
POD.shp <- readOGR(Path.POD,"pods")
community.shp <- readOGR(Path.community,"CommAreas")

source("POIFunction.R")

# patch size
extension.x <- 330
extension.y <- 330
extension <- c(extension.x,extension.y)

RegGrd <- StreetDen(RegGrd,street_line.shp,extension,Attr="StrDen",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,major_street.shp,"Dist2Street",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,cpd_station.shp,"Dist2CPDstation",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,school.shp,"Dist2School",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,park.shp,"Dist2Park",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,hospital.shp,"Dist2Hospital",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,library.shp,"Dist2Library",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,CTA_stop.shp,"Dist2BusStop",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,CTA_route.shp,"Dist2CTAroute",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,CTA_rail.shp,"Dist2CTArail",prj=prj)
RegGrd <- BuildingDen(RegGrd,subset(building.spdf,is.na(NON_STANDA)),extension,Attr="BldgDen")
RegGrd <- BuildingDen(RegGrd,subset(building.spdf,NON_STANDA=="RSGARAGE"),extension,Attr="GarageDen")
RegGrd <- BuildingType(RegGrd,subset(building.spdf,is.na(NON_STANDA)),extension,Attr="AvgStories",
                       BldgType="STORIES",fun=mean,na.rm=TRUE)
RegGrd <- BuildingType(RegGrd,subset(building.spdf,is.na(NON_STANDA)),extension,Attr="AvgUnits",
                       BldgType="NO_OF_UNIT",fun=mean,na.rm=TRUE)
RegGrd <- Grd2POIdist(RegGrd,CHA.shp,Attr="Dist2CHA",prj=prj)
RegGrd <- Grd2POIdist(RegGrd,POD.shp,Attr="Dist2POD",prj=prj)
RegGrd <- GrdinBndy(RegGrd,GangBndy.shp,Attr="IsInGangBndy",prj=prj)
RegGrd <- GrdinBndy(RegGrd,GangConflict.shp,Attr="GangConflictLvl",ShpfileAttr="THREAT_LEV",prj=prj)
levels(RegGrd$GangConflictLvl) <- c("HIGH","LOW","MEDIUM","NA")
RegGrd$GangConflictLvl[is.na(RegGrd$GangConflictLvl)] <- "NA"

# socioeconomic 
Path.census <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/OtherData/"
fileName <- "Census_Data_socioeconomic_2008_2012.csv"
CommCensus <- read.csv(paste0(Path.census,fileName))
names(CommCensus) <- gsub("\\.","_",names(CommCensus))
names(CommCensus) <- gsub("__","_",names(CommCensus))
CensusAttr <- names(CommCensus)[c(-1,-2)]
CommCensus$AREA_NUMBE <- as.factor(CommCensus$Community_Area_Number) 
CommCensus$Community_Area_Number <- NULL

CommCensus.shp <- community.shp
CommCensus.shp <- merge(CommCensus.shp,CommCensus,by="AREA_NUMBE")
CommCensus.shp@data$AREA_NUM_1 <- NULL

RegGrd <- GrdinBndy(RegGrd,CommCensus.shp,Attr=CensusAttr,ShpfileAttr=CensusAttr,prj=prj)

save(RegGrd,file="SpatialFeatureMap_Res660ft.RData")
