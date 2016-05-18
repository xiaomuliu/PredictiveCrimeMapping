# Visualize a test example
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

exampleIdx <- 10 # which example to show
modelIdx <- 1 # which model to show

Pred.test.sub <- PredMat.test[((exampleIdx-1)*nrow(RegGrd)+1):(exampleIdx*nrow(RegGrd)),modelIdx]
Pred.df_inCity <- data.frame(X_COORD=RegGrd$X_COORD,Y_COORD=RegGrd$Y_COORD,ProbPred=Pred.test.sub)
Pred.raster_inCity <- rasterize(Pred.df_inCity[,c("X_COORD","Y_COORD")], r, Pred.df_inCity$ProbPred, fun=mean)

CrimeData.group <- subset(CrimeData, GROUP==groupSeq.test[exampleIdx], select=c("X_COORD","Y_COORD","INC_CNT"))
CrimeData.sp <- SpatialPoints(coords=CrimeData.group[,c("X_COORD","Y_COORD")])
EmptyRaster <- rasterize(RegGrd[,c("X_COORD","Y_COORD")], r, 0)

par(mfrow=c(1,2),mar=c(4,2,1,2),xaxs="i",yaxs="i",cex.axis=0.8,cex.lab=0.8,pty="s")
plot(Pred.raster_inCity,col=jet.colors(256),xlab="X coordinate",ylab="Y coordinate")
plot(EmptyRaster,xlab="X coordinate",ylab="Y coordinate",col="white",legend=FALSE)
plot(city.shp, border="black",add=TRUE)
points(CrimeData.sp, pch=16, cex=.5,col="red")