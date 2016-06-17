if (exists("classifiers") & exists("hotspots")){
  library(abind)
  models <- c(classifiers, hotspots)
  AUCmat <- cbind(AUCmat.cls,AUCmat.hs)
  rocList <- vector("list",length(models))
  for (m in 1:length(classifiers)){
    rocList[[m]] <- perfList.cls[[m]]
  }
  for (m in 1:length(hotspots)){
    rocList[[m+length(classifiers)]] <- perfList.hs[[m]]
  }
}else if (exists("classifiers") & !exists("hotspots")){
  models <- classifiers
  AUCmat <- AUCmat.cls
  rocList <- vector("list",length(models))
  for (m in 1:length(classifiers)){
    rocList[[m]] <- perfList.cls[[m]]
  }
}else if (!exists("classifiers") & exists("hotspots")){
  models <- hotspots
  AUCmat <- AUCmat.hs
  rocList <- perfList.hs
}


cl <- rainbow(length(models))
ptChar <- 13:(13+length(models)-1)
legendTxt<- paste(models,"AUC:",round(colMeans(AUCmat),3))

# PlotROC(rocList,cl,models,legendTxt,downsampling=0,errorbar="stderror",errorscale=1,
#         plotCI.lwd=1,plotCI.sfrac=0.005,lty=1,lwd=2,cex=0.8)

par(mfrow=c(1,1),mar=c(4.5,3,3,3),xaxs="i",yaxs="i",cex.axis=1,cex.lab=1,pty="s")
for (i in 1:length(models)){
  if (i==1){
    plot(rocList[[i]],col=cl[i],downsampling=0,avg="vertical",spread.estimate="stderror",
         spread.scale=1,show.spread.at=seq(0,1,length=21),plotCI.sfrac=0.005,lty=1,lwd=2,cex=0.8)
  }else{
    plot(rocList[[i]],col=cl[i],downsampling=0,avg="vertical",spread.estimate="stderror",
         spread.scale=1,show.spread.at=seq(0,1,length=21),plotCI.sfrac=0.005,lty=1,lwd=2,cex=0.8,add=TRUE)
  }  
}

mtext("ROC",side=3,cex=1,outer=TRUE)
par(new=TRUE)
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
legend("bottomright",legend=legendTxt,col=cl,lwd=2,lty=1,cex=0.8,inset=c(0,0),xpd=TRUE)
