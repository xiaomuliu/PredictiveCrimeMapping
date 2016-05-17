if (exists("classifiers") & exists("hotspots")){
  library(abind)
  models <- c(classifiers,hotspots)
  AUCary <- rbind(AUCary.cls,AUCary.hs)
  TPRary <- abind(TPRary.cls,TPRary.hs,along=1)
}else if (exists("classifiers") & !exists("hotspots")){
  models <- classifiers
  AUCary <- AUCary.cls
  TPRary <- TPRary.cls
}else if (!exists("classifiers") & exists("hotspots")){
  models <- hotspots
  AUCary <- AUCary.hs
  TPRary <- TPRary.hs
}

TPR.mean <- apply(TPRary,c(1,3),mean)
TPR.sd <- apply(TPRary,c(1,3),sd)

cl <- rainbow(length(models))
ptChar <- 13:(13+length(models)-1)

par(mfrow=c(1,1),mar=c(4.5,3,3,3),xaxs="i",yaxs="i",cex.axis=1,cex.lab=1,pty="s")
plot(0, type="n", xlab="Percentage of the highest pixel values (%)", ylab="Hit rate (%)",
     xlim=100*c(0,percentArea[length(percentArea)]),ylim=100*c(0,1.1*max(TPRary)),cex.lab=1.2)
for (i in 1:length(models)){
  x <- 100*percentArea
  y <- 100*TPR.mean[i,]
  bar <- 100*TPR.sd[i,]
  lines(x,y,col=cl[i],pch=ptChar[i],type='b',lty=1,cex=0.8)
  arrows(x, y+bar, x, y-bar, angle=90, code=3, length=0.05,col=cl[i]) # one-standard-deviation error bar
}
# mtext("ROC",side=3,cex=1,outer=TRUE)
par(new=TRUE)
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
legend("bottomright",legend=models,col=cl,pch=ptChar,lty=1,lwd=2,cex=0.8,inset=c(0,0),xpd=TRUE)

plot(0, type="n", xlab="Test Group", ylab="AUC",xlim=c(1,length(groupSeq.test)),ylim=range(AUCary))
for (i in 1:length(models)){
  lines(1:length(groupSeq.test),AUCary[i,],col=cl[i],pch=ptChar[i],type='b')
}
# mtext("partial AUC",side=3,cex=1,outer=TRUE)
par(new=TRUE)
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
legend("bottomright",legend=models,col=cl,pch=ptChar,lty=1,lwd=2,cex=0.8,inset=c(0,0),xpd=TRUE)