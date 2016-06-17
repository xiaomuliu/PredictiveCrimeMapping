#  Cross-validation/boostrapping the roc curves (significant test)
set.seed(123)
bootn <- 100 
perfList.cls <- vector("list",length(classifiers))
AUCmat.cls <- matrix(0,ncol=length(classifiers),nrow=bootn)

repIdxMat <- matrix(rep(1:length(Label.test),bootn),ncol=bootn)
repIdxMat <- apply(repIdxMat,2,sample,size=nrow(repIdxMat),replace=TRUE)
rocCutoff<- seq(1,0,length=101)

for (m in 1:length(classifiers)){
  bootList <- list(Pred=vector("list",bootn),Label=vector("list",bootn))
  
  PredLabel.cls <- data.frame(Pred=PredMat.test[,m],Label=Label.test)
  for (i in 1:bootn){
    bootList$Pred[[i]] <- PredLabel.cls$Pred[repIdxMat[,i]]
    bootList$Label[[i]] <- PredLabel.cls$Label[repIdxMat[,i]]
  }
  bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,cutoff=rocCutoff)
  perfList.cls[[m]] <- bootObj$perf
  AUCmat.cls[,m] <- bootObj$auc
}
