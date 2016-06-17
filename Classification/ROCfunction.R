require(ROCR)
getROC <- function(Pred,Class,AUC=TRUE,downsample=FALSE,...){
  rocVal <- prediction(Pred, Class)
  perf <- performance(rocVal, measure="tpr", x.measure="fpr")
  if (downsample){
    perf <- downsampleROC(perf,...)
  }
  if (AUC){
    auc <- performance(rocVal,"auc")
    auc <- unlist(slot(auc, "y.values")) # convert S4 class to vector
    return(list(perf=perf,auc=auc))
  }else{
    return(perf)
  }
}

downsampleROC <- function(perfObj,downRate=1,cutoff=NA){
  if(any(is.na(cutoff))){
    cutoff <- seq(1,0,length=downRate*max(sapply(perfObj@alpha.values,length)))
  }
  for (i in 1:length(perfObj@alpha.values)){
    perfObj@x.values[[i]] <- quantile(perfObj@x.values[[i]],probs=rev(cutoff),names=FALSE)
    perfObj@y.values[[i]] <- quantile(perfObj@y.values[[i]],probs=rev(cutoff),names=FALSE)
    perfObj@alpha.values[[i]] <- cutoff
  }
  return(perfObj)
}

PlotROC <- function(rocList,colorList,modelList,legendTxt,downsampling=0,errorbar="stderror",errorscale=1,
                    ...){
  par(mfrow=c(1,1),mar=c(4.5,3,3,3),xaxs="i",yaxs="i",cex.axis=1,cex.lab=1,pty="s")
  for (i in 1:length(modelList)){
    if (i==1){
      plot(rocList[[i]],col=colorList[i],downsampling=downsampling,avg="vertical",spread.estimate=errorbar,
           spread.scale=errorscale,...)
    }else{
      plot(rocList[[i]],col=colorList[i],downsampling=downsampling,avg="vertical",spread.estimate=errorbar,
           spread.scale=errorscale,...,add=TRUE)
    }  
  }
  
  mtext("ROC",side=3,cex=1,outer=TRUE)
  par(new=TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
  legend("bottomright",legend=legendTxt,col=colorList,lwd=par("lwd"),lty=par("lty"),inset=c(0,0),xpd=TRUE)
}

# require(pROC)
# require(doMC)
# registerDoMC(cores=4)
# 
# ROCci <- function(Pred,Class,CIlevel=0.95,bootn=100,strata=TRUE){
#   rocObj <- roc(Class, Pred, auc=TRUE)
#   AUC.ci <- ci.auc(rocObj, conf.level=CIlevel, method="bootstrap", boot.n=bootn, boot.stratified=strata, 
#                    reuse.auc=TRUE,progress="none",parallel=TRUE,algorithm=3)
#   return(AUC.ci)
# }
# 
# ROCtest <- function(roc1=NULL,roc2=NULL,Class=NULL,Pred1=NULL,Pred2=NULL,paired=TRUE,bootn=100,strata=TRUE){
#   if (any(is.null(c(roc1,roc2)))){
#     testStat <- roc.test(response=Class,predictor1=Pred1,predictor2=Pred2,method="bootstrap",boot.n=bootn,
#                          paried=paired,boot.stratified=strata,reuse.auc=TRUE,progress="none",parallel=TRUE,algorithm=3)
#   }else{
#     testStat <- roc.test(roc1,roc2,method="bootstrap",boot.n=bootn,boot.stratified=strata,
#                          paried=paired,reuse.auc=TRUE,progress="none",parallel=TRUE,algorithm=3)
#   }
#   return(testStat)
# }