AUCary.cls <- matrix(0,nrow=length(classifiers),ncol=length(groupSeq.test))
TPRary.cls <- array(0,dim=c(length(classifiers),length(groupSeq.test),length(percentArea)))

for (m in 1:length(classifiers)){
  rocObj <- ROCeval(CrimeData,groupSeq.test,RegGrd,percentArea,PredMat.test[,m],r=r,isInCity=isInCity)
  AUCary.cls[m, ] <- rocObj$AUC
  TPRary.cls[m, , ] <- rocObj$TPR
}
