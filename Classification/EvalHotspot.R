hotspots <- c("LT","ST")
period.short <- floor(1/lambda)
period.long <- 5*floor(365/groupSize)
periods <- c(period.long,period.short) 

AUCary.hs <- matrix(0,nrow=length(hotspots),ncol=length(groupSeq.test))
TPRary.hs <- array(0,dim=c(length(hotspots),length(groupSeq.test),length(percentArea)))
for (m in 1:length(hotspots)){
  rocObj <- ROCeval(CrimeData,groupSeq.test,RegGrd,percentArea,Pred=NULL,hotspot=TRUE,RegGrd.full,
                    window.t,kernel.x,kernel.y,periods[m],r,isInCity,prj)
  AUCary.hs[m, ] <- rocObj$AUC
  TPRary.hs[m, , ] <- rocObj$TPR
}