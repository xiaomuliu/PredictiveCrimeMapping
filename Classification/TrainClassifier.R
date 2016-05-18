source("Classification.R")

# Classifier names must be one of ("NB","LDA","LogReg","Lasso","RF","Adaboost","SVM","GAM","sNN","mNN","DBN","SAE")
classifiers <- c("LDA","LogReg")
if ("GAM" %in% classifiers){
  formula.part1 <- paste0("Label~",paste(VarName.call,sep="",collapse="+"))
  formula.part2 <- paste("s(",c(VarName.space,VarName.census,VarName.weather,VarName.STcrime,VarName.LTcrime),")",sep="",collapse="+")
  gamFormula <- as.formula(paste0(formula.part1,"+",formula.part2))
}
if (any(c("DBN","SAE") %in% classifiers)){
  spec <- "learningrate=0.8,momentum=0.3,learningrate_scale=0.5,output='sigm',numepochs=3,
  batchsize=round(0.005*nrow(TrainData.sub)),hidden_dropout=0,visible_dropout=0,cd=1"
}else{
  spec <- NULL
}
if ("SVM" %in% classifiers){
  spec <- "cachesize=256,tolerance=0.05"
  featureMeanDist <- mean(as.vector(dist(subset(TrainData.sub[sample(nrow(TrainData.sub),1000),],select=-c(Label)))))
}
if ("mNN" %in% classifiers){spec <- "rep=5,stepmax=1e+03"}

funArgs <- list(NB="kfold=0,PCA=FALSE,keepPC=NULL",
                LDA="kfold=0,PCA=FALSE,keepPC=NULL",
                LogReg="kfold=0,PCA=FALSE,keepPC=NULL",
                Lasso="kfold=10,lambda=NULL,NumLambda=15",
                RF="kfold=0,NumVar=NULL,NumTree=NULL",
                Adaboost="kfold=0,NumIter=NULL",
                SVM=paste0("kfold=5,SVMkernel='polynomial',Cseq=10^(seq(-2,1,by=0.5)),dSeq=1:3,
                     gammaSeq=1/featureMeanDist^2*10^seq(-1,1,by=0.5),",spec),
                GAM="gamFormula=gamFormula",
                sNN="kfold=5,NumUnit=seq(2,30,by=2),WgtDecaySeq=10^seq(-2,1,by=0.5)",
                mNN=paste0("kfold=5,hiddenArch=list(c(10,5),c(20,10),c(20,5)),",spec),
                DBN=paste0("hiddenArch=c(100,50),",spec),
                SAE=paste0("hiddenArch=c(100,50),",spec)
)

PredMat.test <- matrix(0,nrow=nrow(TestData.sub),ncol=length(classifiers))

for (m in 1:length(classifiers)){
  t <- system.time( eval( parse(text=paste0("classObj <- Classification(TrainData.sub,TestData.sub,classifiers[m],",funArgs[[classifiers[m]]],")")) ) )
  print(t)
  PredMat.test[,m] <- classObj$PredTest
}