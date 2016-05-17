#' Classification of crime incident present/absent using different classifiers.
#' 
#' @param TrainData a data frame of training data.
#' @param TestData a data frame of test data.
#' @param classifier a string value indicating the name of classifier, must be one of these \code{NB,LDA,LogReg,Lasso,RF,Adaboost,SVM,GAM,sNN,DBN,SAE} (case-sensitive)
#' @param kfolds a numeric value indicating the number of folds in cross-validation
#' @param PCA a logical value indicating whether do PCA on input variables (features)
#' @param keepPC a numeric value indicating the number of principal components to be included in training/testing models  
#' @return A list of the trained model \code{Model}, the predictions of test data \code{PredTest}, and CV error rates \code{CV} (if applicable).


Classification <- function(TrainData,TestData,classifier,kfold=0,PCA=FALSE,keepPC=NULL,NumVar=NULL,NumTree=NULL,lambda=NULL,NumLambda=NULL,
                           NumIter=NULL,SVMkernel=NULL,Cseq=NULL,dSeq=NULL,gammaSeq=NULL,gamFormula=NULL,NumUnit=NULL,WgtDecaySeq=NULL,hiddenArch=NULL,...){
  if (kfold>0){
    library(caret)
    kfold <- round(kfold)
    cvFold <- createFolds(TrainData$Label,k=kfold)
  }
  
  switch(classifier,
         # Naive Bayes
         NB={
           library(e1071)
           if (PCA) {         
             PCA.train <- prcomp(subset(TrainData,select=-c(Label)),center=TRUE,scale=TRUE,retx=TRUE,...)
             TestPC <- predict(PCA.train,newdata=subset(TestData,select=-c(Label)))
             
             if (kfold>0){
               # select the optimal number of principal components by CV
               CVtest.err <- matrix(0,nrow=length(keepPC),ncol=kfold)
               for (i in 1:length(keepPC)){
                 for (j in 1:kfold){
                   testIdx <- cvFold[[j]] 
                   Model.cv <- naiveBayes(Label~., data=cbind(as.data.frame(PCA.train$x[-testIdx,1:keepPC[i]]),Label=TrainData$Label[-testIdx]))
                   pred <- predict(Model.cv,cbind(as.data.frame(PCA.train$x[testIdx,1:keepPC[i]]),Label=TrainData$Label[testIdx]),type="class")
                   CVtest.err[i,j] <- sum(TrainData$Label[testIdx]!=pred)/length(testIdx)
                 }
               }
               keepPC.optimal <- keepPC[which.min(rowMeans(CVtest.err))]
             }else{
               keepPC.optimal <- keepPC[1]
             }
             
             TrainData.pca <- cbind(as.data.frame(PCA.train$x[,1:keepPC.optimal]),Label=TrainData$Label)
             TestData.pca <- cbind(as.data.frame(TestPC[,1:keepPC.optimal]),Label=TestData$Label)
             
             Model <- naiveBayes(Label~., data=TrainData.pca)
             Pred.test <- predict(Model, newdata=TestData.pca, type="raw")
             CVinfo <- CVtest.err
           }else{
             # No PCA
             Model <- naiveBayes(Label~., data=TrainData)
             Pred.test <- predict(Model, newdata=TestData, type="raw")
             CVinfo <- NA
           }
           Pred.test <- Pred.test[,2]
         },
         
         # Linear Discriminant Analysis 
         LDA={
           library(MASS)
           if (PCA){
             if (kfold>0){
               # select the optimal number of principal components by CV
               for (i in 1:length(keepPC)){
                 for (j in 1:kfold){
                   testIdx <- cvFold[[j]] 
                   Model.cv <- lda(Label~., data=cbind(as.data.frame(PCA.train$x[-testIdx,1:keepPC[i]]),Label=TrainData$Label[-testIdx]))
                   pred <- predict(Model.cv,cbind(as.data.frame(PCA.train$x[testIdx,1:keepPC[i]]),Label=TrainData$Label[testIdx]))
                   CVtest.err[i,j] <- sum(TrainData$Label[testIdx]!=pred$class)/length(testIdx)
                 }
               }
               keepPC.optimal <- keepPC[which.min(rowMeans(CVtest.err))]
             }else{
               keepPC.optimal <- keepPC[1]
             }
             
             TrainData.pca <- cbind(as.data.frame(PCA.train$x[,1:keepPC.optimal]),Label=TrainData$Label)
             TestData.pca <- cbind(as.data.frame(TestPC[,1:keepPC.optimal]),Label=TestData$Label)
             
             Model <- lda(Label~.,data=TrainData.pca)
             Pred.test <- predict(Model,TestData.pca)
             CVinfo <- CVtest.err
           }else{
             # No PCA
             Model <- lda(Label~.,data=TrainData)
             Pred.test <- predict(Model,TestData)
             CVinfo <- NA
           }
           Pred.test <- Pred.test$posterior[,2]
         },
         
         # Logistic regression
         LogReg={
           if (PCA){
             if (kfold>0){
               # select the optimal number of principal components by CV
               for (i in 1:length(keepPC)){
                 for (j in 1:kfold){
                   testIdx <- cvFold[[j]] 
                   Model.cv <- glm(Label~., family=binomial(link="logit"), 
                                   data=cbind(as.data.frame(PCA.train$x[-testIdx,1:keepPC[i]]),Label=TrainData$Label[-testIdx]))
                   pred <- predict(Model.cv,cbind(as.data.frame(PCA.train$x[testIdx,1:keepPC[i]]),Label=TrainData$Label[testIdx]),type="response")
                   CVtest.err[i,j] <- sum(TrainData$Label[testIdx]!=ifelse(pred>=0.5, 1, 0) )/length(testIdx)
                 }
               }
               keepPC.optimal <- keepPC[which.min(rowMeans(CVtest.err))]
             }else{
               keepPC.optimal <- keepPC[1]
             }
             
             TrainData.pca <- cbind(as.data.frame(PCA.train$x[,1:keepPC.optimal]),Label=TrainData$Label)
             TestData.pca <- cbind(as.data.frame(TestPC[,1:keepPC.optimal]),Label=TestData$Label)
             
             Model <- glm(Label~.,family=binomial(link="logit"), data=TrainData.pca)
             Pred.test <- predict(Model,TestData.pca,type="response")
             CVinfo <- CVtest.err
           }else{
             # No PCA
             Model <- glm(Label~.,family=binomial(link="logit"), data=TrainData)
             Pred.test <- predict(Model,TestData,type="response")
             CVinfo <- NA
           }
         },
         
         # Logistic regression + L1 regularization
         Lasso={
           library(glmnet)
           if (kfold>0){
             # select the optimal lambda
             Model.cv <- cv.glmnet(as.matrix(subset(TrainData,select=-c(Label))),as.vector(TrainData$Label),family="binomial",
                                   standardize=FALSE,nlambda=NumLambda,lambda=lambda,nfolds=kfold,...)
             lambda.optimal <- Model.cv$lambda.min
             CVinfo <- Model.cv
           }else{
             lambda.optimal <- lambda
             CVinfo <- NA
           }
           
           Model <- glmnet(as.matrix(subset(TrainData,select=-c(Label))),as.vector(TrainData$Label),family="binomial",
                           lambda=lambda.optimal,standardize=FALSE,...)
           Pred.test <- predict(Model,newx=as.matrix(subset(TestData,select=-c(Label))),type="response")
         },
         
         # Random Forest
         RF={
           library(randomForest)
           if (kfold>0){
             # select the optimal size of variable subset for each tree-growing
             CVtest.err <- matrix(0,nrow=length(NumVar),ncol=kfold)
             for (i in 1:length(NumVar)){
               for (j in 1:kfold){
                 testIdx <- cvFold[[j]]
                 Model.cv <- randomForest(Label~., data=TrainData[-testIdx,], mtry=NumVar[i], ntree=NumTree,...)
                 pred <- predict(Model.cv,TrainData[testIdx,],type="response")
                 CVtest.err[i,j] <- sum(TrainData$Label[testIdx]!=pred)/length(testIdx)
               }
             }
             mtry.optimal <- NumVar[which.min(rowMeans(CVtest.err))]
             CVinfo <- CVtest.err
           }else{
             mtry.optimal <- NumVar[1]
             CVinfo <- NA
           }
           
           Model <- randomForest(Label~., data=TrainData, mtry=mtry.optimal, ntree=NumTree, importance=FALSE,...)
           Pred.test <- predict(Model,TestData,type="prob") 
           Pred.test <- Pred.test[,2]
         },
         
         # Adaboost
         Adaboost={
           library(adabag)
           library(rpart)
           
           if (kfold>0){
             # select the optimal number of iterations
             CVtest.err <- rep(0,length(NumIter))
             for (i in 1:length(NumIter)){
               Model.cv <- boosting.cv(Label~., TrainData, v=kfold, boos=TRUE, mfinal=NumIter[i],coeflearn="Breiman", control=(cp=0.01),...)
               CVtest.err[i] <- Model.cv$error 
             }
             NumIter.optimal <- NumIter[which.min(CVtest.err)]
             CVinfo <- CVtest.err
           }else{
             NumIter.optimal <- NumIter[1]
             CVinfo <- NA
           }
           
           Model <- boosting(Label~., data=TrainData, boos=TRUE, mfinal=NumIter.optimal, coeflearn='Breiman',control=(cp=0.01),...)
           Pred.test <- predict(Model,TestData)
           Pred.test <- Pred.test$prob[,2]
         },
         
         # SVM 
         SVM={
           library(e1071)
           if (SVMkernel=="linear"){
             if (kfold>0){
               Model.cv <- tune.svm(Label~., data=TrainData, cost=Cseq,scale=TRUE,type="C-classification",kernel="linear",
                                    tunecontrol=tune.control(sampling="cross",cross=kfold,best.model=FALSE,performances=TRUE),...)
               C.optimal <- Model.cv$best.parameters$cost
               CVinfo <- Model.cv
             }else{
               C.optimal <- Cseq[1]
               CVinfo <- NA
             }
             
             Model <- svm(Label~., data=TrainData,scale=TRUE,type="C-classification",
                          cost=C.optimal,kernel="linear",probability=TRUE,fitted=FALSE,...)       

           }else if (SVMkernel=="polynomial"){
             if (kfold>0){
               Model.cv <- tune.svm(Label~.,data=TrainData,cost=Cseq,degree=dSeq,scale=TRUE,type="C-classification",kernel="polynomial",
                                    tunecontrol=tune.control(sampling="cross",cross=kfold,best.model=FALSE,performances=TRUE),...) 
               C.optimal <- Model.cv$best.parameters$cost
               d.optimal <- Model.cv$best.parameters$degree
               CVinfo <- Model.cv
             }else{
               C.optimal <- Cseq[1]
               d.optimal <- dseq[1]
               CVinfo <- NA
             }
             
             coef0.optimal <- 1
             Model <- svm(Label~., data=TrainData,scale=TRUE,type="C-classification",cost=C.optimal,kernel="polynomial",
                          degree=d.optimal,coef0=coef0.optimal,probability=TRUE,fitted=FALSE)
             
           }else if (SVMkernel=="rbf"|SVMkernel=="radial"){
             if (kfold>0){
               Model.cv <- tune.svm(Label~.,data=TrainData,cost=Cseq,gamma=gammaSeq,scale=TRUE,type="C-classification",kernel="radial",
                                    tunecontrol=tune.control(sampling="cross",cross=kfold,best.model=FALSE,performances=TRUE),...)    
               C.optimal <- Model.cv$best.parameters$cost
               gamma.optimal <- Model.cv$best.parameters$gamma
               CVinfo <- Model.cv
             }else{
               C.optimal <- Cseq[1]
               gamma.optimal <- gammaSeq[1]
               CVinfo <- NA
             }
             
             Model <- svm(Label~., data=TrainData,scale=TRUE,type="C-classification",
                          cost=C.optimal,kernel="radial",gamma=gamma.optimal,probability=TRUE,fitted=FALSE,...)
           }
           Pred.test <- predict(Model,TestData,probability=TRUE)
           Pred.test <- attr(Pred.test, "probabilities")
           Pred.test <- Pred.test[,2]
         },
         
         # GAM
         GAM={
           library(mgcv)
           Model <- gam(gamFormula,family=binomial(link="logit"), data=TrainData,method="GCV.Cp",...)
           Pred.test <- as.vector(predict(Model,TestData,type="response"))  
           CVinfo <- NA
         },
         
         # (single hidden layer) Neural Network
         sNN={
           library(nnet)
           if (kfold>0){
             Model.cv <- tune.nnet(Label~.,data=TrainData,size=NumUnit,decay=WgtDecaySeq,entropy=TRUE,trace=FALSE,MaxNWts=2000,
                                   tunecontrol=tune.control(sampling="cross",cross=kfold,nrepeat=1,best.model=FALSE,performances=TRUE),...)
             Unit.optimal <- Model.cv$best.parameters$size
             WgtDecay.optimal <- Model.cv$best.parameters$decay
             CVinfo <- Model.cv
           }else{
             Unit.optimal <- NumUnit[1]
             WgtDecay.optimal <- WgtDecaySeq[1]
             CVinfo <- NA
           }
           
           Model <- nnet(Label~.,data=TrainData,size=Unit.optimal,decay=WgtDecay.optimal,entropy=TRUE,trace=FALSE,MaxNWts=2000,...)
           Pred.test <- predict(Model,TestData,type="raw")
         },
         
         # (multi-layer) Neural Network
         mNN={
           library(neuralnet)
           formula.nn <- as.formula(paste("Label ~", paste(colnames(subset(TrainData,select=-Label)), collapse = " + ")))
           TrainData$Label <- as.numeric(as.character(TrainData$Label))
           if (kfold>0){
             # select the optimal layer specification 
             CVtest.err <- matrix(0,nrow=length(hiddenArch),ncol=kfold)
             for (i in 1:length(hiddenArch)){
               for (j in 1:kfold){
                 testIdx <- cvFold[[j]] 
                 Model.cv <- neuralnet(formula.nn,data=TrainData[!testIdx,],hidden=hiddenArch[[i]],linear.output=FALSE,
                                       err.fct="ce",stepmax = 1e+04,...) 
                 
                 pred <- compute(Model.cv, subset(TrainData[testIdx,],select=-Label))$net.result
                 CVtest.err[i,j] <- sum(TrainData$Label[testIdx]!=ifelse(pred>=0.5, 1, 0))/length(testIdx)
               }
             }
             hidden.optimal <- hiddenArch[[which.min(rowMeans(CVtest.err))]]
             CVinfo <- CVtest.err
           }else{
             hidden.optimal <- hiddenArch  
             CVinfo <- NA
           }
           
           Model <- neuralnet(formula.nn,data=TrainData,hidden=hidden.optimal,linear.output=FALSE,
                              err.fct="ce",stepmax = 1e+04,...)
           Pred.test <- compute(Model, subset(TestData,select=-Label))$net.result
         },
         
         
         # deep learning
         # DBN (Stacked Restricted Boltzmann Machine + Deep Neural Net)
         DBN={
           library(deepnet)
           if (kfold>0){
             # select the optimal deep architecture 
             CVtest.err <- matrix(0,nrow=length(hiddenArch),ncol=kfold)
             for (i in 1:length(hiddenArch)){
               for (j in 1:kfold){
                 testIdx <- cvFold[[j]] 
                 Model.cv <- dbn.dnn.train(as.matrix(subset(TrainData[-testIdx,],select=-Label)), 
                                           as.numeric(as.vector(TrainData$Label[-testIdx])),hidden=hiddenArch[[i]],...) 
                 
                 CVtest.err[i,j] <- nn.test(Model.cv, as.matrix(subset(TrainData[testIdx,],select=-Label)),
                                            as.vector(TrainData$Label[testIdx]),t=0.5)
               }
             }
             hidden.optimal <- hiddenArch[[which.min(rowMeans(CVtest.err))]]
             CVinfo <- CVtest.err
           }else{
             hidden.optimal <- hiddenArch  
             CVinfo <- NA
           }
           Model <- dbn.dnn.train(as.matrix(subset(TrainData,select=-Label)), 
                                  as.numeric(as.vector(TrainData$Label)),hidden=hidden.optimal,...)
           Pred.test <- nn.predict(Model,as.matrix(subset(TestData,select=-Label)))
         },
         
         # Stacked AutoEncoder + Deep Neural Net
         SAE={
           library(deepnet)
           if (kfold>0){
             # select the optimal deep architecture 
             CVtest.err <- matrix(0,nrow=length(hiddenArch),ncol=kfold)
             for (i in 1:length(hiddenArch)){
               for (j in 1:kfold){
                 testIdx <- cvFold[[j]] 
                 Model.cv <- sae.dnn.train(as.matrix(subset(TrainData[-testIdx,],select=-Label)), 
                                           as.numeric(as.vector(TrainData$Label[-testIdx])),hidden=hiddenArch[[i]],...) 
                 
                 CVtest.err[i,j] <- nn.test(Model.cv, as.matrix(subset(TrainData[testIdx,],select=-Label)),
                                            as.vector(TrainData$Label[testIdx]),t=0.5)
               }
             }
             hidden.optimal <- hiddenArch[[which.min(rowMeans(CVtest.err))]]
             CVinfo <- CVtest.err
           }else{
             hidden.optimal <- hiddenArch  
             CVinfo <- NA
           }
           Model <- sae.dnn.train(as.matrix(subset(TrainData,select=-Label)), 
                                  as.numeric(as.vector(TrainData$Label)),hidden=hidden.optimal,...)
           Pred.test <- nn.predict(Model,as.matrix(subset(TestData,select=-Label)))
           
         }
         
         # end of switch
  )
  
  return(list(Model=Model,PredTest=Pred.test,CV=CVinfo))
}
