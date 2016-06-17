setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Code/Classification/")

PredTarget <- "Burglary"

source("SetupGrid.R")
load("SpatialFeatureMap_Res660ft.RData")

source("SetupCrimeData.R")

source("SetupKernels.R")

source("PullRawData.R")

source("StructureData.R")

# Pedictive model training and testing
# ISSUE: expandsion and compression of the probability scales in different regions
# The pixel-ranking evalution is equivalent to mapping the pixel values to a unified scale,
# e.g.,[0,1], then doing the conventional ROC analysis

# Method 1: Hit rate (TPR) is calcuated for every time interval. So each time interval evaluation is associated with an ROC
# source("EvaluationFunction.R")
# percentArea <- seq(0,1,length.out=21)
# 
# source("EvalHotspot.R")
# 
# source("TrainClassifier.R")
# 
# source("EvalClassifier.R")
# 
# source("PlotEval.R")
# 
# source("VisualizePred.R")

# Method 2: Hit rate (TPR) is not calcuated for every time interval but instead for all test examples. 
# The "output" of hotspot models should be normalized to have summation equal to 1
# (i.e. density values instead of intensity values) for each time interval if appying this evaluation
# The pixel-ranking evalution is equivalent to mapping the pixel values to a unified scale,
# e.g.,[0,1], then doing the conventional ROC analysis

# significant test (ROC curves with error bars)
source("TrainClassifier.R")
source("ROCfunction.R")
source("EvalClassifier2.R")
source("EvalHotspot2.R")
source("PlotEval2.R")
source("VisualizePred.R")