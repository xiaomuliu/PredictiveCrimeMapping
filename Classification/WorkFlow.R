setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/PredictiveMapping/Code/Classification/")

PredTarget <- "Burglary"

source("SetupGrid.R")
load("SpatialFeatureMap_Res660ft.RData")

source("SetupCrimeData.R")

source("SetupKernels.R")

source("PullRawData.R")

source("StructureData.R")

# Pedictive model training and testing
source("EvaluationFunction.R")
percentArea <- seq(0,1,length.out=21)

source("EvalHotspot.R")

source("TrainAndEvalClassifier.R")

source("PlotEval.R")

source("VisualizePred.R")
