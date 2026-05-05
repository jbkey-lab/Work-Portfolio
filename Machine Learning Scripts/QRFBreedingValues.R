
library(data.table)
library(dplyr)
#library(onehot)
library(doParallel)
library(caret)
library(caretEnsemble)
library(stats)
# require("BGLR")
# library("coda")
# library("MCMCpack")
library("impute")
library(keras)
#library(gbm)
library(RcppCNPy)
library(tidyr)
#library(abind)
#####################################################
cores=detectCores()
cl <- makeCluster(cores[1]-1, outfile="")
registerDoParallel(cl)

# hdp = "C:/Users/jake.lamkey/Documents/"
# 
# dp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Training/"
# tdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/Test Inputs/"
# gdp = "C:/Users/jake.lamkey/Documents/Dataset_Competition_Zip_File/Dataset_Competition/"
hdp = "/media/jacoblamkey/Storage/cropchallenge/"

dp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/Training/"
tdp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/Test Inputs/"
gdp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/"

#####################################################

o.train <- fread(paste0(dp,"inputs_others_train.csv"))[,-1]
#w.train <- fread(paste0(dp,"inputs_weather_train.csv"))[,-1]
y.train <- fread(paste0(dp,"yield_train.csv"))[,-1]

#w.test <- fread(paste0(tdp,"inputs_weather_test.csv"))[,-1]
o.test <- fread(paste0(tdp,"inputs_others_test.csv"))[,-1]

geno <- fread(paste0(gdp,"clusterID_genotype.csv"))[-1788,-1]


######################################################

trainingx2 = fread(paste0(hdp,"BV.HSIdentical.df.csv"))

field = trainingx2[!duplicated(trainingx2$FIELD), "FIELD"]
field$num = c(9501:(nrow(field)+9500))
trainingx2 = left_join(trainingx2, field, by="FIELD")
colnames(trainingx2)[34] = "Loc"

field = trainingx2[!duplicated(trainingx2$LINE), "LINE"]
field$num = c(95001:(nrow(field)+95000))
trainingx2 = left_join(trainingx2, field, by="LINE")
colnames(trainingx2)[35] = "ID"
colnames(trainingx2)[26] = "Year"
set.seed(8042)
trainingx2 = na.omit(trainingx2[,c(22,34,35,26)])

ID = trainingx2[!duplicated(trainingx2[,3]), 3]
ID = data.frame(ID)

idx = sample(nrow(ID), nrow(ID) * 0.25) #.25

Loc_Validate = data.frame(ID=ID[-idx, ])
Loc_Train = data.frame(ID=ID[idx, ])

ID = o.train[!duplicated(o.train[,2]), 2]
ID = data.frame(ID)

idx = sample(nrow(ID), nrow(ID) * 0.9) #.9

Loc_Validatesoy = data.frame(ID=ID[-idx, ])
Loc_Trainsoy = data.frame(ID=ID[idx, ])

trainx1 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Trainsoy, by="ID")

trainx2 = inner_join(trainingx2, Loc_Train, by="ID") %>% rbind(trainx1)
validatex2 = inner_join(trainingx2, Loc_Validate, by="ID")
validatex3 = inner_join(data.frame(y.train, o.train[ ,c(5,2,4)]), Loc_Validatesoy, by="ID")

trainx1 = data.frame(trainx1)  %>% mutate_all(as.numeric) 
trainx2 = data.frame(trainx2)  %>% mutate_all(as.numeric) #%>% unique() 
validatex2 = data.frame(validatex2)  %>% mutate_all(as.numeric)
validatex3 = data.frame(validatex3)  %>% mutate_all(as.numeric)


##################################################################
final_grid1 <- expand.grid(nrounds = 100000,eta = .8,max_depth = 10,gamma = 0,colsample_bytree = .9,min_child_weight = 1,subsample = 1)
# final_grid2 <- expand.grid(nrounds = 10000,eta = .01,max_depth = 8,gamma = 0,colsample_bytree = .9,min_child_weight = 1,subsample = 1)
 final_grid3 <- expand.grid(nrounds = 50000,eta = .01,max_depth = 10,gamma = 0,colsample_bytree = .9,min_child_weight = 1,subsample = 1) 
# final_grid4 <- expand.grid(nrounds = 10000,eta = .01,max_depth = 10,gamma = 0,colsample_bytree = .9,min_child_weight = 1,subsample = 1)

models.list2 <- caretList(
  x=trainx2[ , -c(1)],
  y=(trainx2[ , 1]),
  
  trControl=trainControl(method="cv",
                         number=1, #1
                         index = createFolds((trainx1[,2]), k=2), #2
                         savePredictions = TRUE, allowParallel = TRUE,
                         verboseIter = TRUE,
                         preProcOptions =  c( method = c("center", "scale"),
                                              thresh = 0.95,
                                              pcaComp = NULL,
                                              na.remove = TRUE,
                                              k = 5,
                                              knnSummary = mean,
                                              outcome = NULL,
                                              fudge = 0.2,
                                              numUnique = 3,
                                              verbose = FALSE,
                                              freqCut = 95/5,
                                              uniqueCut = 10,
                                              cutoff = 0.9,
                                              rangeBounds = c(0, 1)),
                         #p=.75
                         #seeds=c(1,2,3,4,5,6,7,8,9),
                         #indexFinal = length(sample(nrow(trainx2), (nrow(trainx2))*.3))
  ),
  tuneList=list(
     # qrf1=caretModelSpec(method="qrf", ntree=200, tuneLength = 1), #11
     # qrf2=caretModelSpec(method="qrf", ntree=150, tuneLength = 1), #11
    #  qrf3=caretModelSpec(method="qrf", ntree=100, tuneLength = 1), #9
    # qrf4=caretModelSpec(method="qrf", ntree=500, tuneLength = 1), #7
    qrf5=caretModelSpec(method="xgbTree", tuneGrid = final_grid1), #5
    # qrf6=caretModelSpec(method="xgbTree", tuneGrid = final_grid2), #5
      qrf7=caretModelSpec(method="xgbTree", tuneGrid = final_grid3) #5
    #  qrf8=caretModelSpec(method="xgbTree", tuneGrid = final_grid4) #5
    # #qrf6=caretModelSpec(method="qrf", ntree=2, tuneLength = 1) #5
  )
)

invisible(gc())

models.list2

 my_control <- trainControl(method = "cv", # for "cross-validation"
                           number = 100, # number of k-folds
                           #savePredictions = "final",
                           allowParallel = TRUE, verbose=T)
# 

NCAA.stacked<-caretEnsemble(models.list2,
                            trControl = my_control
                            );NCAA.stacked # + 95
# trControl = trainControl(
#   number=100, 
#   method="boot",
#   verboseIter =TRUE,
#   allowParallel = T
# ));NCAA.stacked # + 95

invisible(gc())

#######Validate Corn + 44
preds = predict(models.list2$qrf5, validatex2[,-1])
cor(validatex2[, 1], preds)^2
sqrt(mean((validatex2[, 1] -  preds)^2))

#######training set + 98.5
preds.t = predict(NCAA.stacked, trainx2[,-1])
cor(trainx2[, 1], preds.t)^2
sqrt(mean((trainx2[, 1] -  preds.t)^2))

#######Soybeans + 94.8
preds = predict(NCAA.stacked,  trainx1)
cor(data.frame(trainx1[,1]), (preds))^2
sqrt(mean((trainx1[, 1] -  preds)^2))

#######Validate Soybean 74
preds = predict(NCAA.stacked,  validatex3)
cor(data.frame(validatex3[,1]), (preds))^2
sqrt(mean((validatex3[, 1] -  preds)^2))


