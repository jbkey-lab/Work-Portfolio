

cores=detectCores()

cl <- makeCluster(cores[1]-2)
registerDoParallel(cl)

Test.Final = Test.done
Validate.Final = Validate.done.sample[,-"Yield"]
Train.Final = Train.done.sample[,-"Yield"]

models.list <- caretList(
  x=Train.Final,
  y=as.numeric(Train.done.sample$Yield),
  continue_on_fail = T,
  trControl=trainControl(method="cv", index = createFolds(as.numeric(Train.done.sample$Yield), 2),
                         savePredictions=TRUE,allowParallel = TRUE),
  #tuneList=list(#blassoaveraged=caretModelSpec(method="blassoAveraged", thin=3),
  #blasso=caretModelSpec(method="blasso",  thin=3),
  #enet=caretModelSpec(method="enet", thin=3),
  #xgbTree = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 8)
  #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic), ## Elastic, highly correlated with lasso and ridge regressions
  #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
  #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge) 
  #)
  methodList=c(
    #"glmnet",# okay
    #"rf",
   # "nodeHarvest"
    #"xgbDART" ,
    #"xgbTree",
    #"gcvEarth",
    #"gbm" #okay
  )
  
) 

invisible(gc())
models.list

#NCAA.stacked<-caretStack(models.list, method="glm");NCAA.stacked
models<-caretEnsemble(models.list,trControl=trainControl(
  number=100,
  method = "boot",
  verboseIter = TRUE
))
