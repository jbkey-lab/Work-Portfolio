######################################################################################################
######Rscript for Half-Sib (HS) Quanitative Trait Loci (QTL)
######################################################################################################
rm(list = ls()) #remove environment vairalbes
invisible(gc(reset = T)) #cleans memory "garbage collector"
memory.limit(size = 64000)
setwd("R:/Breeding/MT_TP/Models/QTL")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################
library("dplyr")
library("data.table")
#library("pastecs")
#library("asreml")
#library("tidyr")
#library("tidyverse")
#library("xlsx")
#library("stats")
#library("qtl")
#library("snow") #run qtl in parallel
library("doParallel")
library("caret")
library("caretEnsemble")
library("impute")

#########################Get data
hdp = "/media/jacoblamkey/Storage/plt and ear ht qtl/" #get the data frames
#hdp = "R:/Breeding/MT_TP/Models/QTL/2020/" #get the data frames
#get the data frames

#setwd("C:/Users/jake.lamkey") #folder where you want files 
outpath = "/media/jacoblamkey/Storage/plt and ear ht qtl/"
#outpath = "R:/Breeding/MT_TP/Models/QTL/2020/Half-Sib results/"

######################################################################################################

plt.ss = fread(paste0(hdp,"genos.pltht.done.SS.csv"))
plt.ns = fread(paste0(hdp,"genos.pltht.done.NS.csv"))
ear.ss = fread(paste0(hdp,"genos.earht.done.SS.csv"))
ear.ns = fread(paste0(hdp,"genos.earht.done.NS.csv"))
#covarites, more covarites need to be run in the QTL_org.R script

pltssCov = plt.ss[-c(1,2),c(1:170)] %>% mutate_all(as.factor) %>% data.frame()
pltnsCov = plt.ns[-c(1,2),c(1:170)] %>% mutate_all(as.factor) %>% data.frame()
earssCov = ear.ss[-c(1,2),c(1:170)] %>% mutate_all(as.factor) %>% data.frame()
earnsCov = ear.ns[-c(1,2),c(1:170)] %>% mutate_all(as.factor) %>% data.frame()

pltssCov = pltssCov[, which(colMeans(pltssCov == 0) < 1)]
pltnsCov = pltnsCov[, which(colMeans(pltnsCov == 0) < 1)]
earssCov = earssCov[, which(colMeans(earssCov == 0) < 1)]
earnsCov = earnsCov[, which(colMeans(earnsCov == 0) < 1)]

covars = c(list(pltssCov), list(pltnsCov), list(earssCov), list(earnsCov))

act.df = c(list(plt.ss), list(plt.ns), list(ear.ss), list(ear.ns))

####################################
set.seed(2020)
ncol(plt.ss)
ncol(plt.ns)
ncol(ear.ss)
ncol(ear.ns)

nrow(plt.ss)
nrow(plt.ns)
nrow(ear.ss)
nrow(ear.ns)

genotype = data.frame(plt.ss[-c(1:2),c(177:6921)],check.names = F)


genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 1

genotype = genotype[vapply(genotype, function(x) length(unique(x)) > 1, logical(1L))]
genotype = genotype[, which(colMeans(!is.na(genotype)) >= 0.8)]
genotype = genotype[, which(colMeans(genotype == 0,na.rm=T) >= 0.32)]
genotype = genotype[, which(colMeans(genotype == 1,na.rm=T) >= 0.32)]

genotype = as.matrix(genotype)

X.knn= impute.knn(genotype, k=10, colmax = .80)
geno.imp.plt.ss = X.knn$data

####
genotype = data.frame(plt.ns[-c(1:2),c(177:8641)],check.names = F)
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 1

genotype = genotype[vapply(genotype, function(x) length(unique(x)) > 1, logical(1L))]
genotype = genotype[, which(colMeans(!is.na(genotype)) >= 0.8)]
genotype = genotype[, which(colMeans(genotype == 0,na.rm=T) >= 0.32)]
genotype = genotype[, which(colMeans(genotype == 1,na.rm=T) >= 0.32)]

genotype = as.matrix(genotype)

X.knn= impute.knn(genotype, k=10)
geno.imp.plt.ns = X.knn$data

####
genotype = data.frame(ear.ss[-c(1:2),c(177:6921)],check.names = F)
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 1
genotype = genotype[vapply(genotype, function(x) length(unique(x)) > 1, logical(1L))]
genotype = genotype[, which(colMeans(!is.na(genotype)) >= 0.8)]
genotype = genotype[, which(colMeans(genotype == 0,na.rm=T) >= 0.32)]
genotype = genotype[, which(colMeans(genotype == 1,na.rm=T) >= 0.32)]

genotype = as.matrix(genotype)

X.knn= impute.knn(genotype, k=10)
geno.imp.ear.ss = X.knn$data

####
genotype = data.frame(ear.ns[-c(1:2),c(177:8641)],check.names = F)
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 1
genotype = genotype[vapply(genotype, function(x) length(unique(x)) > 1, logical(1L))]
genotype = genotype[, which(colMeans(!is.na(genotype)) >= 0.8)]
genotype = genotype[, which(colMeans(genotype == 0,na.rm=T) >= 0.32)]
genotype = genotype[, which(colMeans(genotype == 1,na.rm=T) >= 0.32)]

genotype = as.matrix(genotype)

X.knn= impute.knn(genotype, k=10)
geno.imp.ear.ns = X.knn$data


markerDF = c(list(geno.imp.plt.ss), list(geno.imp.plt.ns), list(geno.imp.ear.ss), list(geno.imp.ear.ns))
############################################################

phenotypes.plt.ns = data.frame(plt.ns[-c(1:2), 174])
phenotypes.plt.ss = data.frame(plt.ss[-c(1:2), 174])
phenotypes.ear.ns = data.frame(ear.ns[-c(1:2), 174])
phenotypes.ear.ss = data.frame(ear.ss[-c(1:2), 174])

y.plt.ns = as.array(phenotypes.plt.ns$Plt.Height)
y.plt.ss = as.array(phenotypes.plt.ss$Plt.Height)
y.ear.ns = as.array(phenotypes.ear.ns$EarHt)
y.ear.ss = as.array(phenotypes.ear.ss$EarHt)

samplesize = 100
fold = 2
plt.ss.name = plt.ss[-c(1,2),176]
plt.ns.name = plt.ns[-c(1,2),176]
ear.ss.name = ear.ss[-c(1,2),176]
ear.ns.name = ear.ns[-c(1,2),176]

#########################################################################
#choose about 20% of data as missing values by pedid group so there is no bias with genotypes that are the same
test.size = .05
(ids.plt.ns <- sample(unique(plt.ns.name$name), length(unique(plt.ns.name$name))*test.size ))
(ids.plt.ss <- sample(unique(plt.ss.name$name), length(unique(plt.ss.name$name))*test.size ))
(ids.ear.ns <- sample(unique(ear.ns.name$name), length(unique(ear.ns.name$name))*test.size ))
(ids.ear.ss <- sample(unique(ear.ss.name$name), length(unique(ear.ss.name$name))*test.size ))

test.plt.ns = left_join(data.frame(plt.ns[-c(1,2),176], rows=c(1:nrow(plt.ns[-c(1,2),]))), 
                        data.frame(ids.plt.ns,yes = rep.int(1, length(ids.plt.ns))), 
                        by = c("name"="ids.plt.ns")) %>% filter(yes==1)

test.plt.ss = left_join(data.frame(plt.ss[-c(1,2),176], rows=c(1:nrow(plt.ss[-c(1,2),]))), 
                        data.frame(ids.plt.ss,yes = rep.int(1, length(ids.plt.ss))), 
                        by = c("name"="ids.plt.ss")) %>% filter(yes==1)

test.ear.ns = left_join(data.frame(ear.ns[-c(1,2),176], rows=c(1:nrow(ear.ns[-c(1,2),]))), 
                        data.frame(ids.ear.ns,yes = rep.int(1, length(ids.ear.ns))), 
                        by = c("name"="ids.ear.ns")) %>% filter(yes==1)

test.ear.ss = left_join(data.frame(ear.ss[-c(1,2),176], rows=c(1:nrow(ear.ss[-c(1,2),]))), 
                        data.frame(ids.ear.ss,yes = rep.int(1, length(ids.ear.ss))), 
                        by = c("name"="ids.ear.ss")) %>% filter(yes==1)

whichNa.plt.ns = test.plt.ns$rows
whichNa.plt.ss = test.plt.ss$rows
whichNa.ear.ns = test.ear.ns$rows
whichNa.ear.ss = test.ear.ss$rows

#######################################################################
yNA.plt.ns = y.plt.ns
yNA.plt.ss = y.plt.ss
yNA.ear.ns = y.ear.ns
yNA.ear.ss = y.ear.ss

yNA.plt.ns[whichNa.plt.ns] = NA
yNA.plt.ss[whichNa.plt.ss] = NA
yNA.ear.ns[whichNa.ear.ns] = NA
yNA.ear.ss[whichNa.ear.ss] = NA

y = c(list(y.plt.ss), list(y.plt.ns), list(y.ear.ss), list(y.ear.ns))
yNA = c(list(yNA.plt.ss), list(yNA.plt.ns), list(yNA.ear.ss), list(yNA.ear.ns))
NAwhiches = c(list(whichNa.plt.ss), list(whichNa.plt.ns), list(whichNa.ear.ss), list(whichNa.ear.ns))

#library(doParallel)
#cl <- makePSOCKcluster(19)
#registerDoParallel(cl)
#yNA.df = yNA[df][[1]]
df=2

y.df = y[df][[1]]
coVar = data.frame(covars[df])
marker.df = markerDF[df][[1]]
whichNa = NAwhiches[df][[1]]

#################train dataframe
tets=cbind(coVar[-whichNa,], marker.df[-whichNa,])
tets[] <- lapply(tets, as.character)
tets[] <- lapply(tets, as.numeric)

##############3test dataframe
tets.test=cbind(coVar[whichNa,], marker.df[whichNa,])
tets.test[] <- lapply(tets.test, as.character)
tets.test[] <- lapply(tets.test, as.numeric)

#raw.data = act.df[df][[1]]
cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
methodList=c(
  #"glmnet",
  #"rf",
  #"nodeHarvest", 
  #"xgbDART" ,
  #"xgbTree",
  #"gcvEarth",
  "blasso"
  #"blassoaveraged", 
  #"gbm"
  
  #"gbm",
  #"naive_bayes",
  #"glm.nb",
  #'svmLinear3',
  #"BstLm",###
  #"neuralnet",
  #"pcaNNet",
  #"kernelpls",
  #"nnls",
  #"rpart",
  #"gaussprLinear",
 #"kknn",
 # "leapSeq",
  #"avNNet",
  #"mlpWeightDecay",
  #"nnet",
  #"nnls",
 # "parRF",
  #"qrf", #takes a long time
  #"ranger",
  #"penalized",
  #"rqlasso",
  #"ordinalNet",
  #"RRFglobal" ##
  #"relaxo",
  #"foba",
  #"evtree"
  
)#0.99
xgbTreeGrid <- expand.grid(nrounds = 1000, max_depth = 2, eta = 0.1, gamma = 0, colsample_bytree = 1.0,  subsample = 1.0, min_child_weight = 4)
glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda =  seq(0.1, 1.5,by = .1)) ## notice the . before the parameter
glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.1, 1.5,by = .1))
glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.1, 1.5,by = .1))

set.seed(33)#32
models.list <- caretList(
  x=tets,
  y=as.integer(y.df[-whichNa]),
  continue_on_fail = T,
  #metric="RMSE",
  trControl=trainControl(method="cv", index = createFolds(as.integer(y.df[-whichNa], 10)),
                         savePredictions=TRUE,  allowParallel = TRUE),
  tuneList=list(#blassoaveraged=caretModelSpec(method="blassoAveraged", thin=3),
                #blasso=caretModelSpec(method="blasso",  thin=3),
                #enet=caretModelSpec(method="enet", thin=3),
                #xgbTree = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 16)
                #elastic=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic),## Elastic, highly correlated with lasso and ridge regressions
                #lasso=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
                #ridge=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge) 
  ),
  methodList = methodList
) #.56 and .68



#stopCluster(cl)
models.list

#NCAA.stacked<-caretStack(models.list, method="glm");NCAA.stacked
models<-caretEnsemble(models.list,trControl=trainControl(
  #number=10,
  method = "cv",
  verboseIter = TRUE
))

#######residual
#tmp$residual <- tmp$SalePrice - tmp$pred

###########feature importance
featureImp <- varImp(models$models$xgbTree)

ggplot(featureImp, mapping = NULL,
       
       top = dim(featureImp$importance)[1]-(dim(featureImp$importance)[1]-50), environment = NULL) +
  
  xlab("Feature") +
  
  ylab("Importace") +
  
  theme(text = element_text(size=9))
#--------------------------------------------------------------------------------------------------
featureImp <- varImp(models$models$xgbTree)

featureImp = setDT(featureImp$importance, keep.rownames= T)[]
featureImp = featureImp[1:50,]
tets.refined = marker.df[-whichNa, ]
tets.refined = tets.refined[, featureImp$rn]

tets.refined=cbind(coVar[-whichNa,], tets.refined)
tets.refined[] <- lapply(tets.refined, as.character)
tets.refined[] <- lapply(tets.refined, as.numeric)

##############3test dataframe
test.refined.test = marker.df[whichNa, ]
test.refined.test = test.refined.test[, featureImp$rn]

test.refined.test=cbind(coVar[whichNa,], test.refined.test)
test.refined.test[] <- lapply(test.refined.test, as.character)
test.refined.test[] <- lapply(test.refined.test, as.numeric)

tets.refined = as.matrix(tets.refined)
test.refined.test = as.matrix(test.refined.test)
set.seed(33)#32
models.list <- caretList(
  x=tets.refined,
  y=as.integer(y.df[-whichNa]),
  continue_on_fail = T,
  trControl=trainControl(method="CV", index = as.integer(y.df[-whichNa]),
                         savePredictions=TRUE,  allowParallel = TRUE),
  tuneList=list(#blassoaveraged=caretModelSpec(method="blassoAveraged", thin=3),
    #blasso=caretModelSpec(method="blasso",  thin=3),
    #enet=caretModelSpec(method="enet", thin=3),
    #xgbTree = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 8)
    glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic) ## Elastic, highly correlated with lasso and ridge regressions
    #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
    #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge) 
  )
) 

methodList=c(
  "gbm",
  "naive_bayes",
  "glm.nb",
  'svmLinear3',
  "BstLm",
  "neuralnet",
  "pcaNNet",
  "kernelpls",
  "nnls",
  "rpart",
  "gaussprLinear",
  "kknn",
  "leapSeq",
  "avNNet",
  "mlpWeightDecay",
  "nnet",
  "nnls",
  "parRF",
  "qrf",
  "ranger",
  "penalized",
  "rqlasso",
  "ordinalNet",
  "RRFglobal",
  "relaxo",
  "foba",
  "evtree"
  
)#0.99
#stopCluster(cl)
models.list

#NCAA.stacked<-caretStack(models.list, method="glm");NCAA.stacked
models<-caretEnsemble(models.list,trControl=trainControl(
  #number=10,
  method = "cv",
  verboseIter = TRUE
))

#######residual
#tmp$residual <- tmp$SalePrice - tmp$pred

###########feature importance
featureImp <- varImp(models$models$xgbTree)

ggplot(featureImp, mapping = NULL,
       
       top = dim(featureImp$importance)[1]-(dim(featureImp$importance)[1]-50), environment = NULL) +
  
  xlab("Feature") +
  
  ylab("Importace") +
  
  theme(text = element_text(size=9))



preds<-predict(object=models, tets.test)
cor(y.df[whichNa], preds)^2

preds<-predict(object=models, tets)
cor(y.df[-whichNa], preds)^2




stopCluster(cl)



















