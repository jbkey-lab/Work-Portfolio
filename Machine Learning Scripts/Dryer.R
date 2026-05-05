# %% [markdown]
# This is a starter R markdown kernel for baseline prediction using a glmnet model for the OSIC competition **using only the tabular data** supplied. It is based on [OSIC starter in R (glmnet)](https://www.kaggle.com/lgreig/osic-starter-in-r-glmnet)


# %% [code] {"_kg_hide-output":true}
library(tidyverse)
library(magrittr)
library(onehot)
library(repr)
library(caret)
library(dplyr)
#library(plotly)
#install.packages("monomvn")


#library(devtools)

library(caretEnsemble)
#install.packages("nodeHarvest")
#library("nodeHarvest")

options(warn = -1, repr.plot.width = 14, repr.plot.height =  8)

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
#3133382403 demetrios mastoros @quickenloans.com
# %% [markdown]
# Loading data:

# %% [code]
train <- read_csv('/home/jacoblamkey/Downloads/archive/Drying formula -sub.csv')
head(train)

cat('there are',nrow(train),'rows in train \n')
colnames(train)[2]="ID"
# %% [code]
#test <- read_csv('../input/osic-pulmonary-fibrosis-progression/test.csv')
test = train %>% filter(Datetype == "Test")
head(test)
cat('there are',nrow(test),'rows in test \n')

# %% [code]
sub <- read_csv('/home/jacoblamkey/Downloads/archive/Drying formula -submission.csv')
head(sub)

cat('there are',nrow(sub),'rows in sub \n')

# %% [markdown]
# Combining data:

# %% [code]
#Combine train and test
full <- train
full = full[-c(1:5),]

rm(train,test)

full2 = full %>% mutate(ID = factor(ID), Year = factor(Year), tempF = factor(tempF))


summary(full2)

# %% [markdown]
# Some plots:

# %% [code]
ggplot(full2 %>% select(ID) %>% group_by(ID) %>% mutate(count = n()), 
       aes(x = factor(count))) + geom_bar() + labs(x = 'Counts per ID', y = 'ID whom have it') + theme_bw()

# %% [code]
#ggplot(full2, aes(x = SmokingStatus)) + facet_grid(cols = vars(Sex)) + geom_bar() + theme_bw()

# %% [code]
ggplot(full2, aes(x = Moisture)) + geom_density() + theme_bw()

# %% [code]
ggplot(full2, aes(x = Days)) + geom_density() + theme_bw()

# %% [code]
ggplot(full2, aes(x = chgMoist)) + geom_density() + theme_bw()

# %% [markdown]
# Data preparation:

# %% [code]
#full2 %<>% mutate(AgeGroup = factor(cut(Age, breaks = 2)))

#head(full2)

# %% [code]
#Generate some features

IS =list()

#Days_Elapsed<-c()
Mositure<-c()
chgMoist<-c()
Initial_Day<-c()


for (i in 1:nrow(full))
{
  entry=as.character(full[i, 1])
  if (is.null(IS[[entry]]))
  {
    IS[[entry]]<-c(full[i, 2],full[i, 3], full[i, 4] )
    Initial_Day[i]<- IS[[entry]][1]
    Mositure[i]<- IS[[entry]][2]
    chgMoist[i]<- IS[[entry]][3]
    #Days_Elapsed[i]<-full[i, 2] - Initial_Day[i]
  } else {
    Initial_Day[i]<- IS[[entry]][1]
    Mositure[i]<- IS[[entry]][2]
    chgMoist[i]<- IS[[entry]][3]
    #Days_Elapsed[i]<-full[i, 2] - Initial_Day[i]
  }
}

# %% [code]
#Attach features

full2$Moisture_init<-Mositure
#full2$Percent_init<-Moisture
#full2$Week_num<-Weeks_Elapsed
tail(full2)

rm(full)
rm(Weeks_Elapsed, FVC,Percent,Initial_Week)

# %% [code]
#Convert the categorical variables into dummy varaibles

#tmp = full2[, c(5, 6, 7)]
tmp = full2[, c(1,9)]

#fit=OneHotEncoder.fit(tmp)
#out = transform(fit, tmp)

encoder <- onehot(tmp)
cat('encoder created \n')
#head(full)
out <- predict(encoder, tmp)
#head(output)
colnames(out)[1:6]<-c(2018,2019,2020,90,97,94)

cat('output created \n')

#Create a new dataframe with all the information
full2 <- cbind(full2, out) %>% select(-Year, #-Age,  
                                        -tempF,-totalDays,-chgMoist,-chgMoistPerDay,-Date) %>% 
  mutate(Moisture_init = as.numeric(Moisture_init), 
         Days = as.numeric(Days))
        # Percent_init = as.numeric(Percent_init))

rm(tmp,encoder,out)

head(full2)

summary(full2)

# %% [markdown]
# It's our metric function:

# %% [code]
# create a metric function for validating accuracy

metric <- function(Y_pred, Y_act, sigma)
{
  sigma.clipped <- ifelse(sigma>70, sigma, 70)
  delta <- ifelse(abs(Y_pred- Y_act)>1000, 1000, abs(Y_pred- Y_act))
  custom.metric <- -(sqrt(2)*delta /sigma.clipped ) - log(sqrt(2)*sigma.clipped)
  return(mean(custom.metric))
}

# %% [markdown]
# Train, test, valifation datasets:

# %% [code]
# train and valid datasets

train.new<- full2 %>% filter(Datetype == "Train")
test <- full2 %>% filter(Datetype == "Test")
train.new = train.new[,-4]
test = test[,-4]

#head(train.new)
#head(test)
##################################################################

X = full2 %>% select(-ID, -Moisture)

y = full2$Moisture

# Generate submission file for prediction:

# %% [code]
test_names = test$ID
test_vals = test$Moisture
# %% [code]

sub_ <- test[1, ]

#print(sub_)

#test_test = read_csv('../input/osic-pulmonary-fibrosis-progression/test.csv')

#test_test
#for (i in seq(1, 1, by=1))
#{
  
##  for (j in 1:nrow(test))
 # {
#    new.row <- test[j,]
##    new.row$Day_num <- i - IS[[test[j,1]]]$Days #test_test$Weeks[j]
#    sub_ <-rbind(sub_, new.row)
#  }
#}

#sub_ <- sub_[2:nrow(sub_),]
#head(sub_)

# %% [code]
####################### ########################################
#load model
################################################################
set.seed(33)#32
models.list <- caretList(
  x=X,
  y=as.integer(full2$Moisture),
  continue_on_fail = T,
  trControl=trainControl(method="CV",classProbs = T,repeats=10, index = createFolds(as.integer(full2$Moisture), 20),
                         savePredictions=TRUE),
  #  #tuneList=list(nnet.f=caretModelSpec(method="nnet", trace=FALSE, tuneLength=1)),
  #rf.f=caretModelSpec(method="rf", trace=T, tuneLength=5),
  #gbm.f=caretM"naive_bayes,odelSpec(method="gbm", tuneLength=5),
  methodList=c(#"glmnet", #+
               #"glm",
               #'bagEarth', #Bagged MARS
               #"enet", #Elasticnet 
               #"gaussprPoly" #Gaussian Process with Polynomial Kernel
               #"brnn", #bayesian additive regression trees
               #"bayesglm" #bayesian glm
               #"blassoAveraged", #+ #bayesian lasso
               #"blasso" #+ #bayesian lasso
               
               ###
               'cforest',
              'svmLinear3',
               "bridge",
               "BstLm",
               "rpart",
               "gaussprLinear",
               #"kknn",
               "leapSeq",
               "avNNet",
               "mlpWeightDecay",
               "nnet",
               "nnls",
               "parRF",
               "widekernelpls",
               "pcr",
               "qrf",
               "ranger",
               "rf",
               "extraTrees",
               "RRFglobal",
               "relaxo",
               "ridge",
               "svmLinear",
               "svmPoly",
               "evtree"
               
               ##
               ##"wsrf"
               ##"glmnet_h2o",
               ##"svmRadialWeights",
               ##"chaid", No
               ##"dda", NO
               ##"dwdPoly", NO
               ##"gbm_h2o", N0
               ##"lssvmRadial", NO
               ##"svmLinearWeights", NO
               ##"naive_bayes", no
               ##"svmLinearWeights2",
               ##"lssvmLinear",
               ##"lssvmPoly",
               ##"qrnn", time
               ##"WM", time
               ##"randomGLM", TIME
               ####
               ####"elm",
               ####"gamSpline",
               ####"gpls",
               ####"mlpKerasDropout",
               ####"mxnet",
               ####"rqlasso"
               ####"Rborist"
               ####"vbmpRadial"
  )) #0.99

models.list

NCAA.stacked<-caretStack(models.list, method="glm");NCAA.stacked
models<-caretEnsemble(models.list,trControl=trainControl(
  #number=10,
  method = "cv",
  verboseIter = TRUE
))

#saveRDS(models, "models_mix.rds")

#models<-readRDS("../input/models/models_mix.rds")
sub.forPred = sub_[,-c(1,2)]

preds<-predict(object=models, sub.forPred)


metrics<-c()
range_std = 1:400

for (i in 1:length(range_std))
{
  metrics[i]<- metric(Y_act= sub_[,2], 
                      Y_pred=preds, 
                      range_std[i])
}

plot(y=metrics, x=range_std,main = "Full" )



sub$FVC_pred <- preds
sub$Confidence_pred<-rep(range_std[which.max(metrics)[1]], #as.numeric(stdev.opt[[best_method]]), 
                         times=nrow(sub))
head(sub)
rm(metrics,sub_,preds)
invisible(gc())

ls()

# %% [code]
#Also replace predictions with known data
#Set confidence to 70

for(i in 1:nrow(test)){    
  sub[sub$Patient_Week == test_names[i],]$FVC_pred <- test_vals[i]
  sub[sub$Patient_Week ==  test_names[i],]$Confidence_pred<- 70
}


# %% [code]
#Make submission

sub$FVC <- sub$FVC_pred
sub$Confidence <- sub$Confidence_pred
sub<-sub[, c(1, 2, 3)]
head(sub)
rm(test)
invisible(gc())

write_csv(sub, 'submission.csv')
