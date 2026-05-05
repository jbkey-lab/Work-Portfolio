# %% [markdown]
# This is a starter R markdown kernel for baseline prediction using a glmnet model for the OSIC competition **using only the tabular data** supplied. It is based on [OSIC starter in R (glmnet)](https://www.kaggle.com/lgreig/osic-starter-in-r-glmnet)

# %% [code] {"_kg_hide-output":true}
library(tidyverse)
library(magrittr)
library(onehot)
library(repr)
library(caret)
library(doParallel, quietly=T, warn.conflicts=F)

#library(plotly)
#install.packages("caretEnsemble")
library(caretEnsemble)
#install.packages("nodeHarvest")
#library("nodeHarvest")

options(warn = -1, repr.plot.width = 14, repr.plot.height =  8)

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

# %% [markdown]
# Loading data:

# %% [code]
train <- read_csv('/home/jacoblamkey/Documents/Data/.kaggle/train.csv')
head(train)

cat('there are',nrow(train),'rows in train \n')

# %% [code]
test <- read_csv('/home/jacoblamkey/Documents/Data/.kaggle/test.csv')
head(test)
cat('there are',nrow(test),'rows in test \n')

# %% [code]
sub <- read_csv('/home/jacoblamkey/Documents/Data/.kaggle/sample_submission.csv')
head(sub)

cat('there are',nrow(sub),'rows in sub \n')

# %% [markdown]
# Combining data:

# %% [code]
#Combine train and test
full <- rbind(train, test)

rm(train,test)

full2 = full %>% mutate(Patient = factor(Patient), Sex = factor(Sex), SmokingStatus = factor(SmokingStatus))


summary(full2)

# %% [markdown]
# Some plots:

# %% [code]
ggplot(full2 %>% select(Patient) %>% group_by(Patient) %>% mutate(count = n()), 
       aes(x = factor(count))) + geom_bar() + labs(x = 'Counts per patient', y = 'patients whom have it') + theme_bw()

# %% [code]
ggplot(full2, aes(x = SmokingStatus)) + facet_grid(cols = vars(Sex)) + geom_bar() + theme_bw()

# %% [code]
ggplot(full2, aes(x = Percent)) + geom_density() + theme_bw()

# %% [code]
ggplot(full2, aes(x = Age)) + geom_density() + theme_bw()

# %% [code]
ggplot(full2, aes(x = FVC)) + geom_density() + theme_bw()

# %% [markdown]
# Data preparation:

# %% [code]
#full2 %<>% mutate(AgeGroup = factor(cut(Age, breaks = 2)))

#head(full2)

# %% [code]
#Generate some features

IS =list()

Weeks_Elapsed<-c()
FVC<-c()
Percent<-c()
Initial_Week<-c()


for (i in 1:nrow(full))
{
  entry=as.character(full[i, 1])
  if (is.null(IS[[entry]]))
  {
    IS[[entry]]<-c(full[i, 2],full[i, 3], full[i, 4] )
    Initial_Week[i]<- IS[[entry]][1]
    FVC[i]<- IS[[entry]][2]
    Percent[i]<- IS[[entry]][3]
    Weeks_Elapsed[i]<-full[i, 2] - Initial_Week[i]
  } else {
    Initial_Week[i]<- IS[[entry]][1]
    FVC[i]<- IS[[entry]][2]
    Percent[i]<- IS[[entry]][3]
    Weeks_Elapsed[i]<-full[i, 2] - Initial_Week[i]
  }
}

# %% [code]
#Attach features

full2$FVC_init<-FVC
full2$Percent_init<-Percent
full2$Week_num<-Weeks_Elapsed
tail(full2)

rm(full)
rm(Weeks_Elapsed, FVC,Percent,Initial_Week)

# %% [code]
#Convert the categorical variables into dummy varaibles

#tmp = full2[, c(5, 6, 7)]
tmp = full2[, c(6, 7)]

#fit=OneHotEncoder.fit(tmp)
#out = transform(fit, tmp)

encoder <- onehot(tmp)
cat('encoder created \n')
#head(full)
out <- predict(encoder, tmp)
#head(output)
colnames(out)[1:5]<-c('Fe', 'Male', 'CS', 'EX', 'NS')

cat('output created \n')

#Create a new dataframe with all the information

full2 <- cbind(full2, out) %>% select(-Sex, #-Age,  
                                      -SmokingStatus, -Weeks, -Percent) %>% 
  mutate(FVC_init = as.numeric(FVC_init), 
         Week_num = as.numeric(Week_num), 
         Percent_init = as.numeric(Percent_init))

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

train.new<-full2[1:1549,]
test <- full2[1550:nrow(full2),]

#head(train.new)
#head(test)

set.seed(2)

train.rows <- sample(nrow(train.new), 0.95*nrow(train.new))
train_df <- train.new[train.rows, ]
validate_df <- train.new[-train.rows, ]

head(train_df)


X = train_df %>% select(-Patient, -FVC)

y = train_df$FVC

rm(train.rows,train.new)


invisible(gc())


set.seed(33)#32
models <- caretList(
  x=X,
  y=as.integer(train_df$FVC),
  trControl=trainControl(method="cv",classProbs = T, index = createFolds(as.integer(train_df$FVC), 100),
                         savePredictions=TRUE),
  #  #tuneList=list(nnet.f=caretModelSpec(method="nnet", trace=FALSE, tuneLength=1)),
  #rf.f=caretModelSpec(method="rf", trace=T, tuneLength=5),
  #gbm.f=caretM"naive_bayes,odelSpec(method="gbm", tuneLength=5),
  methodList=c("glmnet",
               'bayesglm',
               'cforest',
               #'lmStepAIC',
               'svmLinear3'
               #"gbm"#
  )) #0.95
#models


#NCAA.stacked<-caretStack(models, method="glm");NCAA.stacked
models.train<-caretEnsemble(models,trControl=trainControl(
  number=100,
  method = "boot",
  verboseIter = TRUE
))


train.pred= predict(models, train_df)
val.pred = predict(models, validate_df)

# %% [code] {"_kg_hide-output":true}
models

range_std = 1:400

#########################################
#train score
metrics.train<-c()


for (i in 1:length(range_std))
{
  metrics.train[i]<- metric(Y_act= train_df$FVC, 
                            Y_pred=train.pred, 
                            range_std[i])
}

plot(y=metrics.train, x=range_std,main="Train")


#########################################
#validate score
metrics.val<-c()


for (i in 1:length(range_std))
{
  metrics.val[i]<- metric(Y_act= validate_df$FVC, 
                          Y_pred=val.pred, 
                          range_std[i])
}

plot(y=metrics.val, x=range_std, main="Validation")

rm(train_df,validate_df,models,metrics.val,metrics.train,val.pred,train.pred)
invisible(gc())

##################################
#load model on full dataset

X = full2 %>% select(-Patient, -FVC)

y = full2$FVC

# Generate submission file for prediction:

# %% [code]
test_names = test$Patient
test_vals = test$FVC
# %% [code]

sub_ <- test[1, ]

#print(sub_)

#test_test = read_csv('../input/osic-pulmonary-fibrosis-progression/test.csv')

#test_test

for (i in seq(-12, 133, by=1))
{
  
  for (j in 1:nrow(test))
  {
    new.row <- test[j,]
    new.row$Week_num <- i - IS[[test[j,1]]]$Week #test_test$Weeks[j]
    sub_ <-rbind(sub_, new.row)
  }
}

sub_ <- sub_[2:nrow(sub_),]
head(sub_)

# %% [code]
rm(new.row)
invisible(gc())

#glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda = 0.009) ## notice the . before the parameter

#glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))

#glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))
options(warn = - 1) 
set.seed(1)
metric.ensemble = list()
for(b in seq(8, 10,1)){
  eval<-foreach(f = 1:10, .combine = rbind, .inorder = FALSE) %dopar% {   
    
    xgbTreeGrid <- expand.grid(nrounds = 100, max_depth = seq(2,6,by = 1), eta = 0.1, gamma = 0, colsample_bytree = 1.0,  subsample = 1.0, min_child_weight =4)
    
    glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda = 0.009) ## notice the . before the parameter
    
    glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))
    
    glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))
    #KerasGrid <- expand.grid()
    #set.seed(f)#32
    models <- caretList(
      x=X,
      y=full2$FVC,
      metric = "RMSE",
      #preProc = c("scale","YeoJohnson"),
      trControl=trainControl(method="cv",classProbs = T, number=i ,
                             index = createResample(full2$FVC, 7),
                             savePredictions="final"),
      
      tuneList=list(  xgbTree = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 5)
                      #xgbTree1 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 8),
                      #xgbTree2 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 10),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 12),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 14),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 16),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 18),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 20),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 22),
                      #xgbTree3 = caretModelSpec(method="xgbTree",  tuneGrid = xgbTreeGrid, nthread = 24)
                      
                      #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic), ## Elastic, highly correlated with lasso and ridge regressions
                      
                      #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
                      
                      #glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge)## Ridge
                      #gbm.f=caretM"naive_bayes,odelSpec(method="gbm", tuneLength=5),
                      
      )
        
      ) #0.99
    models
    
    #NCAA.stacked<-caretStack(models, method="glm");NCAA.stacked
    model.List<-caretEnsemble(models,
     # number=100,
      #method = "cv",
      trControl=trainControl(
        
        number=7, method = "cv"
        
      ),
      metric="RMSE"
      # verboseIter = TRUE
    )
    
    model.List
    
    sub.forPred = sub_[,-c(1,2)]
    preds<-predict(object=model.List, sub.forPred)
    
    metrics<-c()
    
    
    for (i in 1:length(range_std))
    {
      metrics[i]<- metric(Y_act= sub_[,c(2)],                                                 
                          Y_pred=preds, 
                          range_std[i])
    }
    
    plot(y=metrics, x=range_std,main = "Full" )
    met=max(metrics)
    print(met)
    print(i)
    print(b)
    print(f)
    data.frame(f=f,i=i,b=b, Metric = met)
  }
  metric.ensemble[[length(metric.ensemble)+1]] = eval
}


df<-rbindlist(metric.ensemble)
maximum.df<-max(df$F1.score)

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