library(data.table)
library(dplyr)
library(onehot)
library(doParallel)
library(caret)
library(caretEnsemble)
library(stats)
# require("BGLR")
# library("coda")
# library("MCMCpack")
library("impute")
library(keras)
library(gbm)
library(RcppCNPy)
library(tidyr)
#####################################################

dp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/Training/"
tdp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/Test Inputs/"
gdp = "/media/jacoblamkey/Storage/cropchallenge/Dataset_Competition_Zip_File/Dataset_Competition/"

#####################################################
o.train <- fread(paste0(dp,"inputs_others_train.csv"))[,-1]
w.train <- fread(paste0(dp,"inputs_weather_train.csv"))[,-1]
y.train <- fread(paste0(dp,"yield_train.csv"))[,-1]

w.test <- fread(paste0(tdp,"inputs_weather_test.csv"))[,-1]
o.test <- fread(paste0(tdp,"inputs_others_test.csv"))[,-1]

geno <- fread(paste0(gdp,"clusterID_genotype.csv"))[-1788,-1]


######################################################
#EDA
######################################################
#means
# EDA = function(other=o.test, weather=w.test){
#   other$State = gsub(other$State, pattern='""',replacement ="")
#   #o.test$State = gsub(o.test$State, pattern='""',replacement ="")
#   
#   o.train.means = aggregate(x=other,by=other[,"ID"],mean)[,-c(3,4)]
#   colnames(o.train.means)[2:4]=c("MG.M","Year.M","Loc.M")
#   
#   o.train.mead = aggregate(x=other,by=other[,"ID"],median)[,-c(1,3,4)]
#   colnames(o.train.mead)[1:3]=c("MG.Median","Year.Median","Loc.Median")
#   
#   o.train.min = aggregate(x=other,by=other[,"ID"],min)[,-c(1,3,4)]
#   colnames(o.train.min)[1:3]=c("MG.min","Year.min","Loc.min")
#   
#   o.train.max = aggregate(x=other,by=other[,"ID"],max)[,-c(1,3,4)]
#   colnames(o.train.max)[1:3]=c("MG.Max","Year.Max","Loc.Max")
#   o.train.F = data.frame(o.train.means, o.train.mead, o.train.min, o.train.max)
#   
#   geno$ID = c(1:nrow(geno))
#   o.train.geno = left_join(o.train.F,geno,by="ID")
#   o.train.mean.geno = left_join(other[,-3],o.train.geno,by="ID")
#   otraingeno = left_join(other,o.train.geno,by="ID")
#   
#   ######################################
#   
#   weather$ID = c(0,rep(1:(nrow(weather)-1)%/%214))+1
#   weather$IDW = rep(1:214,nrow(weather)/214)
#   #weather$ID2 = rep(1:(nrow(weather)/214), 214)
#   
#   #w.test$ID = c(0,rep(1:(nrow(w.test)-1)%/%214))
#   #weather.select = weather %>% filter(IDW >100)
#   #weather$ID2 = rep(1:(nrow(weather)/214), 214)
#   
#   
#   w.train.means = aggregate(x=weather, by=weather[,"ID"],mean)[,-1]
#   colnames(w.train.means)[1:7]=c("ADNI.M","AP.M","ARH.M","MDNI.M","MaxSur.M","MinSur.M","AvgSur.M")
#   
#   w.train.min = aggregate(x=weather[,-c(8,9)], by=weather[,"ID"],min)[,-1]
#   colnames(w.train.min)=c("ADNI.min","AP.min","ARH.min","MDNI.min","MaxSur.min","MinSur.min","AvgSur.min")
#   
#   w.train.max = aggregate(x=weather[,-c(8,9)], by=weather[,"ID"],max)[,-1]
#   colnames(w.train.max)=c("ADNI.max","AP.max","ARH.max","MDNI.max","MaxSur.max","MinSur.max","AvgSur.max")
#   
#   w.train.median = aggregate(x=weather[,-c(8,9)], by=weather[,"ID"],median)[,-1]
#   colnames(w.train.median)=c("ADNI.median","AP.median","ARH.median","MDNI.median","MaxSur.median","MinSur.median","AvgSur.median")
#   
#   w.train.final = data.frame(w.train.means, w.train.max, w.train.min, w.train.median)
#   #w.train.final = w.train.final[,-c(9,10,17,25)]
#   #assign(i, w.train.final)
#   o.train.mean.geno$ID2 = c(1:nrow(o.train.mean.geno))
#   train = left_join(o.train.mean.geno, w.train.final, by=c("ID2"="ID"))
#   ##############################
#   #impute
#   X.knn= impute.knn(as.matrix(train), k=10, colmax = 1)
#   Train.d = X.knn$data
#   train = data.frame(Train.d)
#   
#   
#   round = c("Cluster","Year","Loc","MG")
#   for(i in round){
#     train[,i] = round(train[,i])
#   }
#   for(i in round){
#     train[,i] = as.factor(train[,i])
#   }
#   train= data.frame(train)
#   #################################################################33
#   #cluster
#   ####################################################################
#   round =  c("Cluster","Year","Loc","MG","other.State")
#   train = data.frame(train,other$State)
#   for(i in round){
#     cluster.train.means = aggregate(x=train[,-c(1,1:4,17,48)],by=train[i],mean)
#     colnames(cluster.train.means)[-1] = paste0(colnames(cluster.train.means[-1]),".",i)
#     
#     cluster.train.median = aggregate(x=train[,-c(1:4,17,48)],by=train[i],median)
#     colnames(cluster.train.median)[-1] = paste0(colnames(cluster.train.median[-1]),".",i)
#     
#     cluster.train.min = aggregate(x=train[,-c(1:4,17,48)],by=train[i],min)
#     colnames(cluster.train.min)[-1] = paste0(colnames(cluster.train.min[-1]),".",i)
#     
#     cluster.train.max = aggregate(x=train[,-c(1:4,17,48)],by=train[i],max)
#     colnames(cluster.train.max)[-1] = paste0(colnames(cluster.train.max[-1]),".",i)
#     
#     cluster.train.F = data.frame(cluster.train.means, cluster.train.median,
#                                  cluster.train.means,cluster.train.means)
#     assign(paste0(i), cluster.train.F)
#     
#   }
#   Cluster = left_join(train,Cluster, by ="Cluster")
#   Year = left_join(Cluster,Year, by ="Year")
#   MG = left_join(Year,MG, by ="MG")
#   Loc = left_join(MG,Loc, by ="Loc")
#   Train = left_join(Loc,other.State, by ="other.State")
#   
#   dim(Train)
#   
#   #####################################################
#   #weather wide format
#   cols = colnames(weather[,-c(8,9)])
#   
#   for(i in cols){
#     weather.wide = weather %>%  
#       pivot_wider(names_from = IDW,values_from=i, id_cols=ID)
#     assign(i, weather.wide)
#   }
#   weatherA = data.frame(ADNI,AP,ARH,AvgSur,MaxSur,MDNI,MinSur)
#   
#   Train = data.frame(Train,weatherA,by="ID")
#   
#   #############################
#   #Cat dummy vars
#   #############################
#   rm(other.State)
#   tmp = otraingeno[, c(1,3,4,5,18,2)]
#   tmp = tmp %>% mutate_all(as.factor)
#   
#   encoder <- onehot(tmp,max_levels=15000)
#   cat('encoder created \n')
#   
#   #head(full)
#   out <- predict(encoder, tmp)
#   out=data.frame(out)
#   #out=out %>% select(-Year, -MG, -State, -Loc) 
#   o.train.dummy = data.frame(Train, out)
#   # o.train.dummy=o.train.dummy %>% dplyr::select(-Year, -MG, -Loc, -other.State) 
#   # o.train.dummy = o.train.dummy %>% mutate_all(as.factor)
#   # o.train.mean.geno = o.train.mean.geno %>% mutate_all(as.numeric)
#   o.train.dummy$other.State = as.factor(o.train.dummy$other.State)
#   o.train.dummy$other.State = as.numeric(o.train.dummy$other.State)
#   
#   o.train.dummy = o.train.dummy %>% mutate_all(as.numeric)
#   
#   #o.train.Final = o.train.Final[,1:211] %>% mutate_all(as.factor)
#   train = data.frame(o.train.dummy[,1:3400])
#   cat(dim(train))
#   rm(out, encoder, tmp, Cluster, cluster.train.F, cluster.train.max, cluster.train.means,
#      cluster.train.median,cluster.train.min, Field,Loc,MG,o.train.F,o.train.geno,
#      o.train.max,o.train.mead,o.train.min,otraingeno,Year,X.knn,Train.d,Train,other,weather)
#   gc()
#   
#   rm(tmp, out, o.train.means, o.train.mean.geno, o.train.Final, 
#      w.train.final, w.train.means, w.train.min, w.train.max, w.train.median,
#      o.train.dummy,weatherA, ARH, MDNI, AvgSur, MaxSur,MinSur, AP,ADNI,weather, weather.wide)
#   gc()
#   
#   return(data.frame(train))
# }
# 
# evaluate = function(object,test,validate,train){
#   pred = predict(object, data.frame(train))
#   
#   cat("preTrain: This is iter "," and r2",cor(pred,Yield.t)^2,
#       " and RMSE is ",sqrt(mean((Yield.t-pred)^2)),"\n")
#   pred = predict.gbm(object, validate, type="response")
#   cat("preTrain: This is iter "," and r2",cor(pred,Yield.v)^2,
#       " and RMSE is ",sqrt(mean((Yield.v-pred)^2)),"\n")
#   
#   ############3
#   preds = predict.gbm(gbmfit, data.frame(test.done))
#   # pred$yieldpred = ifelse(preds<20, 0.2,ifelse(preds>80,1.5,1))
#   # y=data.frame(pred$yieldpred,preds)
#   # y$newpreds = y$pred.yieldpred * y$preds
#   # cor(y)
#   preds.yield = data.frame(preds ,o.test)
#   preds.yield.index= same$rownum
#   preds.yield[preds.yield.index,"preds"] =same[6]
#   predictions = round(preds.yield$preds,2)
#   npySave(object = predictions, filename="submission.pretrain.npy")
# }
# Train = EDA(other=o.train, weather=w.train)
# Test = EDA(other = o.test, weather=w.test)
# y.train$Yield=as.numeric(y.train$Yield)
# 

# 
# Train = Train[, which(colMeans(Train == 0) < 1)]
# 
# Train.t = t(Train)
# Train.t <- setDT(data.frame(Train.t),keep.rownames=T)[]
# Test.t = t(Test)
# Test.t <- setDT(data.frame(Test.t),keep.rownames=T)[]
# 
# Test.cs = inner_join(Test.t, Train.t, by ="rn")
# 
# Test.done = t(Test.cs[,1:10338])
# Test.done = data.frame(Test.done)
# colnames(Test.done)=Test.done[1,]
# 
# Train.done = t(Test.cs[,-c(2:10338)]) 
# Train.done = data.frame(Train.done)
# colnames(Train.done)=Train.done[1,]
# Train.done = Train.done[-1,]
# Test.done = Test.done[-1,]
# 
# dim(Test.done)
# Train.done = cbind(y.train,Train.done)
# dim(Train.done)
# 
# rm(geno,o.test,o.train,Test,Test.cs,Test.t,Train,Train.t,w.test,w.train)
# gc()

# 
# Train.done = fread("/media/jacoblamkey/Storage/Train.csv")
# Test.done = fread("/media/jacoblamkey/Storage/Test.csv")

yo.train = data.frame(o.train,y.train)
on.test = data.frame(o.test,rownum=c(1:nrow(o.test)))
same = inner_join(yo.train,on.test,by=c("ID","Loc","Year","State","MG"))




set.seed(2)
data1 = sample(1:nrow(Train.done),nrow(Train.done)*1)
Train.done.sample<-Train.done[-c(1:9302),]
Validate.done.sample<-Train.done[1:9302,]

Test.Final = Test.done
Validate.Final = data.frame(Validate.done.sample[,-"Yield"])
Train.Final = data.frame(Train.done.sample[,-"Yield"])


# Validate.Final = prcomp(Validate.Final,rank=10)
# Validate.Final = as.matrix(Validate.Final$x)
# Train.Final = prcomp(Train.Final,rank=10)
# Train.Final = as.matrix(Train.Final$x)
# 
# Test.Final = prcomp(Test.Final,rank=10)
# Test.Final = as.matrix(Test.Final$x)

# ###################################################
#w.test$ID = c(0,rep(1:(nrow(w.test)-1)%/%214))+1
w.test$IDD = rep(1:214,nrow(w.test)/214)
# w.test = w.test %>% filter(IDD >4)
# w.test$IDW = c(0,rep(1:(nrow(w.test)-1)%/%7))+1
# 
# cols = colnames(w.test)
# w.test$ID = as.factor(w.test$ID)
# weatherT = aggregate(x = w.test, by = w.test[,c("IDW")], FUN=mean)
# weatherT = weatherT[,-c(11,9)]
# weatherT$IM = rep(1:30,nrow(weatherT)/30)
# weatherT = weatherT[,-c(1)]
# 
#weatherT= data.frame(weatherT)
#weatherT= data.frame(weatherT,Test.done$Cluster)
library(abind)
# WM.array.t = weatherT%>%mutate_all(as.numeric)
# WM.array.t$IM = rep(1:30,nrow(WM.array.t)/30)
IM=data.frame(w.test[,"IDD"])
IM=as.matrix(IM)
WM.array.t = abind(split(w.test,IM), along=3)

#######################
#w.train$ID = c(0,rep(1:(nrow(w.train)-1)%/%214))+1
w.train$IDD = rep(1:214,nrow(w.train)/214)
WM.array = w.train%>%mutate_all(as.numeric)
#WM.array$IM = rep(1:30,nrow(WM.array)/30)

#WM.array<-( weatherATrain.done.sample)
#dim( WM.array) <- c(83725, 1505, 1)
IM=data.frame(WM.array[,"IDD"])
IM=as.matrix(IM)


WM.array = abind(split(WM.array,IM), along=3)


# w.train = w.train %>% filter(IDD >4)
# w.train$IDW = c(0,rep(1:(nrow(w.train)-1)%/%7))+1
# 
# cols = colnames(w.train)
# w.train$ID = as.factor(w.train$ID)
# weatherA = aggregate(x = w.train, by = w.train[,c("IDW")], FUN=mean)
# weatherA = weatherA[,-c(11,9)]
# weatherA$IM = rep(1:30,nrow(weatherA)/30)
# weatherA = weatherA[,-c(1)]

# weatherA= data.frame(weatherA)

#
# 
# train = data.frame(Train.done, weatherA) %>% mutate_all(as.numeric)
# test = data.frame(Test.done, weatherT) %>% mutate_all(as.numeric)
# weatherA = w.train[c(1:nrow(Train.done)),]
# weatherA = data.frame(weatherA, Train.done$Cluster)
# weatherA = prcomp(weatherA,rank=10)
# weatherA = as.matrix(weatherA$x)
# weather = left_join(Train.done[,c("ID2","Yield")], w.train, by=c("ID2"="ID"))
# set.seed(5)
# 
# 
# 
# weatherATrain.done.sample<-weatherA[-c(1:279060),]
# weatherAValidate.done.sample<-weatherA[1:279060,]
# 
#ample = as.matrix(weatherATrain.done.sample)


#weatherATrain.done.s


Yield = Train.done.sample$Yield
Yield = as.matrix(Yield)
Yield.t = (Yield)
#Yield.t = array(length(Yield.t), dim= c(nrow(Yield.t),30,1))

#Train.Final$ID3 =c(1:nrow(Train.Final))
Train.Final = data.frame(Train.done.sample[,-"Yield"])

Train.Final = (Train.Final[,1:2500])   %>% as.matrix()
TF = (Train.Final)



#weatherAValidate.done.sample = as.matrix(weatherAValidate.done.sample)
WM.array.v = weatherAValidate.done.sample%>%mutate_all(as.numeric)

#WM.array.v<-as.matrix( weatherAValidate.done.sample)
#dim( WM.array) <- c(83725, 1505, 1)
WM.array.v$IM = rep(1:30,nrow(WM.array.v)/30)

IM=data.frame(WM.array.v[,"IM"])
IM=as.matrix(IM)

#dim( WM.array) <- c(83725, 1505, 1)
WM.array.v = abind(split(WM.array.v, IM), along=3)



Yield = Validate.done.sample$Yield
Yield = as.matrix(Yield)
Yield.v = (Yield)
#Yield.t = array(length(Yield.t), dim= c(nrow(Yield.t),30,1))
#Validate.Final$ID3 = c(1:nrow(Validate.Final))
Validate.Final = data.frame(Validate.done.sample[,-"Yield"])

Validate.Final = (Validate.Final[,1:2500]) %>% as.matrix()
VF = (Validate.Final)
train = data.frame(WM.array,TF[,1:2500]) %>% mutate_all(as.numeric)
train = train[, which(colMeans(train == 0) < 1)]
validate = data.frame(WM.array.v,VF[,1:2500]) %>% mutate_all(as.numeric)
test = data.frame(WM.array.t,Test.Final[,1:2500]) %>% mutate_all(as.numeric)




gbmfit = gbm(Yield.t~. ,  cv.folds = 4, 
             # w =a1,
             n.cores= 15, 
             
             #train.fraction = .9,
             distribution ="gaussian",
             verbose=T,
             data=train, 
             keep.data = F,n.trees = 650)

evaluate(gbmfit)

v=summary.gbm(gbmfit)
# head(v)
 vselect = v[1:308,1]
 train.done = train %>% select(all_of(vselect),"ID","other.State",
                                    "MG","Loc","Year","Cluster")
 validate.done = validate %>% select(all_of(vselect),"ID","other.State",
                                    "MG","Loc","Year","Cluster")
 test.done = test %>% select(all_of(vselect),"ID","other.State",
                                   "MG","Loc","Year","Cluster")

write.csv(train.done,"train.done.csv",row.names=F)
write.csv(validate.done,"validate.done.csv",row.names=F)
write.csv(test.done,"test.done.csv",row.names=F)

train.done = fread("train.done.csv")
validate.done = fread("validate.done.csv")
test.done = fread("test.done.csv")

train.done = train.done %>% mutate_all(as.numeric) %>% as.matrix()
validate.done = validate.done %>% mutate_all(as.numeric) %>% as.matrix()

#############################################################
#a1 = ifelse(Yield.t)









# 
# set.seed(5)
#  gbmfit = gbm(Yield.t~. ,  cv.folds = 4, 
#               # w =a1,
#               n.cores= 15, 
#               
#               #train.fraction = .9,
#               distribution ="gaussian",
#               verbose=T,
#               data=train.done, 
#               keep.data = F,n.trees = 650)
#  
#  pred=predict(gbmfit, data.frame(train.done))
#  pred=data.frame(pred)
 # cor(pred$pred, Yield.t)^2
 # pred$yieldpred = ifelse(pred$pred<20, 0.2,ifelse(pred$pred>80,1.5,1))
 # y=data.frame(pred$yieldpred, pred$pred, Yield.t)
 # y$newpred = y$pred.yieldpred * y$pred.pred
 # y$diff= y$pred.pred - y$Yield.t
 # cor(y)
 
 # for(i in seq(0.1,2,0.1)){
 #   for(j in seq(0.1,2,0.1)){
 #     for(e in c(40,50,60)){
 #       for(u in c(70,80,90)){
 #     pred$yieldpred = ifelse( pred$pred > u &  pred$pred <e, i,ifelse(pred$pred>u,j,1))
 #     y=data.frame(pred$yieldpred, pred$pred, Yield.t)
 #     y$newpred = y$pred.yieldpred * y$pred.pred
 #     y$diff= y$newpred - y$Yield.t
 #     if(cor(y$newpred, y$Yield.t)^2>.63){
 #     cat(cor(y$newpred, y$Yield.t)^2, i, j, u ,e ,"\n")
 #   }
 #     }
 # }
 #   }}
 # 
 
 evaluate(gbmfit,train=train.done,validate=validate.done,test=test.done)

