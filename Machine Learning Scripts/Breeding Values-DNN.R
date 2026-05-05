rm(list=ls())
invisible(gc())

library(keras)
library(data.table)
library(caret)
library(dplyr)

BV = read.table("/home/jacoblamkey/Downloads/BV.diallel.readyforASReml.csv")

BV.noNA = BV[!is.na(BV$Yield),]


BV.train = as.matrix(BV.noNA[,c("YEAR","FIELD","MALE","FEMALE")])
BV.feature = data.matrix(BV.noNA[,c("Yield")])

#Create a new dataframe with all the information

BV.train = transform(BV.train, 
                     MALE  = factor(MALE),
                     FEMALE = factor(FEMALE),
                     YEAR = factor(YEAR),
                     FIELD = factor(FIELD)
)

#head(BV.train)
#Encoder onehot
encoder <- dummyVars(" ~ .",BV.train)
cat('encoder created \n')
out <- predict(encoder, BV.train)

cat('output created \n')

rm(encoder,BV.train)
out=data.frame(out)
#BV.train = out %>% select(where(~ any(. != 0)))

#head(BV.train)
#summary(BV.train)
BV.train = data.matrix(BV.train)
rm(out)
invisible(gc())

scheduler = function(epoch){
    lr = function(epoch, start_lr, rampup_epochs, exp_decay){
        if(epoch < rampup_epochs){
          return(start_lr)
        }else{
          
          return(start_lr * exp(-exp_decay * epoch))
        }
    
    }
    return( lr(epoch, start_lr, rampup_epochs, exp_decay))
    
}
  #decay_rate = 0.001
  #new_lrate = initial_lrate/(1+decay_rate*epoch)
  #return(new_lrate)



callbacks = list(callback_model_checkpoint(filepath = "/home/jacoblamkey/Documents/save_at_{epoch}.h5"),
                 callback_reduce_lr_on_plateau(monitor='val_mean_squared_logarithmic_error',mode="min", factor=0.1, 
                                               patience=2, min_lr=-5)
               # callback_learning_rate_scheduler(scheduler)
)

model <- keras_model_sequential()

model %>%
 # layer_dense(units = 512, activation = 'relu', input_shape = dim(BV.train)[2]) %>%
 # layer_dropout(rate = 0.1) %>%
  layer_dense(units = 512, activation = 'relu', input_shape = dim(BV.train)[2]) %>%
  #layer_dropout(rate = 0.1) %>%
  layer_dense(units = 254, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 4, activation = 'relu') %>%
 # layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1) #output


model %>% compile(
  loss = "mse",
  optimizer = optimizer_sgd(lr=0.00001),
  metrics = 'mean_squared_logarithmic_error')

model

# Fit the model and store training stats
history <- model %>% fit(
  x=BV.train,
  y=BV.feature,
  epochs = 20,
  batch_size=1024,
  validation_split = 0.1,
  callbacks=callbacks,
  shuffle=F
  #verbose = 0
)

library(ggplot2)

plot(history, metrics = "mean_squared_logarithmic_error", smooth = FALSE) +
  coord_cartesian(ylim = c(20, 25))

model = load_model_hdf5("/home/jacoblamkey/Documents/save_at_20.h5")

BV.predict = predict(object=model, x= BV.train, batch_size=1024) 

BV.predict = cbind(BV.predict, BV.noNA[,c("MALE","FEMALE")])



BV.predict.female = aggregate(BV.predict.female[,c("predict")], by=list(FEMALE = BV.predict.female$FEMALE),mean,na.rm=T); 
colnames(BV.predict.female) = c("FEMALE","g_mean"); BV.predict.female=data.frame(BV.predict.female)

BV.predict.male = aggregate(BV.predict.male[,c("predict")], by=list(MALE = BV.predict.male$MALE),mean,na.rm=T); 
colnames(BV.predict.male) = c("MALE","g_mean"); BV.predict.male=data.frame(BV.predict.male)

BV.predict = rbind(BV.predict.female, BV.predict.male)


write.csv(BV.predict,"BV.predict",row.names=F)

