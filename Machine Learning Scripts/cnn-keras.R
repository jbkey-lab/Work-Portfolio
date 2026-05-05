



# Temporal Attention
temporal_one_step_attention = function(a, u){
  t_densor = layer_dense(units=u, activation = "relu")
  
  activator = layer_activation_softmax(axis=1)
  # a: Sequence of encoder hidden states (n_sample, 10, 16)
  e_temporal = t_densor(a)  # (n_samples, 10, 1)
  alphas = activator(e_temporal)    # (n_samples, 10, 1)
  t_context = layer_dot(inputs=c(alphas, a),axes=1)    # (n_samples, 1, 16)
  
  return( t_context)
}

in_dim = c(dim(WM.array)[2:3])
in_dim2 = c(dim(train.done)[2])

#model = keras_model_sequential() %>%
encoder = layer_input(shape=c(in_dim))
decoder = layer_input(shape=c(in_dim2))
alphas_list = list()


#lstm1=layer_lstm(units = 128,return_sequences = T, return_state=T)(encoder) 
#lstm1=layer_lstm(units = 128,return_sequences = TRUE) (encoder)
#lstm1=  layer_lstm(units = 128,return_sequences = TRUE) (lstm1)
lstm1=  bidirectional(layer=layer_lstm(activation = "linear",units = 128,return_sequences = T, return_state=T)) (encoder)
  
lstm1 = layer_dropout(rate=.2)(lstm1[[1]])
lstm1=layer_dense(units=1,activation="linear")(lstm1)

t_context1 = temporal_one_step_attention (lstm1, 1)  # (None, 1, 32)
lstm1=layer_dense(units=2,activation="linear")(lstm1)
# 
# t_context2 = temporal_one_step_attention (lstm1, 2)  # (None, 1, 32)
# lstm1=layer_dense(units=3,activation="linear")(lstm1)
# 
# t_context3 = temporal_one_step_attention (lstm1, 3)  # (None, 1, 32)
# lstm1=layer_dense(units=4,activation="linear")(lstm1)
# 
# t_context4 = temporal_one_step_attention (lstm1, 4)  # (None, 1, 32)
# lstm1=layer_dense(units=5,activation="linear")(lstm1)
# 
# t_context5 = temporal_one_step_attention (lstm1, 5)  # (None, 1, 32)
# lstm1=layer_dense(units=6,activation="linear")(lstm1)
# 
# t_context6 = temporal_one_step_attention (lstm1, 6)  # (None, 1, 32)
# lstm1=layer_dense(units=7,activation="linear")(lstm1)
# 
# t_context7 = temporal_one_step_attention (lstm1, 7)  # (None, 1, 32)
# lstm1=layer_dense(units=8,activation="linear")(lstm1)
# 
# t_context8= temporal_one_step_attention (lstm1, 8)  # (None, 1, 32)
# lstm1=layer_dense(units=9,activation="linear")(lstm1)
# 
# t_context9 = temporal_one_step_attention (lstm1, 9)  # (None, 1, 32)
# lstm1=layer_dense(units=10,activation="linear")(lstm1)
# 
# t_context10 = temporal_one_step_attention (lstm1, 10)  # (None, 1, 32)

#lstm1 = layer_gaussian_noise(stddev = 0.5)(lstm1)
#layer_dropout(rate=.3) %>%
#lstm2=layer_lstm(units = 128,return_sequences = TRUE) (lstm1)
#lstm2=  layer_lstm(units = 128,return_sequences = TRUE) (lstm2)
lstm2=  bidirectional(layer=layer_lstm(activation="linear",units = 128,return_sequences = F, return_state=T)) (lstm1)

lstm2 = layer_dropout(rate=.2)(lstm2[[1]])
#lstm2 = layer_layer_normalization()(lstm2)

t_context1 = layer_flatten(t_context1)  # (None, 32)
# t_context2 = layer_flatten(t_context2)  # (None, 32)
# t_context3 = layer_flatten(t_context3)  # (None, 32)
# t_context4 = layer_flatten(t_context4)  # (None, 32)
# t_context5 = layer_flatten(t_context5)  # (None, 32)
# t_context6 = layer_flatten(t_context6)  # (None, 32)
# t_context7 = layer_flatten(t_context7)  # (None, 32)
# t_context8 = layer_flatten(t_context8)  # (None, 32)
# t_context9 = layer_flatten(t_context9)  # (None, 32)
# t_context10 = layer_flatten(t_context10)  # (None, 32)

#t_context=layer_dense(units=2,activation="linear")(lstm2)

#  layer_dense(units=32,activation="linear") %>%
context = layer_concatenate(c(t_context1,
                              # t_context2,
                              # t_context3,
                              # t_context4,
                              # t_context5,
                              # t_context6,
                              # t_context7,
                              # t_context8,
                              # t_context9,
                              # t_context10,
                              
                              decoder ), axis=-1)
# layer_dropout(rate=.3) %>%

pred= layer_dense(units = 1, activation = "linear")(context)
model = keras_model(c(encoder,decoder), pred)
summary(model)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam( learning_rate= 0.01) , 
  metrics = metric_root_mean_squared_error()
)

Epochs = 25
#for(i in 1:Epochs ){

#for(i in 1:100){
 # print(i)4
gc()
model %>% fit(x=list(WM.array, as.matrix(train.done)), 
              y= list(c(Yield.t)), 
              epochs=100, batch_size=512, 
            #sample_weight=a1,
              verbose=2, shuffle=T,
              callbacks = list(callback_model_checkpoint(
                filepath = "{epoch:02d}-{root_mean_squared_error:.2f}.hdf5",
                monitor = "val_loss",
                 save_best_only = T )),
               validation_data =list(list(WM.array.v,as.matrix(validate.done)),
                                     list(c((Yield.v) ))))
 #model %>% reset_states()
#}evalu

evaluate(model,train=train.done,validate=validate.done,test=test.done)

############################################
train = data.frame(WM.array,TF) %>% mutate_all(as.numeric)
train = train[, which(colMeans(train == 0) < 1)]
validate = data.frame(weatherAValidate.done.sample,VF) %>% mutate_all(as.numeric)
test = data.frame(weatherT,Test.Final) %>% mutate_all(as.numeric)

gbmfit = gbm(y.train$Yield~. ,  cv.folds = 4, 
             # w =a1,
             n.cores= 15, 
             
             #train.fraction = .9,
             distribution ="gaussian",
             verbose=T,
             data=Train.done[,1:53], 
             keep.data = F,n.trees = 650)


#a1 = ifelse(Yield.t<10, 1,ifelse(Yield.t>90,2,0))


#fit <- glmnet(train, Yield.t, alpha = 0.2, weights = a1, nlambda = 20)

# 
# library(doMC)
# registerDoMC(cores = 15)
# 
# cvfit <- cv.glmnet(train, Yield.t,  type.measure = "mse", nfolds = 10,parallel=T)

pred = predict(gbmfit, data.frame(Train.done))
cat("preTrain: This is iter "," and r2",cor(pred,y.train$Yield)^2,
    " and RMSE is ",sqrt(mean((y.train$Yield-pred)^2)),"\n")
# pred = predict.gbm(gbmfit, validate, type="response")
# cat("preTrain: This is iter "," and r2",cor(pred,Validate.done.sample$Yield)^2,
#     " and RMSE is ",sqrt(mean((Validate.done.sample$Yield-pred)^2)),"\n")
preds = predict.gbm(gbmfit, data.frame(Test.done))
preds.yield = data.frame(preds,o.test)


preds.yield.index= same$rownum

preds.yield[preds.yield.index,"preds"] =same[6]
predictions = round(preds.yield$preds,2)
npySave(object = predictions, filename="submission.pretrain.npy")


