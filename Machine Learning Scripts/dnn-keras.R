callbacks = list()
model = keras_model_sequential()
#model %>% layer_input(shape = c(  dim(Weather))  ) %>%
#  decoder= layer_input(shape = c(  dim(other))   )
model%>%
  layer_dense(units=32, activation="linear",input_shape = dim(Train.done[,1:53]))%>%
 # layer_dropout(rate=.4)%>%
  layer_dense(units=32, activation="linear")%>%
  #layer_dropout(rate=.2)%>%
  layer_dense(units=1, activation="linear")
  


summary(model)
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.001 ),  
  metrics = metric_root_mean_squared_error()
)

Epochs = 2 
#for(i in 1:Epochs ){
model %>% fit(list(as.matrix(Train.done[,1:53])), as.matrix(y.train$Yield), 
              epochs=25, batch_size=300, 
              verbose=2, shuffle=T)
#              validation_data = 
#                 list(as.matrix(validate),as.matrix(Yield.v)) )
# #model %>% reset_states()
#}
pred = predict(model, as.matrix(Train.done[,1:53]))
cat("preTrain: This is iter "," and r2",cor(pred,y.train$Yield)^2,
    " and RMSE is ",sqrt(mean((y.train$Yield-pred)^2)),"\n")

preds = predict(model, as.matrix(Test.done))
preds.yield = data.frame(preds,o.test)
preds.yield.index= same$rownum
preds.yield[preds.yield.index,"preds"] =same[6]
predictions = round(preds.yield$preds,2)
npySave(object = predictions, filename="submission.pretrain.npy")
















model = keras_model_sequential()
#model %>% layer_input(shape = c(  dim(Weather))  ) %>%
#  decoder= layer_input(shape = c(  dim(other))   )
model%>%
layer_lstm(units = 32,return_sequences = TRUE) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  bidirectional(layer_lstm(units = 32)) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.01 ),  
  metrics = metric_root_mean_squared_error()
)

Epochs = 2 
#for(i in 1:Epochs ){
model %>% fit(list(as.matrix(train)), as.matrix(Yield.t), 
              epochs=500, batch_size=512, 
              verbose=2, shuffle=T,
              validation_data = 
                list(as.matrix(validate),as.matrix(Yield.v)) )
#model %>% reset_states()
#}