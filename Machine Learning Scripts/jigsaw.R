
library(tidyverse)
library(data.table)
library(ModelMetrics, quietly=TRUE, warn.conflicts=FALSE)
library(magrittr, quietly=TRUE, warn.conflicts=FALSE)
library(Matrix, quietly=TRUE, warn.conflicts=FALSE)
library(tensorflow, quietly=TRUE, warn.conflicts=FALSE)
library(qdapRegex, quietly=TRUE, warn.conflicts=FALSE)
library(keras, quietly=TRUE, warn.conflicts=FALSE)
library(gdata)

trainVal = function(data, colToInd, sample ){

  ID = data[!duplicated(data[, colToInd]),  colToInd]
  ID = data.frame(ID)

  idx = sample(nrow(ID), nrow(ID) * sample) #.25

  Loc_Validate = data.frame(ID=ID[-idx, ])
  colnames(Loc_Validate) = colToInd
  Loc_Train = data.frame(ID=ID[idx, ])
  colnames(Loc_Train) = colToInd

  trainx2 = dplyr::inner_join(data, Loc_Train, by=colToInd) #%>% rbind(trainx1)
  validatex2 = dplyr::inner_join(data, Loc_Validate, by=colToInd)
  trainx2 = data.frame(trainx2)    #%>% dplyr::mutate_all(as.numeric) #%>% unique()
  validatex2 = data.frame(validatex2)   # %>% dplyr::mutate_all(as.numeric)


  return(list(data.frame(trainx2),data.frame(validatex2)))
}

randomEffect = function(CNN, CN,trainingx2, startVal=1){
  CS = CN[1]
  field.2 = (trainingx2[!duplicated(trainingx2[,CS]), CN])
  field.2$num = c(startVal:(nrow(field.2)+(startVal-1)))
  colnames(field.2)[-ncol(field.2)] = CN
  trainingx2 = left_join(trainingx2, field.2[,c(CS,"num")], by=CS)
  colnames(trainingx2)[ncol(trainingx2)] = CNN
  gc()
  return(list(data.frame(field.2), data.frame(trainingx2)))
}

 worddim=c(60);  epochs=5; samplep=.2; batch = 128;seed = 8042;
embeddingGlove=T; dataloc = "home";word_len=25000
paramTuning = function(worddim, word_len, epochs, samplep, batch , seed, embeddingGlove, dataloc ){
  set.seed(seed)
  if(dataloc == "ubuntu"){
    ex_loc = "/media/jacoblamkey/Storage/jigsaw-toxic-severity-rating/ex_jigsaw.csv"
    test_loc="/media/jacoblamkey/Storage/jigsaw-toxic-severity-rating/validation_data.csv"
    train_loc="/media/jacoblamkey/Storage/jigsaw-toxic-severity-rating/comments_to_score.csv"
    ex_loc2="/media/jacoblamkey/Storage/jigsaw-toxic-severity-rating/train_data_version2.csv"

    if(embeddingGlove){
      embedding.glove_loc="/media/jacoblamkey/Storage/embeddings/glove-840B-300d.txt"
    }else{
      embedding.glove_loc="/media/jacoblamkey/Storage/embeddings/wiki-news-1M-300d.vec"
    }
    # embedding.crawl_loc="D:/OneDrive/Data/fasttext-crawl-300d-2m/crawl-300d-2M.vec"
    # embedding.twitter_loc="D:/OneDrive/Data/glovetwitter27b100dtxt/glove.twitter.27B.200d.txt"
    sample.submission_loc="/media/jacoblamkey/Storage/jigsaw-toxic-severity-rating/sample_submission.csv"

  }
  if(dataloc =="home"){

    ex_loc = "D:/OneDrive/Data/ex_jigsaw.csv"
    test_loc="C:/Users/jake.lamkey/Documents/jigsaw-toxic-severity-rating/validation_data.csv"
    train_loc="C:/Users/jake.lamkey/Documents/jigsaw-toxic-severity-rating/comments_to_score.csv"
    ex_loc2="C:/Users/jake.lamkey/Documents/jigsaw-toxic-severity-rating/train_data_version2.csv"

    if(embeddingGlove){
      embedding.glove_loc="D:/OneDrive/Data/embeddings/glove.840B.300d/glove.840B.300d.txt"
    }else{
      embedding.glove_loc="D:/OneDrive/Data/embeddings/wiki-news-300d-1M/wiki-news-300d-1M.vec"
    }
    # embedding.crawl_loc="D:/OneDrive/Data/fasttext-crawl-300d-2m/crawl-300d-2M.vec"
    # embedding.twitter_loc="D:/OneDrive/Data/glovetwitter27b100dtxt/glove.twitter.27B.200d.txt"
    sample.submission_loc="C:/Users/jake.lamkey/Documents/jigsaw-toxic-severity-rating/sample_submission.csv"
  }
  if(dataloc=="kaggle"){

    ex_loc = "../input/exjigsaw/ex_jigsaw.csv"
    test_loc="../input/jigsaw-toxic-severity-rating/validation_data.csv"
    train_loc="../input/jigsaw-toxic-severity-rating/comments_to_score.csv"
    if(embeddingGlove){
      embedding.glove_loc="../input/embeddings/glove-840B-300d.txt"
    }else{
      embedding.glove_loc="../input/embeddings/wiki-news-1M-300d.vec"
    }
    # embedding.crawl_loc="D:/OneDrive/Data/crawl-300d-2M.vec"
    # embedding.twitter_loc="D:/OneDrive/Data/glove.twitter.27B.200d.txt"
    sample.submission_loc="../input/jigsaw-toxic-severity-rating/sample_submission.csv"

  }

  train_data = suppressWarnings(suppressMessages(fread( train_loc)))
  test_data = suppressWarnings(suppressMessages(fread( test_loc)))
  ex_data = suppressWarnings(suppressMessages(fread( ex_loc)))
  ex_data2 = suppressWarnings(suppressMessages(fread( ex_loc2)))
  ex_data2 = data.frame(comment_id = c(1111111:(nrow(ex_data2)+1111110)), ex_data2)
  colnames(ex_data2)[3] = "target"


  ex_data=data.frame(ex_data[,-1])
  cat("loaded data")
  test_Data.2 = test_data[!duplicated(test_data$less_toxic),c(1,2)]
  test_Data.3 = test_data[!duplicated(test_data$more_toxic),c(1,3)]


  #test_data.less = anti_join(test_Data.2,test_Data.3, by=c("less_toxic"="more_toxic"))
  #test_data.more = anti_join(test_Data.3,test_Data.2, by=c("more_toxic"="less_toxic"))

  # test_data.less$target = 0
  # test_data.more$target = 1
  #
  test_Data.2$target = 0
  test_Data.3$target = 1

  test_Data.1 = full_join(test_Data.2, test_Data.3, by =c("less_toxic"="more_toxic"))
  test_Data.1$target.y = ifelse(is.na(test_Data.1$target.y), test_Data.1$target.x, test_Data.1$target.y)
  test_Data.1$target.x = ifelse(is.na(test_Data.1$target.x), test_Data.1$target.y, test_Data.1$target.x)

  test_Data.1$target = rowMeans(test_Data.1[,c(3,5)])

  train_data = left_join(train_data, test_Data.1[,-c(1,3,4,5)], by=c("text"="less_toxic"))
  #train_data = left_join(train_data, test_Data.1[,-c(1,3,4,5)], by=c("text"="less_toxic"))

  # train_data$target = ifelse(is.na(train_data$target.x), train_data$target.y, train_data$target.x  )
  # train_data$target = ifelse(is.na(train_data$target), 1, train_data$target )
  #
  # train_data = train_data[,-c(3,4)]
  train_data = data.frame(train_data)

  # df1 = data.frame(comments=test_data$less_toxic)
  # df2 = data.frame(comments=test_data$more_toxic)
  # test_data.comments = rbind(df1, df2,deparse.level=0)
  # test_data.comments = test_data.comments[!duplicated(test_data.comments$comments),]
  # test_data.comments=data.frame(test_data.comments)
  #
  # left_overcomments = anti_join(test_data.comments, train_data[,1:2], by=c("test_data.comments"="text"))
  # left_overcomments.less = semi_join(left_overcomments, df1, by=c("test_data.comments"="comments"))
  # left_overcomments.more = semi_join(left_overcomments, df2, by=c("test_data.comments"="comments"))
  #
  # left_overcomments.more2 = anti_join(left_overcomments, left_overcomments.less)
  # left_overcomments.less2 = anti_join(left_overcomments, left_overcomments.more)
  # left_overcomments.more2$target = 1
  # left_overcomments.less2$target = 0
  #
  # left_overcomments = left_join(left_overcomments, left_overcomments.more2)
  # left_overcomments = left_join(left_overcomments, left_overcomments.less2,by=c("test_data.comments"))
  # left_overcomments$target.x = ifelse(is.na(left_overcomments$target.x ), left_overcomments$target.y,left_overcomments$target.x )
  # left_overcomments$target.x = ifelse(is.na(left_overcomments$target.x), 1, left_overcomments$target.x)
  #
  # left_overcomments=left_overcomments[,-c(3)]
  # left_overcomments$comments_id = seq(1:nrow(left_overcomments))
  # colnames(left_overcomments) = c("text","target","comment_id")
  # left_overcomments=left_overcomments[,c(3,1,2)]
  left_overcomments = test_Data.1[,c(1,2,6)]
  colnames(left_overcomments)=c("comment_id","text","target")
  left_overcomments = data.frame(left_overcomments)

  RE = randomEffect(CNN= "TEXT", CN=c("text","comment_id"),ex_data2)
  text = RE[[1]]
  ex_data2 = RE[[2]]

  RE = randomEffect(CNN= "TEXT", CN=c("text","comment_id"),left_overcomments)
  text = RE[[1]]
  left_overcomments = RE[[2]]

  RE = randomEffect(CNN= "TEXT", CN=c("text","comment_id"),train_data)
  text = RE[[1]]
  train_data = RE[[2]]

  RE = randomEffect(CNN= "TEXT", CN=c("text","comment_id"),ex_data)
  text = RE[[1]]
  ex_data = RE[[2]]
  ex_data$comment_id = c(999999999:(nrow(ex_data)+(999999999-1)))
  #ex_data$target = ifelse(ex_data$target<=.5 , 0 , 1)

  datasets = trainVal(data = ex_data, colToInd= "comment_id", sample = samplep)
  trainx3 = datasets[[1]]
  validatex2 = datasets[[2]]

  #datasets = trainVal(data = validatex2, colToInd= "comment_id", sample = .8)
  #validatex2 = datasets[[2]]



  #train_datax2 = rbind(train_data, left_overcomments,trainx3)
  train_datax2 = rbind(trainx3,ex_data2)

  train_datax2 = train_datax2[!duplicated(train_datax2$text),]
  #train_datax2$target = ifelse(train_datax2$target<=.5 , 0 , 1)
  trainx2 = train_datax2
  cat("Train and Test done")

  # datasets = trainVal(data = train_datax2, colToInd= "comment_id", sample = samplep)
  # trainx2 = datasets[[1]]
  # validatex2 = datasets[[2]]
  word_len=word_len

  ##########################################################################

  invisible(gc())

  puncts <- c(
    '\u002C','\u002E','\u0022','\u003A','\u0029','\u0028','\u002D','\u0021','\u003B','\u0027','\u0024','\u0026','\u002F','\u005B','\u005D','\u003E','\u0025','\u003D','\u0023','\u002A','\u002B','\u005C',
    '\u2022','\u007E','\u0040','\u00A3','\u00B7','\u005F','\u007B','\u007D','\u00A9','\u005E','\u00AE','\u0060','\u003C','\u2192','\u00B0','\u20AC','\u2122','\u203A','\u2665','\u2190','\u00D7','\u00A7','\u2033',
    '\u2032','\u00C2','\u2588','\u00BD','\u00E0','\u2026','\u201C','\u2605','\u201D','\u2013','\u25CF','\u00E2','\u25BA','\u2212','\u00A2','\u00B2','\u00AC','\u2591','\u00B6','\u2191','\u00B1','\u00BF','\u25BE',
    '\u2550','\u00A6','\u2551','\u2015','\u00A5','\u2593','\u2014','\u2039','\u2500','\u2592','\uFF1A','\u00BC','\u2295','\u25BC','\u25AA','\u2020','\u25A0','\u2019','\u2580','\u00A8','\u2584','\u266B','\u2606',
    '\u00E9','\u00AF','\u2666','\u00A4','\u25B2','\u00E8','\u00B8','\u00BE','\u00C3','\u22C5','\u2018','\u221E','\u2219','\uFF09','\u2193','\u3001','\u2502','\u00BB','\uFF0C','\uFF08','\u266A','\u2569','\u00B3',
    '\u30FB','\u2566','\u2563','\u2554','\u2557','\u25AC','\u2764','\u00EF','\u00D8','\u00B9','\u2264','\u221A','\u2021','\u5350','\u534D','\u003F', '\u002A','\u007C'
  )
  replace_special_chars <- function(sentence) {
    stringi::stri_replace_all_fixed(sentence, puncts, paste0(" ", puncts, " "), vectorize_all = F)
  }
  preprocess_sentence <- compose(
    replace_special_chars
  )
  embeddings.list=list(embedding.glove_loc
  )
  ensemble_embs <- function(emb_list=list()){
    tmp = Reduce('+', emb_list)
    return(tmp/length(emb_list))
  }
  embedd<-length(embeddings.list)
  #parms<-expand.grid(worddim = worddim, frw1.1=frw1, frw0.1=frw0, embeddings.list=embeddings.list)#embeddings.file.1=embeddings.file

  #worddim<-parms[1, ]$worddim





  ############################################################################
  #train_data$text <- str_replace_all(train_data$text, "\\d", " number ") %>% str_to_lower()
  train_data$text <- preprocess_sentence(train_data$text)

  #trainx2$text <- str_replace_all(trainx2$text, "\\d", " number ") %>% str_to_lower()
  trainx2$text <- preprocess_sentence(trainx2$text)
  # validatex2$text <- str_replace_all(validatex2$text, "\\d", " number ") %>% str_to_lower()
   validatex2$text <- preprocess_sentence(validatex2$text)

  #test_data$less_toxic <- str_replace_all(test_data$less_toxic, "\\d", " number ") %>% str_to_lower()
  test_data$less_toxic <- preprocess_sentence(test_data$less_toxic)
  #test_data$more_toxic<- str_replace_all(test_data$more_toxic, "\\d", " number ") %>% str_to_lower()
  test_data$more_toxic <- preprocess_sentence(test_data$more_toxic)


  combined_comments<-rbind(trainx2$text)
  combined_comments <- combined_comments[!duplicated(combined_comments[,1]), ]
  wordseq = text_tokenizer(num_words = word_len) %>%
    fit_text_tokenizer(as.character(combined_comments))
  # rm(combined_comments)
  x_train = texts_to_sequences(wordseq, trainx2$text ) %>% pad_sequences( maxlen = worddim)
  # x_test = texts_to_sequences(wordseq, validatex2$text ) %>% pad_sequences( maxlen = worddim)
  trainingx2 = texts_to_sequences(wordseq, train_data$text ) %>% pad_sequences( maxlen = worddim)

  word_index = wordseq$word_index; wordindex = unlist(wordseq$word_index)
  dic = data.frame(y=as.character(names(wordindex)), key = wordindex, row.names = NULL) %>% #dic from training model
    arrange(key) %>%
    .[1:word_len,]
  #rm(x_traintext2s,text2s,word_index,wordindex,wordseq,Wordstokeep)
  invisible(gc())
  #rm(w_embed, combined.data,train,mix.embeddings,wgt.glove)
  # test<-data.frame(validatex2$comment_id,validatex2$target, x_test);rm(x_test)

 # combined_comments = data.frame(combined_comments)
  # vec = TfIdfVectorizer$new(min_df=0.3, max_df=0.7)
  # #fitted =vec$fit(combined_comments)
  #
  # combined_comments = c(combined_comments)
  # vec_matrix = vec$fit_transform(combined_comments)
  #

  train<-data.frame(trainx2$comment_id,trainx2$target, x_train);rm(x_train)
  trainingx2<-data.frame(train_data$comment_id, train_data$target, trainingx2);rm(x_train)

  # library(tm)
  # trainx2.toxic = trainx2 %>% filter(target == 1)
  # corp = Corpus(VectorSource(trainx2.toxic$text))
  # corp = tm_map(corp, stripWhitespace)
  # corp = tm_map(corp, stemDocument)
  # dtm.1 = DocumentTermMatrix(corp)
  # frqterms = findFreqTerms(dtm.1, lowfreq = 2, highfreq=4)
  # #frqterms = data.frame(frqterms)
  # rm(x_traintext2s,x_testtext2s,validate1,t_target,validatex2,trainx2,
  #    emb.n,embedding, mix.embeddings,mix.embeds,wgt.glove,wgt.wiki,words.train,words.test)
  # rm(comment_id.target,comment_id.train,target)
  invisible(gc())
  #library(keras)
  #validate<-train[1:150000,]
  #x_train.tar=validate[,2]
  #validate.list=x_train.tar;rm(x_train.tar)
  #invisible(gc())
  x_target<-as.matrix(train$trainx2.target)
  # y_target<-keras::to_categorical(test$validatex2.target)
  cat("Padding done")

  # x_target<-as.matrix(train$trainx2.target)
  # y_target<-as.matrix(test$validatex2.target)


  #saveRDS(dic,"dic.list.rds"); rm(dic) #; saveRDS(embedding,"embedding.rds"); rm(embedding)
  invisible(gc())

  # validate=as.matrix(test[, c(1,3:ncol(test))])
  train=as.matrix(train[, c(1,3:ncol(train))])
  trainingx2 = as.matrix(trainingx2)
  ##########################################################################################
  #dic.list<-readRDS("dic.list.rds")

  wgt.glove = suppressWarnings(suppressMessages(fread(embedding.glove_loc, data.table = FALSE,skip=1)))
  #if(ncol(wgt.glove) == 301){
  names(wgt.glove)<-c("y", paste0("V", 1:300))
  wgt.glove=wgt.glove %>% mutate(y=gsub("[[:punct:]]"," ", rm_white(y) ))
  # }
  emb1 <- suppressWarnings(suppressMessages(data.frame(dic) %>% left_join(data.frame(wgt.glove))));rm(wgt.glove)
  invisible(gc())
  emb1<-emb1[,3:302]
  emb1<-data.frame(emb1)
  emb_mat_ensemble = emb1[1:(nrow(emb1)-1),1:ncol(emb1)] %>%
    mutate_all(as.numeric) %>% mutate_all(round,4) %>% mutate_all(funs(replace(., is.na(.), 0)))
  #saveRDS(w_embed1,"wgt.glove.rds")
  #rm(emb1,w_embed,w_embed1,dic.list)
  invisible(gc())

  #emb_mat_ensemble<-readRDS("wgt.glove.rds")
  embedding=data.frame(emb_mat_ensemble); rm(emb_mat_ensemble)
  names(embedding)<-c(paste0("V", 1:300))
  embedding=list(as.matrix(embedding))
  #print("Done with importing Glove")
  invisible(gc())
  #saveRDS(embedding,"wgt.glove.rds")
  cat("Embedding done")



  ##########################################################################################
  train = train[,-1] %>% data.frame() %>% mutate_all(as.numeric) %>% as.matrix()
  # validate = validate[,-1] %>% data.frame() %>% mutate_all(as.numeric) %>% as.matrix()
  trainingx2 = trainingx2 %>% data.frame() %>% mutate_all(as.numeric) %>% as.matrix()

  #   return(list(worddim, word_len, epochs, batch, samplep, batch,
  #               data.frame(train),data.frame(validate),data.frame(trainingx2),
  #               data.frame(trainx2),data.frame(validatex2),data.frame(train_data)))
  #
  # }
  cat("worddim = ",worddim,  "word_len = ",word_len,  "samplep = ",samplep, "\n")
  # return(list((train),(validate),(trainingx2),
  #             data.frame(trainx2),data.frame(validatex2),data.frame(test_data),
  #             data.frame(train_data),embedding, x_target, y_target))
  return(list((train),(trainingx2),
              data.frame(trainx2),data.frame(test_data),
              data.frame(train_data),embedding, x_target))

}


evaluate  = function(){
  #test = rbind(train,validate)
  y_pred.keras = model %>% predict(trainingx2[,-c(1,2)], batch=batch)
  #trainy_pred.keras = model %>% predict(train, batch=batch)
  # validatey_pred.keras = model %>% predict(validate, batch=batch)

  # submit = rbind(data.frame(sub = trainy_pred.keras))
  # train_dataAdj = rbind(trainx2)
  # submitEval = data.frame(submit, train_dataAdj$text)
  #
  # submitEvalTest = left_join(test_data, submitEval, by=c("less_toxic"="train_dataAdj.text"))
  # submitEvalTest = left_join(submitEvalTest, submitEval, by=c("more_toxic"="train_dataAdj.text"))
  #
  # submitEvalTest$Correct = ifelse(submitEvalTest$sub.x<submitEvalTest$sub.y,T,F)
  # submitEvalTest.accuracy = submitEvalTest %>% group_by(Correct,na.rm=T) %>%
  #   summarise(total = n(),na.rm=T)
  # per = (data.frame(submitEvalTest.accuracy[2,3] )/ data.frame(submitEvalTest.accuracy[1,3] + submitEvalTest.accuracy[2,3] ))
  # cat("Total accuracy =", per[1,1], "\n")
  #
  #
  submit = data.frame(sub=y_pred.keras)
  train_dataAdj = trainingx2
  submitEval = data.frame(submit, train_data$text)

  submitEvalTest = left_join(test_data, submitEval, by=c("less_toxic"="train_data.text"))
  submitEvalTest = left_join(submitEvalTest, submitEval, by=c("more_toxic"="train_data.text"))

  submitEvalTest$Correct = ifelse(submitEvalTest$sub.x<submitEvalTest$sub.y,T,F)
  submitEvalTest.accuracy = submitEvalTest %>% group_by(Correct,na.rm=T) %>%
    summarise(total = n(),na.rm=T)
  per = (data.frame(submitEvalTest.accuracy[2,3] )/ data.frame(submitEvalTest.accuracy[1,3] + submitEvalTest.accuracy[2,3] ))
  cat("Testing accuracy =", per[1,1], "\n")


  submitEvalTestFalse = submitEvalTest %>% filter(Correct == F)

  submitEvalTestFalse$sub = submitEvalTestFalse$sub.y
  submitEvalTestFalse$sub.y = submitEvalTestFalse$sub.x
  submitEvalTestFalse$sub.x = submitEvalTestFalse$sub
  submitEvalTestFalse$Correct = ifelse(submitEvalTestFalse$sub.x<submitEvalTestFalse$sub.y,T,F)
  submitEvalTestFalse1 = submitEvalTestFalse[,c(2,4)]
  colnames(submitEvalTestFalse1)=c("more_toxic","sub.y")
  submitEvalTestFalseBind = rbind(submitEvalTestFalse1,submitEvalTestFalse[,c(3,5)])
  submitEvalTestFalseBind = submitEvalTestFalseBind[!duplicated(submitEvalTestFalseBind$more_toxic),]
  submit = left_join(submitEval, submitEvalTestFalseBind, by=c("train_data.text"="more_toxic"))
  submit$sub.y = ifelse(is.na(submit$sub.y), submit$sub,submit$sub.y)
  submit = submit[,c(3,2)]
  colnames(submit)[2] = "submit"
  submitEval = submit

  submit.indx = order(submitEval$sub.y, decreasing =F)
  submitEval = submitEval[submit.indx,]
  submitEval$score = c(1:nrow(submitEval))

  submitEvalTest = left_join(test_data, submitEval, by=c("less_toxic"="submit"))
  submitEvalTest = left_join(submitEvalTest, submitEval, by=c("more_toxic"="submit"))

  submitEvalTest$Correct = ifelse(submitEvalTest$score.x<submitEvalTest$score.y,T,F)
  submitEvalTest.accuracy = submitEvalTest %>% group_by(Correct,na.rm=T) %>%
    summarise(total = n(),na.rm=T)
  per = (data.frame(submitEvalTest.accuracy[2,3] )/ data.frame(submitEvalTest.accuracy[1,3] + submitEvalTest.accuracy[2,3] ))
  cat("Testing accuracy =", per[1,1], "\n")


  submit = left_join(train_data[,1:2], submitEval[,2:3],by=c("text"="submit"))
  submit = submit[,c(1,3)]
  # keep(list = c("submit","paramTuning","randomEffect","trainVal","i","j","b"),sure = TRUE)
  gc()
  return(data.frame(submit))


}

# datasets = paramTuning(worddim = 60, word_len=90000, epochs = 5,
#                        samplep = 0.9, batch = 128,seed=8041, embeddingGlove=F,
#                        dataloc = "ubuntu")
#
# train = datasets[[1]]
# validate = datasets[[2]]
# trainingx2 = datasets[[3]]
# trainx2 = datasets[[4]]
# validatex2 = datasets[[5]]
# test_data = datasets[[6]]
# train_data = datasets[[7]]
# embedding = datasets[[8]]



#   input_dim=ncol(train)
#   input_layer<-layer_input(shape=c(input_dim))
#   inpt1<-layer_input(shape=list(input_dim))
#
#   matrix=inpt1 %>% keras::layer_embedding(trainable=F, input_dim=nrow(data.frame(embedding)),
#                                           output_dim=ncol(data.frame(embedding)),
#                                           input_length=input_dim,
#                                           weights=embedding, name="embedding")
#
#   model1=matrix %>% layer_spatial_dropout_1d(rate=0.1) %>%
#     bidirectional(layer_lstm(units=128, return_sequences=T)) %>%
#     layer_batch_normalization() %>%
#     layer_dropout(rate=0.1) %>%
#     layer_conv_1d(
#       60,
#       3,
#       padding = "valid",
#       activation = "relu",
#       strides = 1
#     )
#
#   invisible(gc())
#   max_pool1=model1 %>% layer_global_max_pooling_1d()
#   ave_pool1=model1 %>% layer_global_average_pooling_1d()
#   outp = layer_concatenate(list(ave_pool1, max_pool1)) %>%
#     layer_dense(units = 2, activation = "softmax")
#   invisible(gc())
#   model<-keras_model(c(inpt1), outp)
#   model %>% compile(loss='mse', optimizer="adam", metrics="accuracy")
#   get_layer(model, name="embedding") %>% set_weights(embedding) %>% freeze_weights()
#   invisible(gc())
#
#   History = model %>%
#     fit(train, x_target, epochs=epochs, batch_size=batch, #shuffle=T,
#                verbose=1, #validation_split = 0.02,
#                validation_data=list(validate, (y_target) ) )
#
#   # callbacks=list(#callback_early_stopping(monitor="val_loss", verbose=1, patience=2, mode=c("min")),
#   #                #callback_csv_logger(filename=paste0("logger-gru-","Word set",paste0(e),"in embedding",paste0(w),"for fold",paste0(f), ".csv")),
#   #                callback_model_checkpoint(monitor="val_loss",
#   #                                          filepath=paste0("quora_question_model for embedding", ".h5"), save_best_only=TRUE)))
#   print(History)
#   invisible(gc())
#
# submit=evaluate()
