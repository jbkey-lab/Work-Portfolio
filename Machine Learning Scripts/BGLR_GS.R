######################################################################################################
######Rscript for Half-Sib (HS) Quanitative Trait Loci (QTL)
######################################################################################################
rm(list = ls()) #remove environment vairalbes
invisible(gc(reset = T)) #cleans memory "garbage collector"
memory.limit(size = 8071)
setwd("R:/Breeding/MT_TP/Models/QTL")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################
library("dplyr")
library("data.table")
library("pastecs")
library("asreml")
library("tidyr")
library("tidyverse")
library("xlsx")
library("stats")
#library("qtl")
#library("snow") #run qtl in parallel
#library("doParallel")
require("BGLR")
library("coda")
library("MCMCpack")
library("impute")
#########################Get data
hdp = "C:/Users/jake.lamkey/Documents/" #get the data frames
#hdp = "R:/Breeding/MT_TP/Models/QTL/2020/" #get the data frames
#get the data frames

#setwd("C:/Users/jake.lamkey") #folder where you want files 
outpath = "C:/Users/jake.lamkey/Documents/GS/"
#outpath = "R:/Breeding/MT_TP/Models/QTL/2020/Half-Sib results/"

######################################################################################################

plt.ss = fread(paste0(hdp,"genos.pltht.done.SS.csv"))
plt.ns = fread(paste0(hdp,"genos.pltht.done.NS.csv"))
ear.ss = fread(paste0(hdp,"genos.earht.done.SS.csv"))
ear.ns = fread(paste0(hdp,"genos.earht.done.NS.csv"))
#covarites, more covarites need to be run in the QTL_org.R script

pltssCov = plt.ss[-c(1,2),c(172,175)] %>% mutate_all(as.factor) %>% as.vector()
pltnsCov = plt.ns[-c(1,2),c(172,175)] %>% mutate_all(as.factor) %>% as.vector()
earssCov = ear.ss[-c(1,2),c(172,175)] %>% mutate_all(as.factor) %>% as.vector()
earnsCov = ear.ns[-c(1,2),c(172,175)] %>% mutate_all(as.factor) %>% as.vector()

covars = c(list(pltssCov), list(pltnsCov), list(earssCov), list(earnsCov))

act.df = c(list(plt.ss), list(plt.ns), list(ear.ss), list(ear.ns))

####################################
set.seed(2020)
ncol(plt.ss)
ncol(plt.ns)
ncol(ear.ss)
ncol(ear.ns)

genotype = as.matrix(plt.ss[-c(1:2),c(177:6921)])
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 2

X.knn= impute.knn(genotype, k=10, colmax = 1)
geno.imp.plt.ss = X.knn$data

####
genotype = as.matrix(plt.ns[-c(1:2),c(177:8641)])
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 2

X.knn= impute.knn(genotype, k=10, colmax = 1)
geno.imp.plt.ns = X.knn$data

####
genotype = as.matrix(ear.ss[-c(1:2),c(177:6921)])
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 2

X.knn= impute.knn(genotype, k=10, colmax = 1)
geno.imp.ear.ss = X.knn$data

####
genotype = as.matrix(ear.ns[-c(1:2),c(177:8641)])
genotype[genotype == "AA"] = 0
genotype[genotype == "."] = NA
genotype[genotype == "BB"] = 2

X.knn= impute.knn(genotype, k=10, colmax = 1)
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
(ids.plt.ns <- sample(unique(plt.ns.name$name), length(unique(plt.ns.name$name))*.1 ))
(ids.plt.ss <- sample(unique(plt.ss.name$name), length(unique(plt.ss.name$name))*.1 ))
(ids.ear.ns <- sample(unique(ear.ns.name$name), length(unique(ear.ns.name$name))*.1 ))
(ids.ear.ss <- sample(unique(ear.ss.name$name), length(unique(ear.ss.name$name))*.1 ))

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

###########################################################################
######################################################################
#choose a dataframe
df = 1
for(df in 1:4){
  yNA.df = yNA[df][[1]]
  y.df = y[df][[1]]
  coVar = data.frame(covars[df])
  marker.df = markerDF[df][[1]]
  whichNa = NAwhiches[df][[1]]
  raw.data = act.df[df][[1]]
  
  ######################################################################
  fmR = BGLR(y = yNA.df, response_type= 'gaussian', a = NULL, b = NULL,
             ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
                          list(X = marker.df, model = "BL")),
             nIter = 10000, burnIn = 2500, thin = 10,
             saveAt = outpath)
  summary(fmR)
  
  
  #### *  Calculate model statistics  
  yHat = fmR$yHat[whichNa]
  dat.metrics = function(yHat=yHat){
    dat.yHat = yHat
    df1 <- data.frame(dat.yHat, y.df[whichNa])
    PredAbi = round( cor(df1[, 2], df1[, 1]), 2)                              
    Rank = round( cor(df1[, 2], df1[, 1], method = 'spearman'), 2)         
    MSE =  round( mean((df1[, 2] - df1[,1])^2), 2)                          
    Bias  = round( coef(lm(y.df[whichNa] ~ dat.yHat))[2], 2)        
    Best10 =   round( mean(tail(sort(df1[, 2]), n = ceiling(nrow(df1)*0.1))), 2)   
    # organize in a table 
    table <- cbind(PredAbi, Rank, MSE, Bias, Best10)    
    rownames(table) <- c('Trait')
    print(table)
    return(table)
  }
  effe=dat.metrics(yHat)
  write.csv(effe,paste0(outpath,"B",df,"Metrics.1st.BL.csv"))
  #################################################################
  ## 0.1- phenotype (y=phenotype) and direct genetic values (yHat)
  #################################################################
  
  
  plot( fmR$yHat ~ y.df,   xlab = "Phenotype", ylab = "Predicted", 
        col = densCols(y.df), pch = 20, cex = 1)
  
  # add GEBV 
  points(x = y.df[whichNa], y = fmR$yHat[whichNa], 
         col = 2, bg = 'red', cex = .9, pch = 21)
  # add correlation to the legend 
  txt <- paste("r_vset = ", round(cor(fmR$yHat[whichNa], y.df[whichNa], 
                                      use = 'complete.obs'), 2), sep = "" ) 
  legend("topleft", txt, col = 'red3', bty = 'n')
  
  
  ### 2 - Observed*Predictions
  yHat<-fmR$yHat
  tmp<-range(c(y.df,yHat)) 
  plot(yHat~y.df, xlab='Observed', ylab='Predicted',
       col=2, xlim=tmp,ylim=tmp, pch=19, cex=.7)
  abline(a=0,b=1,col=1,lwd=1) 
  
  
  
  
  
  
  #################################################
  ## 0.2 Estimated Marker Effects & posterior SDs
  ################################################
  
  bHat<- fmR$ETA[[2]]$b
  SD.bHat<- fmR$ETA[[2]]$SD.b
  
  ########## Visualize marker effects aand Their SD
  ### Line plots with two colors
  plot(bHat^1, ylab='Estimated Marker Effect', 
       ylim=c(0,0.1),
       type='o',cex=.5,col=4,main='Marker Effects and their SD')
  
  points(SD.bHat^1, ylab='Estimated SD',
         type='o',cex=.5,col=6,main='Estimated SD of Marker Effects')
  
  
  ### Visualize marker effects only
  # Create data frame 
  meff <- data.frame(bHat)
  names(meff) <- c('Effect')
  
  # scatter plots of marker effects 
  plot(meff$Effect,type="h", col = densCols(meff), pch = 20, cex = 0.6, 
       ylab = 'Marker effect',  main = ' ')
  
  
  
  
  ###########################################################
  ## 0.3 Show marker effect on linkage map (Manhattan Plot) 
  ###########################################################
  #oad('C:/Users/jake.lamkey/Downloads/maritime pine gpData.rda')
  #  Load phased GENETIC MAP
  markpos<- as.data.frame(t(raw.data[c(1,2),-c(1:176)]))
  markpos$eff <- unlist(meff)  # extract betas for markers 
  names(meff) <- c('Effect')  # name the estimates as effect 
  markposNa = na.omit(markpos)
  colnames(markposNa) = c("chr","pos","eff")
  markposNa$chr = as.numeric(as.character(markposNa$chr))
  markposNa$pos = as.numeric(as.character(markposNa$pos))
  
  ### Here the aim is to change the positions of each chromosomes to avoid overlap
  ### if we display them on the same graph
  # First we get the max positions value of each chromosome
  chr.max <- by(markposNa$pos, max, INDICES = markposNa$chr)
  # Then we create an offset value using cumulative sum 
  # sep is the distance between two chromosome on the graph
  sep <- 50
  chr.offset <- data.frame(c(0, head(cumsum(chr.max + sep), -1)), 
                           row.names = names(chr.max))
  # Add this offsets as a new column
  markposNa$offset <- chr.offset[markposNa$chr,]
  # Sum the position an the offest
  markposNa$abspos <- markposNa$offset + markposNa$pos
  
  M.plot <- function(bhat = bhat,add.text=add.text){
    meff <- data.frame(bHat)
    names(meff) <- c('Effect')
    markpos$eff <- unlist(meff)  # extract betas for markers 
    names(meff) <- c('Effect')  # name the estimates as effect 
    markposNa = na.omit(markpos)
    colnames(markposNa) = c("chr","pos","eff")
    markposNa$chr = as.numeric(as.character(markposNa$chr))
    markposNa$pos = as.numeric(as.character(markposNa$pos))
    chr.max <- by(markposNa$pos, max, INDICES = markposNa$chr)
    # Then we create an offset value using cumulative sum 
    # sep is the distance between two chromosome on the graph
    sep <- 50
    chr.offset <- data.frame(c(0, head(cumsum(chr.max + sep), -1)), 
                             row.names = names(chr.max))
    # Add this offsets as a new column
    markposNa$offset <- chr.offset[markposNa$chr,]
    # Sum the position an the offest
    markposNa$abspos <- markposNa$offset + markposNa$pos
    
    
    # Regular plot, x axis is changed to chrmosomes names
    # Estimated Marker Effects & posterior SDs
    #pdf("~/Google Drive/Book/Images/ch12_gs/Fig12-3_MarkerEFfect.pdf", width = 5, height = 3)
    jpeg(file = paste0(outpath,"B",df,add.text,"Mainhatan plot.jpg"), height = 10, width = 12, res = 1000, units = 'cm')
    par(mar=c(4,4,.25,.25))#sets margins of plotting area
    
    plot(markposNa$abspos, abs(markposNa$eff), 
         pch = 20, col = (markposNa$chr)%%2+1,
         xaxt="n", xlab = 'Chromosomes', ylab = 'Marker effects', cex = 1)
    axis(1, at = chr.offset[,1], labels = rownames(chr.offset))
    dev.off()
    return(markposNa)
  }
  
  # Regular plot, x axis is changed to chrmosomes names
  # Estimated Marker Effects & posterior SDs
  #pdf("~/Google Drive/Book/Images/ch12_gs/Fig12-3_MarkerEFfect.pdf", width = 5, height = 3)
  par(mar=c(4,4,.75,.75)+0.1)#sets margins of plotting area
  
  plot(markposNa$abspos, abs(markposNa$eff), 
       pch = 20, col = (markposNa$chr)%%2+1,
       xaxt="n", xlab = 'Chromosomes', ylab = 'Marker effects', cex = 1)
  axis(1, at = chr.offset[,1], labels = rownames(chr.offset))
  
  #dev.off()
  
  
  #############################################
  ## * 4 Comparing models 
  #############################################
  
  nIter = 25000;  burnIn = 5000
  
  ## Bayesian LASSO
  ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
               list(X = marker.df, model = "BL"))
  
  fmBL <- BGLR(y = yNA.df,  ETA = ETA,  nIter=nIter,  burnIn=burnIn,  saveAt = paste0(outpath,"B",df,"BL_"))
  bHat<- fmBL$ETA[[2]]$b
  effe = M.plot(bHat, add.text = "fmBL")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBL.csv"))
  
  ## Bayesian Ridge Regression (Gaussian prior),             
  ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
               list(X = marker.df, model = "BRR"))
  
  fmBRR<-BGLR(y=yNA.df,ETA=ETA, nIter=nIter,burnIn=burnIn, saveAt=paste0(outpath,"B",df,"BRR_") )
  bHat<- fmBRR$ETA[[2]]$b
  effe = M.plot(bHat, add.text="fmBRR")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBRR.csv"))
  
  
  ## Bayes A (Scaled-t prior) 
  ETA[[2]]$model<-"BayesA" 
  fmBA<-BGLR(y=yNA.df,ETA=ETA,nIter=nIter,burnIn=burnIn,saveAt=paste0(outpath,"B",df,"BA_") )
  bHat<- fmBA$ETA[[2]]$b
  effe = M.plot(bHat, add.text = "fmBA")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBA.csv"))
  
  
  ## Bayes B (point of mass at zero + scaled-t slab) 
  ETA[[2]]$model<-"BayesB" 
  fmBB<-BGLR(y=yNA.df,ETA=ETA,nIter=nIter,burnIn=burnIn,saveAt=paste0(outpath,"B",df,"BB_") )
  bHat<- fmBB$ETA[[2]]$b
  effe = M.plot(bHat, add.text="fmBB")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBB.csv"))
  
  
  # Bayes C (point of mass at zero + scaled-t slab) 
  ETA[[2]]$model<-"BayesC" 
  fmBC<-BGLR(y=yNA.df,ETA=ETA,nIter=nIter,burnIn=burnIn,saveAt=paste0(outpath,"B",df,"BC_") )
  bHat<- fmBC$ETA[[2]]$b
  effe = M.plot(bHat, add.text = "fmBC")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBC.csv"))
  
  
  #### *  Calculate correlations  
  r_BL  <- cor(y.df[whichNa], fmBL$yHat[whichNa] )
  r_BRR <- cor(y.df[whichNa],fmBRR$yHat[whichNa] )
  r_BA  <- cor(y.df[whichNa], fmBA$yHat[whichNa] ) 
  r_BB  <- cor(y.df[whichNa], fmBB$yHat[whichNa] ) 
  r_BC  <- cor(y.df[whichNa], fmBC$yHat[whichNa] ) 
  
  # organize in a table 
  table <- data.frame(rbind( r_BL,          r_BRR,         r_BA,         r_BB,  r_BC) , 
                      rbind( fmBL$varE,     fmBRR$varE,    fmBA$varE,    fmBB$varE, fmBC$varE) ,
                      rbind( fmBL$fit$pD,   fmBRR$fit$pD,  fmBA$fit$pD,  fmBB$fit$pD, fmBC$fit$pD)  ,
                      rbind( fmBL$fit$DIC,  fmBRR$fit$DIC, fmBA$fit$DIC, fmBB$fit$DIC, fmBC$fit$DIC)  ) 
  
  colnames(table) <- c( 'PredAbi', "varE", "pD", "DIC")
  rownames(table) <- c( 'BLasso', "BRidge", "BayesA", "BayesB", "BayesC")
  round(table, 3)
  
  write.csv(table,paste0(outpath,"B",df,"Metrics.Baysian.models.csv"))
  
  
  # Estimated effects 
  pdf(paste(outpath,"B",df,"Fig12-5_Predictions.pdf",sep=""), width = 8, height = 8)
  
  par(mar=c(4,4.5,0,0)+0.2)#sets margins of plotting area
  
  p<-ncol(marker.df)
  #   tmp<-range(abs(b0)) 
  plot(numeric()~numeric(),ylim=c(-0.2,0.06),xlim=c(1,p), 
       ylab=expression(paste("|",beta[j],"|")), 
       xlab="Marker Position (order)",yaxt='n') 
  axis(2,at=seq(-0.05,0.05,len=5),lab=c(".05",".025","0",".025",".05")) 
  # abline(v=whichQTL,lty=2,col=4) 
  # points(x=whichQTL,y=abs(b0[whichQTL]),pch=19,col=4) 
  
  points(x=1:p,y=abs(fmBRR$ETA[[2]]$b),col=1,cex=0.6, type="o") 
  points(x=1:p,y=-abs(fmBB$ETA[[2]]$b),col=2,cex=0.6,type="o") 
  text(x=300,y=-0.05,label="Bayes-B", col ="red")
  text(x=300,y=0.05,label="Bayes-RR")
  
  dev.off()
  
  
  
  
  
  ######################################################
  ### *  5  TRACE PLOTS FOR LASSO 
  ######################################################
  
  
  ### plot of residuals convergence
  pdf(paste(outpath,"Fig12-4_TracePlots.pdf",sep=""), width = 7, height = 3)
  
  varE <-scan(paste(outpath,'B',df,'BL_varE.dat',sep=''))
  
  par(mfrow=c(1,2))
  par(mar=c(4,4.5,0,0)+0.2)#sets margins of plotting area
  
  plot(varE, type="o", col = "gray60", pch = 20, cex = 1, ylim = c(12,16),  
       ylab = expression(paste(sigma[epsilon]^2)))
  
  abline(h=fmBL$varE, col=1, lwd=2, lty=1) 
  abline(v=fmBL$burnIn/fmR$thin,col=1, lwd=2, lty=1)
  
  
  
  ### lambda (regularization parameter of BL) 
  lambda <- scan(paste(outpath,'B',df,'BL_ETA_2_lambda.dat',sep='')) 
  
  plot(lambda, type='o', col="gray60",cex=.5, ylim = c(75,140),
       ylab=expression(lambda))
  
  abline(v=fmBL$burnIn/fmR$thin, col=1, lwd=2, lty=1) # vertical line
  abline(h=fmBL$ETA[[2]]$lambda, col=1, lwd=2, lty=1) # horizontal line
  dev.off()
  
  
  ###################################################################
  # 6 PRIOR SPECIFICATION
  ##################################################################
  
  # r, proportion of phenotypic variance attributed to model residuals
  R2=0.5
  
  # Prior hyperparameter values
  # sigmaE2 (residual variance)
  mode.sigE=R2*var(y.df)
  dfe=5
  Se=mode.sigE*(dfe + 2)
  
  # lambda
  mode.sigL=(1-R2)*var(y.df) # variance explained by markers
  rate=2*Se/mode.sigL*sum(colMeans(marker.df)^2)
  shape=1.1
  # Set priors 
  prior=list( varE=list(S0=Se,df0=dfe),
              lambda=list(type='random',
                          shape=shape, 
                          rate=rate) )
  
  ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
               list(X = marker.df, model = "BL",prior))
  fmBL <- BGLR(y = y.df,  ETA = ETA,  nIter=nIter,  burnIn=burnIn, saveAt=paste0(outpath,'B',df,"prior_" ))
  
  str(fmBL)
  
  
  bHat<- fmBL$ETA[[2]]$b
  effe = M.plot(bHat,add.text="prior.fmBL")
  write.csv(effe,paste0(outpath,"B",df,"effects.fmBL.priors.csv"))
  
  yHat = fmBL$yHat[whichNa]
  mets=dat.metrics(yHat)
  write.csv(mets,paste0(outpath,"B",df,"metrics.fmBL.priors.csv"))
  
  ###################################################################
  # 6 PRIOR INFLUENCE
  ##################################################################
  
  # Bayesian LASSO 
  # propE, proportion of phenotypic variance attributed to model residuals
  # Number of samples
  nIter = 25000
  # Burn-in period for the Gibbs sampler
  burnIn = 2500
  # propE, proportion of phenotypic variance attributed to model residuals
  propE=c(0.8,0.6,0.4)
  for (r in propE ){
    # Prior hyperparameter values
    # sigmaE2
    mode.sigE=r*var(y.df)
    dfe=3
    Se=mode.sigE*(dfe + 2)
    
    # lambda
    mode.sigL=(1-r)*var(y.df)
    lambda.hat=sqrt(2*mode.sigE/mode.sigL*sum(colMeans(marker.df)^2))
    delta.lambda=0.05                # rate
    r.lambda=lambda.hat*delta.lambda # shape
    
    # Set priors 
    prior=list( varE=list(S=Se,df=dfe),
                lambda=list(type='random',
                            value=lambda.hat, 
                            shape=r.lambda, 
                            rate=delta.lambda) )
    
    ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
                 list(X = marker.df, model = "BL",prior))
    # Fit Bayesian LASSO Regression
    fmR<-BGLR(y=y.df,ETA=ETA,
              nIter=nIter,
              burnIn=burnIn,
              thin=1,
              saveAt=paste(outpath,"B",df,'LASSO_r_',r,'_',sep=''))
    
    bHat<- fmR$ETA[[2]]$b
    effe=M.plot(bhat,add.text="prior.fmR")
    
    yHat = fmR$yHat[whichNa]
    mets=dat.metrics(yHat)
    write.csv(mets,paste0(outpath,"B",df,"metrics.LASSO_r_',r,'_'.csv"))
    write.csv(effe,paste0(outpath,"B",df,"effects.LASSO_r_',r,'_'.csv"))
    
  }
  
  # Load MCMC draws of varE in matrix varE_L
  k=0
  varE_L=matrix(NA,ncol=length(propE),nrow=nIter)
  for (r in propE){
    k=k+1
    varE_L[,k]=read.table(file=paste(outpath,"B",df,'LASSO_r_',r,'_varE.dat',sep=''),sep=' ')$V1
  }
  
  
  # Load MCMC chain draws of lambda in matrix Lambda
  k=0
  Lambda=matrix(NA,ncol=length(propE),nrow=nIter)
  for (r in propE){
    k=k+1
    Lambda[,k]=read.table(file=paste(outpath,"B",df,'LASSO_r_',r,'_ETA_2_lambda.dat',sep=''),sep=' ')$V1
  }
  
  
  
  # BL posterior means and SEs 
  round( colMeans(varE_L[(burnIn+1):nIter,]) , 2 )
  round( apply(varE_L[(burnIn+1):nIter,],2,sd) , 2 )
  
  colMeans(Lambda[(burnIn+1):nIter,]) 
  apply(Lambda[(burnIn+1):nIter,],2,sd)
  
  
  
  
  # Prior densities for varE
  k=0
  N=2^9
  priorE=matrix(NA,ncol=length(propE),nrow=N)
  for(r in propE){
    k=k+1
    mode.sigE=r*var(y.df)
    dfe=3
    Se=mode.sigE*(dfe + 2)
    xE=seq(from=0.01,to=0.8,length.out=N)
    priorE[,k]=dinvgamma(x=xE,shape=dfe/2, scale=Se/2)
  }
  
  
  # Posterior densities for varE
  for (k in 1:length(propE)){
    namx=paste('dLx_',k,sep='')
    namy=paste('dLy_',k,sep='')
    dL=density(varE_L[(burnIn+1):nIter,k],n=N)
    assign(namx,dL$x)
    assign(namy,dL$y)
  }
  
  
  # Put the output to a file rather than the standard output
  jpeg(file = paste0(outpath,"B",df,"BL_SWEEP.jpg"), height = 8, width = 10, res = 1000, units = 'in')
  
  # Plot
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 6, 2), mgp=c(3,1,0) )
  
  plot(xE,priorE[,1]/max(priorE[,1]),
       xlab=expression(sigma[e]^2),ylab='Scaled density',
       main='Bayesian LASSO Regression',
       type='l',lty=2,lwd=2,cex.lab=1.5,col=2)
  
  for (k in 2:length(propE)){ 
    lines(xE,priorE[,k]/max(priorE[,k]),type='l',lty=2,lwd=2,col=k+1) }
  
  for (k in 1:length(propE)){
    xx=get(paste('dLx_',k,sep=''))
    yy=get(paste('dLy_',k,sep=''))
    lines(xx,yy/max(yy),type='l',lwd=2,col=k+1)
  }
  
  legend('bottomright',lty=c(1,1,1),lwd=2,col=c(2,3,4),
         legend=c('Posterior (Prior: 20% Genetics)',
                  'Posterior (Prior: 40% Genetics)',
                  'Posterior (Prior: 60% Genetics)'),
         bty='n',cex=1.3)
  
  dev.off()
  
  
  
  
  ###################################
  # 8 MODEL DIAGNOSTICS
  ##################################
  
  
  
  # Bayesian LASSO Regression
  # Number of samples
  nIter = 25000
  # Burn-in period for the Gibbs sampler
  burnIn = 0
  # Number of chains
  n.chains=5
  
  # r, proportion of phenotypic variance attributed to model residuals
  r=0.5
  
  # Prior hyperparameter values
  # sigmaE2 (residual variance)
  mode.sigE=r*var(y.df)
  dfe=3
  Se=mode.sigE*(dfe + 2)
  
  # lambda
  mode.sigL=(1-r)*var(y.df) # proportion attributed to genetics
  lambda.hat=sqrt(2*mode.sigE/mode.sigL*sum(colMeans(marker.df)^2))
  delta.lambda=0.05                # rate
  r.lambda=lambda.hat*delta.lambda # shape
  
  # Set priors 
  prior=list( varE=list(S0=Se,df0=dfe,value=runif(1,min=0,max=100)),
              lambda=list(type='random',
                          value=runif(1,min=0,max=100),
                          shape=r.lambda, 
                          rate=delta.lambda) )
  
  
  ETA = list(  list(~factor(PopId)+ factor(Male.Pedigree), data=coVar, model="FIXED"),
               list(X = marker.df, model = "BL",prior))
  
  for (k in 1:n.chains){
    # Fit Bayesian LASSO Regression
    fmR<-BGLR(y = y.df,ETA = ETA,
              nIter = nIter,
              burnIn = burnIn,
              thin =  1,
              saveAt = paste(outpath,"B",df,'Gelman-Rubin_Chain_L_',k,sep = ''))
    
    #bHat <- fmR$ETA[[2]]$b
    #M.plot(bhat)
    
    #yHat = fmR$yHat[whichNa]
    #dat.metrics(yHat)
    
    bHat<- fmR$ETA[[2]]$b
    effe=M.plot(bhat,add.text=paste0("fmR",k))
    
    yHat = fmR$yHat[whichNa]
    mets=dat.metrics(yHat)
    write.csv(mets,paste0(outpath,"B",df,"metrics.Gelman-Rubin_Chain_L_",k,".csv"))
    write.csv(effe,paste0(outpath,"B",df,"effects.Gelman-Rubin_Chain_L_",k,".csv"))
  }
  
  
  
  # Load MCMC draws of varE in matrix varE_L
  varE_L=matrix(NA,ncol=n.chains,nrow=nIter)
  for (k in 1:n.chains){
    varE_L[,k]=read.table( file=paste(outpath, "B",df,'Gelman-Rubin_Chain_L_',k,'varE.dat',sep='') )$V1
  }
  
  # Load MCMC draws of lambda in matrix Lambda
  Lambda=matrix(NA,ncol=n.chains,nrow=nIter)
  for (k in 1:n.chains){
    Lambda[,k]=read.table( file=paste(outpath, "B",df,'Gelman-Rubin_Chain_L_',k,'ETA_2_lambda.dat',sep='') )$V1
  }
  
  
  
  
  # Select parameter
  draws=varE_L
  #draws=Lambda
  
  # Statistics for all chains
  summary(mcmc(draws))
  
  # MCMC object with all chains
  idx=500:20000
  THETA=mcmc.list(mcmc(draws[idx,1]),
                  mcmc(draws[idx,2]),
                  mcmc(draws[idx,3]),
                  mcmc(draws[idx,4]),
                  mcmc(draws[idx,5]))
  
  
  
  # Gelman-Plot
  # Create output file 
  jpeg(file = paste0(outpath,"B",df,"GelmanPlot_BL_HT.jpg"), height = 10, width = 12, res = 1000, units = 'cm')
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 5, 5), mgp=c(3,1,0) )
  gelman.plot(THETA,
              main='Bayesian LASSO',
              xlab='Iteration',
              cex.lab=1.2,
              cex=1.5,
              lwd=2)
  dev.off()
  
  # Gelman-Rubin statistic
  gelman.rubin=gelman.diag(THETA)
  gelman.rubin
  
  # Geweke-Plot of one chain
  geweke.plot(mcmc(as.matrix(draws[,5])))
  
  
}







