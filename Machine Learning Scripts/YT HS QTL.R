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
library("qtl")
library("snow") #run qtl in parallel
library("doParallel")

#########################Get data
hdp = "C:/Users/jake.lamkey/Documents/" #get the data frames
setwd("C:/Users/jake.lamkey") #folder where you want files 

######################################################################################################

plt.ss = fread(paste0(hdp,"genos.pltht.done.SS.csv"))
plt.ns = fread(paste0(hdp,"genos.pltht.done.NS.csv"))
ear.ss = fread(paste0(hdp,"genos.earht.done.SS.csv"))
ear.ns = fread(paste0(hdp,"genos.earht.done.NS.csv"))

#covarites, more covarites need to be run in the QTL_org.R script
pltssCov = plt.ss[-c(1,2),c(1:170)]
pltnsCov = plt.ns[-c(1,2),c(1:170)]
earssCov = ear.ss[-c(1,2),c(1:170)]
earnsCov = ear.ns[-c(1,2),c(1:170)]

#remove columns with all 0 values
pltssCov = pltssCov %>% select(where(~ any(. != 0)))
pltnsCov = pltnsCov %>% select(where(~ any(. != 0)))
earssCov = earssCov %>% select(where(~ any(. != 0)))
earnsCov = earnsCov %>% select(where(~ any(. != 0)))


############################################################
# Data import
############################################################
PhSs<-read.cross("csv", 
                 file=paste0(hdp,"genos.pltht.done.SS.csv"), 
                 genotypes = c("AA","BB"), na.strings=".",
                 alleles = c("B","A"), crosstype="dh")
PhNs<-read.cross("csv", 
                 file=paste0(hdp,"genos.pltht.done.NS.csv"), 
                 genotypes = c("AA","BB"), na.strings=".",
                 alleles = c("B","A"), crosstype="dh")
EhSs<-read.cross("csv", 
                 file=paste0(hdp,"genos.earht.done.SS.csv"), 
                 genotypes = c("AA","BB"), na.strings=".",
                 alleles = c("B","A"), crosstype="dh")
EhNs<-read.cross("csv", 
                 file=paste0(hdp,"genos.earht.done.NS.csv"), 
                 genotypes = c("AA","BB"), na.strings=".",
                 alleles = c("B","A"), crosstype="dh")

############################################################
#jittermap(PhSs)
#jittermap(PhNs)
#jittermap(EhSs)
#jittermap(EhNs)

PhSs <- calc.genoprob(PhSs, step=0, error.prob= 0.001, map.function = "kosambi")
PhNs <- calc.genoprob(PhNs, step=0, error.prob= 0.001, map.function = "kosambi")
EhSs <- calc.genoprob(EhSs, step=0, error.prob= 0.001, map.function = "kosambi")
EhNs <- calc.genoprob(EhNs, step=0, error.prob= 0.001, map.function = "kosambi")

#nind(sug) # number of individuals
#nchr(sug) # number of chromosomes
#totmar(sug) # total number of markers
#nmar(sug) # number of markers per chromosome
#nphe(sug) # number of phenotypes in the data
#cg<- comparegeno(PhSs)
#hist(cg, breaks=200,xlab="Proportion of Identical Genotypes")
#rug(cg)
#which(cg ==1, arr.ind = T)
#erf = est.rf(PhSs)

###############################################
#permutations and other things before computation of two QTL scan
###############################################
#out2 <- scantwo(sug, verbose=FALSE,clean.output=TRUE)
#strat <- (nmissing(sug) > 50)

############################################################
##  For the impatient Single-QTL analysis using a marker based approach
###########################################################
covars = c(list(pltssCov), list(pltnsCov), list(earssCov), list(earnsCov))
dflist = c(list(PhSs), list(PhNs), list(EhSs), list(EhNs))
markerDF = c(list(plt.ss), list(plt.ns), list(ear.ss), list(ear.ns))

perms = 500
t = c(173)#quantiative trait
df = 2
clus = 1

cores=detectCores()
cl <- makeCluster(cores[1]-6)
registerDoParallel(cl)
system.time(foreach(df = 1:4, 
                    .export=c('scanone', 'bayesint', 'cim', "makeqtl","fitqtl","filter","group_by","select","effectplot","sim.geno","setDT"), 
                    .packages=c('qtl','snow','dplyr',"data.table")) %dopar% {
                      #for(df in seq_len(4)) {
                      sug = dflist[df]
                      coVar = data.frame(covars[df])
                      marker.df = markerDF[df]
                      
                      sink(file=paste("B",df, "QTL ",t,".txt"),split=TRUE)
                      pdf(file = paste("B",df, "QTL",t,".pdf"), paper="special", width = 8.5, height = 11, 
                          family="Times", pointsize=11, bg="white", fg="black")
                      print(paste("This is trait ", t, "for data frame B", df))
                      
                      ############################################################
                      # Ok, wait a second... let us examine the data
                      ############################################################
                      # par(mar=c(4,4.5,4.5,3.5)) 
                      #plot(sug)
                      
                      #plotMissing(sug)
                      #plotMap(sug)
                      #plotPheno(sug, pheno.col=t)
                      #Sug<-est.rf(sug)
                      #par(mar=c(4,4,4,4)) 
                      #plotRF(Sug)
                      ############################################################
                      # Single-QTL analysis using interval marker
                      ############################################################
                      print("HK QTLs")
                      
                      out.hk <- scanone(sug[[1]], method="hk",pheno.col=t, addcovar = coVar ) # a fast approximation to interval mapping based on a regression function called Haley-Knott
                      print(summary(out.hk))
                      
                      #plot(out.hk, col="red")
                      
                      #sug <- sim.geno(sug, step=1, n.draws=64)#generates imputations for gentoype data
                      #out.imp <- scanone(sug, method="imp",pheno.col=t)#multple imupation method
                      
                      ############################################################
                      # Permutation tests
                      ############################################################
                      print("Permutation testing")
                      
                      operm <- scanone(sug[[1]], method="hk", n.perm=perms, pheno.col=t, addcovar = coVar, n.cluster=clus)
                      #plot(operm)
                      save(operm, file = paste0("B",df,".Perms.RData"))
                      #load("B1.Perms.RData")
                      #operm = c(operm)
                      #print(summary(operm))
                      #print(summary(operm, alpha=c(0.001, 0.05, 0.2)))
                      #print("Summary of Permutation QTLs")
                      summ.out=summary(out.hk, perms=operm, alpha=0.05, pvalues=TRUE)
                      #summ.out
                      print("Looking for signficant QTL's")
                      if(length(summ.out$lod)==0){
                        print("No significant QTLs")
                        dev.off()
                        sink()
                      } else{
                        print(summ.out)
                        chrs = print(summ.out$chr)
                        posi = print(summ.out$pos) 
                        ###########################################################
                        #QTL support intervals
                        ###########################################################
                        for(i in chrs){
                          #VAT<-c(VAT,lodint(out.hk, chr=i, 1.5)) #1.5 LOD support interval
                          print("Support interval at 1.5 LOD")
                          #si<-bayesint(out.hk, chr=i, 0.95, expandtomarkers=TRUE)
                          si<-lodint(out.hk, i, 1.5, expandtomarkers=TRUE) #expand to nearest flanking marker
                          #bayesint(out.hk, i, 0.95, expandtomarkers=TRUE)
                          print(si)
                        }
                        print("ANOVA results of HK")
                        qtl <- makeqtl(sug[[1]], chr=chrs,
                                       pos=posi,what="prob")
                        xyz = paste0(qtl$altname, collapse="+")
                        xyz<-paste0('y~',xyz)
                        
                        out.fq = fitqtl(sug[[1]], qtl=qtl,pheno.col=t, formula=xyz, covar=coVar,  method="hk"
                                        ,get.ests=TRUE, tol=T, model="normal",dropone=T)#get estimated QTL effects
                        print(summary(out.fq))#negetive est mean decrease, find transgressive alleles)
                        
                        
                        ###########################################################
                        #WinQTLCart/r
                        ##########################################################
                        cat(df," dataset for WinQTLCart/r")
                        out.cim.20 <- cim(sug[[1]],pheno.col=t, n.marcovar=3, map.function = "kosambi",
                                          window=2.5, error.prob=0.0001, method="hk", n.perm=perms)
                        
                        out.cim <- cim(sug[[1]],pheno.col=t, n.marcovar=3, map.function = "kosambi",
                                       window=2.5, error.prob=0.0001, method="hk")
                        print(paste0(df," WinQTLCart/r threshold at 95%"))
                        #load("B1Perms.CIM.RData")
                        
                        summ.cim=print(summary(out.cim.20))
                        
                        out.cim.filter = out.cim %>% filter(lod >= as.numeric(summ.cim[1,1]))
                        print(paste0("signficant QTL results of WinQTLCart/r by perms of ", perms))
                        print(out.cim.filter)
                        
                        write.csv(out.cim.filter, paste0("B",df,"WinQTLCart/r.Results.csv"))
                        save(out.cim.20, file = paste0("B",df,"Perms.WinQTLCart/r.RData"))
                        
                        #######################################################################33
                        
                        chrs.cim = out.cim.filter[!duplicated(out.cim.filter$chr),]
                        
                        posi = out.cim.filter %>% group_by(chr) %>% filter(lod == max(lod)) %>% select(pos)
                        
                        
                        print(paste0("ANOVA results of WinQTLCart/r"))
                        qtl <- makeqtl(sug[[1]], chr=chrs$chr,
                                       pos=posi$pos,what="prob")
                        xyz = paste0(qtl$altname, collapse="+")
                        xyz<-paste0('y~',xyz)
                        
                        out.fq = fitqtl(sug[[1]], qtl=qtl,pheno.col=t, formula=xyz, covar=coVar,  method="hk"
                                        ,get.ests=TRUE, tol=T, model="normal",dropone=T)#get estimated QTL effects
                        print(summary(out.fq))
                        
                       
                        #attach()
                        #########################################################
                        thresh1<-summary(operm, alpha=c(0.05, 0.2, 0.8))# creates the 37%, 90%, 95% significance threshold lines
                        
                        plot(out.hk, out.cim,
                             main=paste0("Mainscan Plot bp For Chroms ",chrs$chr), 
                             xlab="Chromosome; blue = WinQTLCart/r; black = HK", 
                             ylab="LOD",
                             chr = chrs$chr
                        )
                        #plots LODxchromsome
                        #chomo = rbind(chrs, chrs.cim)
                        add.cim.covar(out.cim,  col="green")
                        abline(h=as.numeric(summ.cim[1,1]), lty="dotted", lwd=2, col="blue") #creates the signifigance threshold
                        abline(h=thresh1[1], lty="dotted", lwd=2, col="green") #creates the signifigance threshold
                        abline(h=thresh1[2], lty="dotted", lwd=2, col="orange") #creates the signifigance threshold
                        abline(h=thresh1[3], lty="dotted", lwd=2, col="red") #creates the signifigance threshold
                        
                        ################################################
                        #################QTL Effects Plot###############
                        M1 = setDT(data.frame(summ.out),keep.rownames=T)[,1]
                        M2 = setDT(data.frame(out.cim.filter),keep.rownames=T)[,1]
                        
                        Marks = rbind(M1,M2); Marks = Marks[!duplicated(Marks), ]
                        hyper <- sim.geno(sug[[1]], error.prob=0.0001,
                                          step=0, n.draws = 1)
                        
                        for(i in Marks$rn){
                          
                          print(paste0("Markers Effects per Allele for ", i ))
                          Marker = marker.df[[1]][-c(1:2),] %>% select(i)
                          eff = effectplot(hyper, mname1 = i, mark1 = as.matrix(Marker), pheno.col = t,  draw = F )
                          print(eff)
                          
                        }
                        
                        print("--------------------------------------DONE---------------------------------------")
                        dev.off()
                        sink()
                        
                      }
                      
                    })
stopCluster(cl)



###################################################
#Co-factor
###################################################
print(summ.out)

mar <- find.marker(sug[[1]], chr=c(1),pos=c(192 )) #15.275746, 163.863020, 106.004890; 5,8,9
mar
g<-pull.geno(sug[[1]])[,mar]
sum(is.na(g))

#mar1 <- find.marker(sug, c(2), c(57.7))
#mar1
#g1<-pull.geno(sug)[,mar1]
#sum(is.na(g1))

g2<-cbind(g,coVar)

out.ag<-scanone(sug[[1]], addcovar=coVar)
plot(out.hk, out.ag, col=c("blue", "red"), ylab="LOD score")
summary(out.ag,threshold=2.5)

strat <- (ntyped(sug[[1]]) > 100)
operm.ag <- scanone(sug[[1]], addcovar=coVar,
                    perm.strata=strat, n.perm=perms, n.cluster=7)

summary(operm.ag, alpha=c(0.2, 0.05))
summary(out.ag, perms=operm.ag, alpha=0.05, pvalues=TRUE)

out.ig <- scanone(sug[[1]], addcovar=g, intcovar=g)
plot(out.ig - out.ag, ylab="interaction LOD score")

qtl <- makeqtl(sug[[1]], chr=c(1,5,8,9),
               pos=c(192,15.275746, 163.863020, 106.004890),what="prob")
qtl
out.fq <- fitqtl(qtl=qtl ,sug[[1]],method="hk") #formula=y~Q1+Q2+Q3+Q4
print(summary(out.fq))#d

###########################################################
#CIM
###########################################################
out.cim.20 <- cim(sug[[1]], n.marcovar=3, window=5)
out.cim.40 <- cim(sug[[1]], n.marcovar=3, window=40)
out.cim.inf <- cim(sug[[1]], n.marcovar=3, window=Inf)

chr <- c(1:10)
par(mfrow=c(3,1))
plot(out.ag, out.cim.20, chr=chr, ylab="LOD score",
     col=c("blue", "red"), main="window = 20 cM")
add.cim.covar(out.cim.20, chr=chr, col="green")
plot(out.ag, out.cim.40, chr=chr, ylab="LOD score",
     col=c("blue", "red"), main="window = 40 cM")
add.cim.covar(out.cim.40, chr=chr, col="green")
plot(out.ag, out.cim.inf, chr=chr, ylab="LOD score",
     col=c("blue", "red"), main="window = Inf")
add.cim.covar(out.cim.inf, chr=chr, col="green")









