######################################################################################################
######Rscript for Contrasts of Half-Sib (HS) Quanitative Trait Loci (QTL)
######################################################################################################
rm(list = ls()) #remove environment vairalbes
invisible(gc(reset = T)) #cleans memory "garbage collector"
memory.limit(size = 64000)
setwd("R:/Breeding/MT_TP/Models/QTL")
######################################################################################################
######The following packages need to be loaded
######Load packages:  #
######################################################################################################
library(stats)
library(dplyr)

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

plt.ss.anova = plt.ss %>% select(Plt.Height, Male.Pedigree, PopId, PZE-101060224, AX-91439876, AX-91439876, SYN36244, PZE-109009936)
plt.ns.anova = plt.ss %>% select(Plt.Height, Male.Pedigree, PopId, PZE-103104032, AX-90621116, AX-90596306, AX-90573926,  AX-91427448)
ear.ss.anova = plt.ss %>% select(EarHt, Male.Pedigree, PopId, AX-90529126,AX-91428401,AX-91412692, AX-91341664, AX-91416832 )
ear.ns.anova = plt.ss %>% select(EarHt, Male.Pedigree, PopId, PZE-103160673, AX-91440652, AX-91431026,PZE-109019364 ,SYN2898)



act.df = c(list(plt.ss.anova), list(plt.ns.anova), list(ear.ss.anova), list(ear.ns.anova))
df=1


cont.df = act.df[[df]]

#generate contrast matrix
tapply(cont.df$Male.Pedigree, cont.df$Plt.Height, mean)

#get levels of categorical variable (male Pedigree)
contr.treatment(levels(as.factor(cont.df$Male.Pedigree)))

#assign treatment contrast to categorical variable (male pedigree)
contrasts(cont.df$Plt.Height) = contr.treatment(levels(as.factor(cont.df$Male.Pedigree)))

summary(lm(contr.treatment))
























