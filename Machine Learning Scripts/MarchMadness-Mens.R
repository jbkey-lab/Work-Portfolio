library(XML)
library(rvest)
#library(stringr)
#library(plyr)
#library(dplyr)
#install.packages("ggvis")
#library(ggvis)
library(knitr)
library(neuralnet)
library(caret)
library(rpart)
library(reshape2)
library(caret)
#install.packages("caretEnsemble")
#library(caretEnsemble)
library(data.table)
library(doParallel)
library(tidyverse)

CurrYear <- 2022

##Where you want the output files stored
kaggle=FALSE

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
  trainx2 = data.frame(trainx2)    %>% dplyr::mutate_all(as.numeric) #%>% unique()
  validatex2 = data.frame(validatex2)    %>% dplyr::mutate_all(as.numeric)
  
  
  return(list(data.frame(trainx2),data.frame(validatex2)))
}


if(kaggle){
  init.dir='../input/'
  init.dir <- paste0(init.dir,'mens-march-mania-2022/MDataFiles_Stage1/')
  install.packages("caretEnsemble")
  library(caretEnsemble)
  install.packages("ggvis")
  library(ggvis)
  teams<- paste("../input/teamsalls/TeamsAll.csv", sep ="")
  teamsAll<- read.csv(teams, header = TRUE)
  
}else{
  init.dir='/media/jacoblamkey/Storage/mens-march-mania-2022/MDataFiles_Stage2/'
  library(caretEnsemble)
  library(ggvis)
  teams<- paste("/media/jacoblamkey/Storage/mens-march-mania-2022/MDataFiles_Stage1/teamsalls/TeamsAll.csv", sep ="")
  teamsAll<- read.csv(teams, header =TRUE)
}

cores=parallel::detectCores()
cl <- parallel::makeCluster(cores[1]-1, outfile="")
doParallel::registerDoParallel(cl)



###############################################################################
## Webscrape of Ken Pom Website
YearA<- seq(2002, CurrYear, by=1)

#Generates the list of all the websites that it will pull data from
WebsiteListA <- paste("http://www.kenpom.com/index.php?y=",YearA, sep = "")

WebsiteA <- read_html(WebsiteListA[1])

KenPomRatings <-WebsiteA %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table();

Yeari <-YearA[1]

KenPomYear <- merge(Yeari,KenPomRatings)
KenPomFinal <-KenPomYear

##Counts how many websites are in WesiteList
LoopsA<-length(WebsiteListA)

#Go through each year and pull the data from each website

for(i in 2:LoopsA)  {
  
  WebsiteA <- read_html(WebsiteListA[i])
  
  KenPomRatings <-WebsiteA %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table();
  
  Yeari <-YearA[i]
  
  KenPomYear <- merge(Yeari,KenPomRatings)
  
  KenPomFinal <-rbind(KenPomFinal,KenPomYear)
}

colnames(KenPomFinal)[1]<-"Year"
colnames(KenPomFinal)[2]<-"Rank"
colnames(KenPomFinal)[3]<-"Team"
colnames(KenPomFinal)[4]<-"Conf"
colnames(KenPomFinal)[5]<-"Record"
colnames(KenPomFinal)[6]<-"Pyth"
colnames(KenPomFinal)[7]<-"AdjO"
colnames(KenPomFinal)[8]<-"AdjORank"
colnames(KenPomFinal)[9]<-"AdjD"
colnames(KenPomFinal)[10]<-"AdjDRank"
colnames(KenPomFinal)[11]<-"AdjT"
colnames(KenPomFinal)[12]<-"AdjTRank"
colnames(KenPomFinal)[13]<-"Luck"
colnames(KenPomFinal)[14]<-"LuckRank"
colnames(KenPomFinal)[15]<-"ADJEM_SOS"
colnames(KenPomFinal)[16]<-"PythRank_Opp"
colnames(KenPomFinal)[17]<-"AdjO_Opp"
colnames(KenPomFinal)[18]<-"AdjORank_Opp"
colnames(KenPomFinal)[19]<-"AdjD_Opp"
colnames(KenPomFinal)[20]<-"AdjDRank_Opp"
colnames(KenPomFinal)[21]<-"NCSOS_Pyth"
colnames(KenPomFinal)[22]<-"NCSOS_PythRank"

## Get rid of all other headers and leave only the data
KenPomFinal<-na.omit(KenPomFinal)
KenPomFinal$Team = gsub("\\ [0-9]*$", "", KenPomFinal$Team)
KenPomFinal <- KenPomFinal[-which(KenPomFinal$Rank==""),]
KenPomFinal <- subset(KenPomFinal, Rank != "Rk")
KenPomFinal <- merge(KenPomFinal, teamsAll, by.x = "Team", by.y = "KenPom", all.x = TRUE)
KenPomFinal <- unique(KenPomFinal[,1:23])


#KenPomTeam <- as.data.frame(KenPomFinal$Team[!duplicated(KenPomFinal$Team)])
#filenameKP <- paste(FileLocation,"KenPomTeam.csv",sep="")
#write.csv(KenPomTeam,filenameKP,row.names=FALSE)


######################################################################################################
### Website scraping for HS Rankings

YearB<- seq(1998, CurrYear-1, by=1)

WebsiteListB<- paste("http://www.basketball-reference.com/awards/recruit_rankings_",YearB,".html",sep ="")

WebsiteB <- read_html(WebsiteListB[1])

HSRatings <-WebsiteB %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table();

Yearii <- YearB[1]

HSRatings <- HSRatings[,1:12]
HSRatingYear <- merge(Yearii,HSRatings)
HSRatingFinal<-HSRatingYear

LoopsB<-length(WebsiteListB)

for(x in 2:LoopsB)  {
  WebsiteB <- read_html(WebsiteListB[x])
  
  HSRatings <-WebsiteB %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table();
  
  Yearii <- YearB[x]
  
  HSRatings <- HSRatings[,1:12]  
  HSRatingYear <- merge(Yearii,HSRatings)
  HSRatingFinal<- rbind(HSRatingFinal, HSRatingYear)
}


colnames(HSRatingFinal)[1]<- "Year"
colnames(HSRatingFinal)[2]<- "Rank"
#colnames(HSRatingFinal)[2]<- "RSCI"
colnames(HSRatingFinal)[3]<- "Player"
colnames(HSRatingFinal)[4]<- "Draft"
colnames(HSRatingFinal)[5]<- "Round"
colnames(HSRatingFinal)[6]<- "Pick"
colnames(HSRatingFinal)[7]<- "College"
colnames(HSRatingFinal)[8]<- "NBA_Tm"
colnames(HSRatingFinal)[9]<- "From"
colnames(HSRatingFinal)[10]<- "To"
colnames(HSRatingFinal)[11]<- "WS"
colnames(HSRatingFinal)[12]<- "Blank"
colnames(HSRatingFinal)[13]<- "AS"

#filenameHSRating <- paste(FileLocation,"HSRatingData.csv",sep="")
#write.csv(HSRatingFinal,filenameHSRating,row.names=FALSE)

#HSRatingFile<- paste(FileLocation,"HSRatingData.csv", sep ="")
#HSRatingFinal<- read.csv(HSRatingFile, header = TRUE)


HSRatingFinal<-subset(HSRatingFinal, Rank !="Rk")

HSTest<- subset(HSRatingFinal, College !="")
HSTest$Rank <- as.integer(as.character(HSTest$Rank))
HSTest<- within(HSTest, YearLeft<-ifelse(Draft == "",Year+4,Draft))
HSTest<- within(HSTest, Freshman <- ifelse(YearLeft>Year,Year+1,"NA"))
HSTest <- within(HSTest, Sophomore <- ifelse(YearLeft>Year+1,Year+2,"NA"))
HSTest <- within(HSTest, Junior <- ifelse(YearLeft>Year+2,Year+3,"NA"))
HSTest <- within(HSTest, Senior <- ifelse(YearLeft>Year+3,Year+4,"NA"))
HSTest <- within(HSTest, TopTen<-ifelse(as.numeric(Rank)<11,"TopTen","TopHundred"))
sapply(HSTest, class)

FreshTop<- as.data.frame(table("College" = HSTest$College,"Year" = HSTest$Freshman, "TopTen" = HSTest$TopTen))
colnames(FreshTop)[4]<-"Freshman"
SophTop<- as.data.frame(table("College" = HSTest$College,"Year" = HSTest$Sophomore, "TopTen" = HSTest$TopTen))
colnames(SophTop)[4]<-"Sophomore"
JuniorTop<- as.data.frame(table("College" = HSTest$College,"Year" = HSTest$Junior, "TopTen" = HSTest$TopTen))
colnames(JuniorTop)[4]<-"Junior"
SeniorTop<- as.data.frame(table("College" = HSTest$College,"Year" = HSTest$Senior, "TopTen" = HSTest$TopTen))
colnames(SeniorTop)[4]<-"Senior"

HSFinal<-merge(FreshTop,SophTop, by = c("College","Year","TopTen"))
HSFinal <-merge(HSFinal,JuniorTop, by = c("College","Year","TopTen"))
HSFinal <-merge(HSFinal,SeniorTop, by = c("College","Year","TopTen"))
HSFinal <- merge(HSFinal, teamsAll, by.x = "College", by.y = "HSTeam")
HSFinal$Year <- as.numeric(as.character(HSFinal$Year))

HSFinalT100 <- subset(HSFinal, HSFinal$TopTen == "TopHundred")
HSFinalT100 <-HSFinalT100 %>% dplyr::group_by(Team_Id, Year) %>% dplyr::summarise(FreshmanT100 = sum(Freshman),SophomoreT100 = sum(Sophomore),JuniorT100 = sum(Junior),SeniorT100 = sum(Senior))

HSFinalT10 <- subset(HSFinal, HSFinal$TopTen == "TopTen")
HSFinalT10 <-HSFinalT10 %>% dplyr::group_by(Team_Id, Year) %>% dplyr::summarise(FreshmanT10 = sum(Freshman),SophomoreT10 = sum(Sophomore),JuniorT10 = sum(Junior),SeniorT10 = sum(Senior))



HSTeams<- as.data.frame(HSFinal$College[!duplicated(HSFinal$College)])
#filenameHS <- paste(FileLocation,"HSTeam.csv",sep="")
#write.csv(HSTeams,filenameHS,row.names=FALSE)


##########################################################################################
### Bring in data from Kaggle site

regseasonDetail<-read.csv(paste0(init.dir,'MRegularSeasonDetailedResults.csv'),header = TRUE)

regseasonWin <-regseasonDetail
regseasonWin <-within(regseasonWin, Win<-1)

colnames(regseasonWin)[1] <- "Year"
colnames(regseasonWin)[2] <- "daynum"
colnames(regseasonWin)[3] <- "Team"
colnames(regseasonWin)[4] <- "Score"
colnames(regseasonWin)[5] <- "OppTeam"
colnames(regseasonWin)[6] <- "OppScore"
colnames(regseasonWin)[7] <- "Location"
colnames(regseasonWin)[8] <- "numOT"
colnames(regseasonWin)[9] <- "fgm"
colnames(regseasonWin)[10] <- "fga"
colnames(regseasonWin)[11] <- "fgm3"
colnames(regseasonWin)[12] <- "fga3"
colnames(regseasonWin)[13] <- "ftm"
colnames(regseasonWin)[14] <- "fta"
colnames(regseasonWin)[15] <- "or"
colnames(regseasonWin)[16] <- "dr"
colnames(regseasonWin)[17] <- "ast"
colnames(regseasonWin)[18] <- "to"
colnames(regseasonWin)[19] <- "stl"
colnames(regseasonWin)[20] <- "blk"
colnames(regseasonWin)[21] <- "pf"
colnames(regseasonWin)[22] <- "Oppfgm"
colnames(regseasonWin)[23] <- "Oppfga"
colnames(regseasonWin)[24] <- "Oppfgm3"
colnames(regseasonWin)[25] <- "Oppfga3"
colnames(regseasonWin)[26] <- "Oppftm"
colnames(regseasonWin)[27] <- "Oppfta"
colnames(regseasonWin)[28] <- "Oppor"
colnames(regseasonWin)[29] <- "Oppdr"
colnames(regseasonWin)[30] <- "Oppast"
colnames(regseasonWin)[31] <- "Oppto"
colnames(regseasonWin)[32] <- "Oppstl"
colnames(regseasonWin)[33] <- "Oppblk"
colnames(regseasonWin)[34] <- "Opppf"

regseasonLoss <-regseasonDetail
regseasonLoss <-within(regseasonLoss, Win<-0)

colnames(regseasonLoss)[1] <- "Year"
colnames(regseasonLoss)[2] <- "daynum"
colnames(regseasonLoss)[3] <- "OppTeam"
colnames(regseasonLoss)[4] <- "OppScore"
colnames(regseasonLoss)[5] <- "Team"
colnames(regseasonLoss)[6] <- "Score"
colnames(regseasonLoss)[7] <- "Location"
colnames(regseasonLoss)[8] <- "numOT"
colnames(regseasonLoss)[9] <- "Oppfgm"
colnames(regseasonLoss)[10] <- "Oppfga"
colnames(regseasonLoss)[11] <- "Oppfgm3"
colnames(regseasonLoss)[12] <- "Oppfga3"
colnames(regseasonLoss)[13] <- "Oppftm"
colnames(regseasonLoss)[14] <- "Oppfta"
colnames(regseasonLoss)[15] <- "Oppor"
colnames(regseasonLoss)[16] <- "Oppdr"
colnames(regseasonLoss)[17] <- "Oppast"
colnames(regseasonLoss)[18] <- "Oppto"
colnames(regseasonLoss)[19] <- "Oppstl"
colnames(regseasonLoss)[20] <- "Oppblk"
colnames(regseasonLoss)[21] <- "Opppf"
colnames(regseasonLoss)[22] <- "fgm"
colnames(regseasonLoss)[23] <- "fga"
colnames(regseasonLoss)[24] <- "fgm3"
colnames(regseasonLoss)[25] <- "fga3"
colnames(regseasonLoss)[26] <- "ftm"
colnames(regseasonLoss)[27] <- "fta"
colnames(regseasonLoss)[28] <- "or"
colnames(regseasonLoss)[29] <- "dr"
colnames(regseasonLoss)[30] <- "ast"
colnames(regseasonLoss)[31] <- "to"
colnames(regseasonLoss)[32] <- "stl"
colnames(regseasonLoss)[33] <- "blk"
colnames(regseasonLoss)[34] <- "pf"

regseasonLoss$Location<- ifelse(regseasonLoss$Location== "H","A", ifelse(regseasonLoss$Location=="A","H","N"))

RegSeasonTotal <- rbind(regseasonWin,regseasonLoss)

RegSeasonTotal <- within(RegSeasonTotal, FGPerct <- fgm/fga)
RegSeasonTotal <- within(RegSeasonTotal, FG3Perct <- fgm3/fga3)
RegSeasonTotal <- within(RegSeasonTotal, FTPerct <- ifelse(is.na(ftm/fta),0,ftm/fta))
RegSeasonTotal <- within(RegSeasonTotal, O_Eff<- 100*Score /(.5*((fga+.4*fta-1.07*(or/(or+Oppdr))*(fga-fgm)+to)+(Oppfga+.4*Oppfta-1.07*(Oppor/(Oppor+dr))*(Oppfga-Oppfgm)+Oppto))))
RegSeasonTotal <- within(RegSeasonTotal, D_Eff<- 100*OppScore /(Oppfga - Oppor + Oppto +(.4*Oppfta)))

### Need to do if NA step on these variables

sumRegSeason <- RegSeasonTotal %>% dplyr::group_by(Team, Year) %>% dplyr::summarise(fgm3agg=mean(fgm3),fga3agg=mean(fga3),fgmagg=mean(fgm),fgaagg=mean(fga),ftmagg=mean(ftm),ftaagg=mean(fta),
                                                                                    oragg = mean(or), dragg= mean(dr),astagg = mean(ast), toagg=mean(to),stlagg = mean(stl),blkagg =mean(blk), 
                                                                                    pfagg = mean(pf), FGPerctVar = var(FGPerct),FG3PerctVar =var(FG3Perct),FTPerctVar = var(FTPerct), toVar=var(to), OffRtg = mean(O_Eff),DffRtg = mean(D_Eff))
sumRegSeason <- within(sumRegSeason, ast2to <- astagg/toagg)


last30regseas <- subset(RegSeasonTotal, daynum > 100)
last30regseas <-last30regseas %>% dplyr::group_by(Team, Year) %>% dplyr::summarise(fgm3aggl30=mean(fgm3),fga3aggl30=mean(fga3),fgmaggl30=mean(fgm),fgaaggl30=mean(fga),ftmaggl30=mean(ftm),ftaaggl30=mean(fta),
                                                                                   oraggl30 = mean(or), draggl30= mean(dr),astaggl30 = mean(ast), toaggl30=mean(to),stlaggl30 = mean(stl),blkaggl30 =mean(blk), 
                                                                                   pfaggl30 = mean(pf), FGPerctVarl30 = var(FGPerct),FG3PerctVarl30 =var(FG3Perct),FTPerctVarl30 = var(FTPerct), toVarl30=var(to), OffRtgl30 = mean(O_Eff),DffRtgl30 = mean(D_Eff))
last30regseas <- within(last30regseas, ast2tol30 <- astaggl30/toaggl30)

###############3
#MasseyOrdinals
MasseyOrdinals<-read.csv( paste0(init.dir,'MMasseyOrdinals_thruDay128.csv'),header = TRUE)

MasseyOrdinalTable <- melt(MasseyOrdinals, id.vars = c("Season","SystemName","TeamID","RankingDayNum"), measure.vars = c("OrdinalRank"))
MasseyOrdinalTable <- dcast(MasseyOrdinalTable, Season + SystemName + TeamID ~ RankingDayNum + variable, value.var = "value")
MasseyOrdinalTable <- within(MasseyOrdinalTable, MinRank <- apply(MasseyOrdinalTable[,4:93],1,min, na.rm = TRUE))
MasseyOrdinalTable <- within(MasseyOrdinalTable, MaxRank <- apply(MasseyOrdinalTable[,4:93],1,max, na.rm = TRUE))
MasseyOrdinalTable <- within(MasseyOrdinalTable, VarRank <- apply(MasseyOrdinalTable[,4:93],1,var, na.rm = TRUE))

colnames(MasseyOrdinalTable)[93] <- "FinalRanking"
MasseyOrdinalTable$FinalRanking[is.na(MasseyOrdinalTable$FinalRanking)] <- MasseyOrdinalTable$`128_OrdinalRank`[is.na(MasseyOrdinalTable$FinalRanking)]

MasseyOrdinalTable <- select(MasseyOrdinalTable, c(Season, SystemName, TeamID,FinalRanking, MinRank,MaxRank, VarRank))
MasseyOrdinalTable <- melt(MasseyOrdinalTable, id.vars = c("Season","SystemName","TeamID"), measure.vars = c("FinalRanking","MinRank","MaxRank","VarRank"))
MasseyOrdinalTable <- dcast(MasseyOrdinalTable, Season + TeamID ~ SystemName + variable, value.var = "value")

FinalRankings <- select(MasseyOrdinalTable, c(Season, TeamID, MAS_FinalRanking,MAS_MinRank,MAS_MaxRank,MAS_VarRank,COL_FinalRanking,COL_MinRank,COL_MaxRank,COL_VarRank,RPI_FinalRanking,RPI_MinRank,RPI_MaxRank,RPI_VarRank)) 
#colSums(is.na(FinalRankings))

#########################################################
### Tournament Data From kaggle
TourneyCompact<-read.csv(paste0(init.dir,'MNCAATourneyCompactResults.csv'),header = TRUE)
TourneySeeds<-read.csv(paste0(init.dir,'MNCAATourneySeeds.csv'),header = TRUE)
#TourneySeeds <-tourney_seeds

TourneyWin <-TourneyCompact
TourneyWin <-within(TourneyWin, Win<-1)

colnames(TourneyWin)[1]<- "Year"
colnames(TourneyWin)[2]<- "Daynum"
colnames(TourneyWin)[3]<- "Team"
colnames(TourneyWin)[4]<- "Score"
colnames(TourneyWin)[5]<- "OppTeam"
colnames(TourneyWin)[6]<- "OppScore"
colnames(TourneyWin)[7]<- "Location"
colnames(TourneyWin)[8]<- "Numot"

TourneyLose <-TourneyCompact
TourneyLose <-within(TourneyLose, Win<-0)

colnames(TourneyLose)[1]<- "Year"
colnames(TourneyLose)[2]<- "Daynum"
colnames(TourneyLose)[3]<- "OppTeam"
colnames(TourneyLose)[4]<- "OppScore"
colnames(TourneyLose)[5]<- "Team"
colnames(TourneyLose)[6]<- "Score"
colnames(TourneyLose)[7]<- "Location"
colnames(TourneyLose)[8]<- "Numot"

TourneyTotal <- rbind(TourneyWin,TourneyLose)

TourneySeeds<- within(TourneySeeds,SeedNum<-as.numeric(gsub("\\D","",TourneySeeds$Seed)))

TourneyTotal <- merge(TourneyTotal,TourneySeeds, by.x =c("Year","Team"), by.y = c("Season","TeamID"),all.x= TRUE)
TourneyTotal <- merge(TourneyTotal,TourneySeeds, by.x =c("Year","OppTeam"), by.y = c("Season","TeamID"),all.x= TRUE)
TourneyTotal <- within(TourneyTotal, SeedDiff<- TourneyTotal$SeedNum.x-TourneyTotal$SeedNum.y)

Tourney03to11 <- subset(TourneyTotal, Year>2002)
Tourney03to11 <- merge(Tourney03to11, KenPomFinal[,2:23], by.x = c("Year","Team"),by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, KenPomFinal[,2:23], by.x = c("Year","OppTeam"),by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, sumRegSeason, by.x = c("Year","Team"), by.y = c("Year","Team"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, sumRegSeason, by.x = c("Year","OppTeam"), by.y = c("Year","Team"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, last30regseas, by.x = c("Year","Team"), by.y = c("Year","Team"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, last30regseas, by.x = c("Year","OppTeam"), by.y = c("Year","Team"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, HSFinalT10, by.x = c("Year","Team"), by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, HSFinalT10, by.x = c("Year","OppTeam"), by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, HSFinalT100, by.x = c("Year","Team"), by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, HSFinalT100, by.x = c("Year","OppTeam"), by.y = c("Year","Team_Id"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, FinalRankings, by.x = c("Year","Team"), by.y = c("Season","TeamID"),all.x = TRUE)
Tourney03to11 <- merge(Tourney03to11, FinalRankings, by.x = c("Year","OppTeam"), by.y = c("Season","TeamID"),all.x = TRUE)

Tourney03to11 <- within (Tourney03to11, AdjDDiff<-as.numeric(AdjD.x)-as.numeric(AdjD.y))
Tourney03to11 <- within (Tourney03to11, AdjD_OppDiff<-as.numeric(AdjD_Opp.x)-as.numeric(AdjD_Opp.y))

Tourney03to11 <- within (Tourney03to11, AdjDRankDiff<-as.numeric(AdjDRank.x)-as.numeric(AdjDRank.y))
Tourney03to11 <- within (Tourney03to11, AdjDRank_OppDiff<-as.numeric(AdjDRank_Opp.x)-as.numeric(AdjDRank_Opp.y))
Tourney03to11 <- within (Tourney03to11, AdjODiff<-as.numeric(AdjO.x)-as.numeric(AdjO.y))
Tourney03to11 <- within (Tourney03to11, AdjO_OppDiff<-as.numeric(AdjO_Opp.x)-as.numeric(AdjO_Opp.y))
Tourney03to11 <- within (Tourney03to11, AdjORankDiff<-as.numeric(AdjORank.x)-as.numeric(AdjORank.y))
Tourney03to11 <- within (Tourney03to11, AdjORank_OppDiff<-as.numeric(AdjORank_Opp.x)-as.numeric(AdjORank_Opp.y))
Tourney03to11 <- within (Tourney03to11, AdjTDiff<-as.numeric(AdjT.x)-as.numeric(AdjT.y))
Tourney03to11 <- within (Tourney03to11, AdjTRankDiff<-as.numeric(AdjTRank.x)-as.numeric(AdjTRank.y))
Tourney03to11 <- within (Tourney03to11, ast2tol30Diff<-as.numeric(ast2tol30.x)-as.numeric(ast2tol30.y))
Tourney03to11 <- within (Tourney03to11, astaggDiff<-as.numeric(astagg.x)-as.numeric(astagg.y))
Tourney03to11 <- within (Tourney03to11, astaggl30Diff<-as.numeric(astaggl30.x)-as.numeric(astaggl30.y))
Tourney03to11 <- within (Tourney03to11, blkaggDiff<-as.numeric(blkagg.x)-as.numeric(blkagg.y))
Tourney03to11 <- within (Tourney03to11, blkaggl30Diff<-as.numeric(blkaggl30.x)-as.numeric(blkaggl30.y))
Tourney03to11 <- within (Tourney03to11, draggDiff<-as.numeric(dragg.x)-as.numeric(dragg.y))
Tourney03to11 <- within (Tourney03to11, draggl30Diff<-as.numeric(draggl30.x)-as.numeric(draggl30.y))
Tourney03to11 <- within (Tourney03to11, FG3PerctVarDiff<-as.numeric(FG3PerctVar.x)-as.numeric(FG3PerctVar.y))
Tourney03to11 <- within (Tourney03to11, FG3PerctVarl30Diff<-as.numeric(FG3PerctVarl30.x)-as.numeric(FG3PerctVarl30.y))
Tourney03to11 <- within (Tourney03to11, fga3aggDiff<-as.numeric(fga3agg.x)-as.numeric(fga3agg.y))
Tourney03to11 <- within (Tourney03to11, fga3aggl30Diff<-as.numeric(fga3aggl30.x)-as.numeric(fga3aggl30.y))
Tourney03to11 <- within (Tourney03to11, fgaaggDiff<-as.numeric(fgaagg.x)-as.numeric(fgaagg.y))
Tourney03to11 <- within (Tourney03to11, fgaaggl30Diff<-as.numeric(fgaaggl30.x)-as.numeric(fgaaggl30.y))
Tourney03to11 <- within (Tourney03to11, fgm3aggDiff<-as.numeric(fgm3agg.x)-as.numeric(fgm3agg.y))
Tourney03to11 <- within (Tourney03to11, fgm3aggl30Diff<-as.numeric(fgm3aggl30.x)-as.numeric(fgm3aggl30.y))
Tourney03to11 <- within (Tourney03to11, fgmaggDiff<-as.numeric(fgmagg.x)-as.numeric(fgmagg.y))
Tourney03to11 <- within (Tourney03to11, fgmaggl30Diff<-as.numeric(fgmaggl30.x)-as.numeric(fgmaggl30.y))
Tourney03to11 <- within (Tourney03to11, FGPerctVarDiff<-as.numeric(FGPerctVar.x)-as.numeric(FGPerctVar.y))
Tourney03to11 <-within (Tourney03to11, FGPerctVarl30Diff<-as.numeric(FGPerctVarl30.x)-as.numeric(FGPerctVarl30.y))
Tourney03to11 <-within (Tourney03to11, FreshmanT10Diff<-as.numeric(FreshmanT10.x)-as.numeric(FreshmanT10.y))
Tourney03to11 <-within (Tourney03to11, FreshmanT100Diff<-as.numeric(FreshmanT100.x)-as.numeric(FreshmanT100.y))
Tourney03to11 <-within (Tourney03to11, ftaaggDiff<-as.numeric(ftaagg.x)-as.numeric(ftaagg.y))
Tourney03to11 <-within (Tourney03to11, ftaaggl30Diff<-as.numeric(ftaaggl30.x)-as.numeric(ftaaggl30.y))
Tourney03to11 <-within (Tourney03to11, ftmaggDiff<-as.numeric(ftmagg.x)-as.numeric(ftmagg.y))
Tourney03to11 <-within (Tourney03to11, ftmaggl30Diff<-as.numeric(ftmaggl30.x)-as.numeric(ftmaggl30.y))
Tourney03to11 <-within (Tourney03to11, FTPerctVarDiff<-as.numeric(FTPerctVar.x)-as.numeric(FTPerctVar.y))
Tourney03to11 <-within (Tourney03to11, FTPerctVarl30Diff<-as.numeric(FTPerctVarl30.x)-as.numeric(FTPerctVarl30.y))
Tourney03to11 <-within (Tourney03to11, JuniorT10Diff<-as.numeric(JuniorT10.x)-as.numeric(JuniorT10.y))
Tourney03to11 <-within (Tourney03to11, JuniorT100Diff<-as.numeric(JuniorT100.x)-as.numeric(JuniorT100.y))
Tourney03to11 <-within (Tourney03to11, LuckDiff<-as.numeric(Luck.x)-as.numeric(Luck.y))
Tourney03to11 <-within (Tourney03to11, LuckRankDiff<-as.numeric(LuckRank.x)-as.numeric(LuckRank.y))
Tourney03to11 <-within (Tourney03to11, NCSOS_PythDiff<-as.numeric(NCSOS_Pyth.x)-as.numeric(NCSOS_Pyth.y))
Tourney03to11 <-within (Tourney03to11, NCSOS_PythRankDiff<-as.numeric(NCSOS_PythRank.x)-as.numeric(NCSOS_PythRank.y))
Tourney03to11 <-within (Tourney03to11, oraggDiff<-as.numeric(oragg.x)-as.numeric(oragg.y))
Tourney03to11 <-within (Tourney03to11, oraggl30Diff<-as.numeric(oraggl30.x)-as.numeric(oraggl30.y))
Tourney03to11 <-within (Tourney03to11, pfaggDiff<-as.numeric(pfagg.x)-as.numeric(pfagg.y))
Tourney03to11 <-within (Tourney03to11, pfaggl30Diff<-as.numeric(pfaggl30.x)-as.numeric(pfaggl30.y))
Tourney03to11 <-within (Tourney03to11, PythDiff<-as.numeric(Pyth.x)-as.numeric(Pyth.y))
Tourney03to11 <-within (Tourney03to11, ADJEM_SOSDiff<-as.numeric(ADJEM_SOS.x)-as.numeric(ADJEM_SOS.y))
Tourney03to11 <-within (Tourney03to11, PythRank_OppDiff<-as.numeric(PythRank_Opp.x)-as.numeric(PythRank_Opp.y))
Tourney03to11 <-within (Tourney03to11, RankDiff<-as.numeric(Rank.x)-as.numeric(Rank.y))
Tourney03to11 <-within (Tourney03to11, SeedNumDiff<-as.numeric(SeedNum.x)-as.numeric(SeedNum.y))
Tourney03to11 <-within (Tourney03to11, SeniorT10Diff<-as.numeric(SeniorT10.x)-as.numeric(SeniorT10.y))
Tourney03to11 <-within (Tourney03to11, SeniorT100Diff<-as.numeric(SeniorT100.x)-as.numeric(SeniorT100.y))
Tourney03to11 <-within (Tourney03to11, SophomoreT10Diff<-as.numeric(SophomoreT10.x)-as.numeric(SophomoreT10.y))
Tourney03to11 <-within (Tourney03to11, SophomoreT100Diff<-as.numeric(SophomoreT100.x)-as.numeric(SophomoreT100.y))
Tourney03to11 <-within (Tourney03to11, stlaggDiff<-as.numeric(stlagg.x)-as.numeric(stlagg.y))
Tourney03to11 <-within (Tourney03to11, stlaggl30Diff<-as.numeric(stlaggl30.x)-as.numeric(stlaggl30.y))
Tourney03to11 <-within (Tourney03to11, toaggDiff<-as.numeric(toagg.x)-as.numeric(toagg.y))
Tourney03to11 <-within (Tourney03to11, toaggl30Diff<-as.numeric(toaggl30.x)-as.numeric(toaggl30.y))
Tourney03to11 <-within (Tourney03to11, toVarl30Diff<-as.numeric(toVarl30.x)-as.numeric(toVarl30.y))
Tourney03to11 <- within( Tourney03to11, PerctOfrom3pt.x<- (3*fgm3agg.x)/(fgm3agg.x*3+fgmagg.x*2+ftmagg.x*1))
Tourney03to11 <- within( Tourney03to11, PerctOfrom3pt.y<- (3*fgm3agg.y)/(fgm3agg.y*3+fgmagg.y*2+ftmagg.y*1))
Tourney03to11 <- within( Tourney03to11, PerctOfrom3ptDiff<- PerctOfrom3pt.x-PerctOfrom3pt.y)
Tourney03to11 <- within( Tourney03to11, PerctOfromFT.x<- (1*ftmagg.x)/(fgm3agg.x*3+fgmagg.x*2+ftmagg.x*1))
Tourney03to11 <- within( Tourney03to11, PerctOfromFT.y<- (1*ftmagg.y)/(fgm3agg.y*3+fgmagg.y*2+ftmagg.y*1))
Tourney03to11 <- within( Tourney03to11, PerctOfromFTDiff<- PerctOfrom3pt.x-PerctOfrom3pt.y)
Tourney03to11<- within( Tourney03to11, AggresPrct.x<- (ftmagg.x/fgmagg.x)/(fgm3agg.x/fgmagg.x))
Tourney03to11 <- within( Tourney03to11, AggresPrct.y<- (ftmagg.y/fgmagg.y)/(fgm3agg.y/fgmagg.y))
Tourney03to11 <- within( Tourney03to11, AggresPrctDiff<- AggresPrct.x-AggresPrct.y)
Tourney03to11 <- within(Tourney03to11, After1stWkd <- ifelse(Daynum>140,1,0))
Power5List <- c("B10","ACC","B12","P10","P12","BE","SEC")
Tourney03to11 <- within(Tourney03to11, Power5.x <- ifelse(Conf.x %in% Power5List,1,0))
Tourney03to11 <- within(Tourney03to11, Power5.y <- ifelse(Conf.y %in% Power5List,1,0))
Tourney03to11 <- within(Tourney03to11, Power5Flag <- Power5.x-Power5.y)
Tourney03to11 <- within(Tourney03to11, RemoveDups <- ifelse(Team>OppTeam,0,1))
Tourney03to11 <- within(Tourney03to11, Eff_L30_Diff<- (OffRtgl30.x-DffRtgl30.x) -(OffRtgl30.y-DffRtgl30.y))
Tourney03to11 <- within(Tourney03to11, OffMomentum.x<- OffRtgl30.x/OffRtg.x)
Tourney03to11 <- within(Tourney03to11, DefMomentum.x<- DffRtgl30.x/DffRtg.x)
Tourney03to11 <- within(Tourney03to11, OffMomentum.y<- OffRtgl30.y/OffRtg.y)
Tourney03to11 <- within(Tourney03to11, DefMomentum.y<- DffRtgl30.y/DffRtg.y)
Tourney03to11 <- within(Tourney03to11, OffMomentumDiff<- OffMomentum.x-OffMomentum.y)
Tourney03to11 <- within(Tourney03to11, DefMomentumDiff<- DefMomentum.x-DefMomentum.y)
Tourney03to11 <- within(Tourney03to11, MAS_Final_Diff<- MAS_FinalRanking.x-MAS_FinalRanking.y)
Tourney03to11 <- within(Tourney03to11, MAS_Min_Diff<- MAS_MinRank.x-MAS_MinRank.y)
Tourney03to11 <- within(Tourney03to11, MAS_Max_Diff<- MAS_MaxRank.x-MAS_MaxRank.y)
Tourney03to11 <- within(Tourney03to11, MAS_Var_Diff<- MAS_VarRank.x-MAS_VarRank.y)
Tourney03to11 <- within(Tourney03to11, COL_Final_Diff<- COL_FinalRanking.x-COL_FinalRanking.y)
Tourney03to11 <- within(Tourney03to11, COL_Min_Diff<- COL_MinRank.x-COL_MinRank.y)
Tourney03to11 <- within(Tourney03to11, COL_Max_Diff<- COL_MaxRank.x-COL_MaxRank.y)
Tourney03to11 <- within(Tourney03to11, COL_Var_Diff<- COL_VarRank.x-COL_VarRank.y)
Tourney03to11 <- within(Tourney03to11, RPI_Final_Diff<- RPI_FinalRanking.x-RPI_FinalRanking.y)
Tourney03to11 <- within(Tourney03to11, RPI_Min_Diff<- RPI_MinRank.x-RPI_MinRank.y)
Tourney03to11 <- within(Tourney03to11, RPI_Max_Diff<- RPI_MaxRank.x-RPI_MaxRank.y)
Tourney03to11 <- within(Tourney03to11, RPI_Var_Diff<- RPI_VarRank.x-RPI_VarRank.y)
Tourney03to11 <- within(Tourney03to11, Eff_L30_Diff<- (OffRtgl30.x-DffRtgl30.x) -(OffRtgl30.y-DffRtgl30.y))
Tourney03to11 <- within(Tourney03to11, OEff_Diff<- OffRtgl30.x -OffRtgl30.y)
Tourney03to11 <- within(Tourney03to11, DEff_Diff<- DffRtgl30.x -DffRtgl30.y)
Tourney03to11[is.na(Tourney03to11)] <- 0

TourneyDataTotal <- subset(Tourney03to11, RemoveDups == 1)

TeamNames <- teamsAll[,c("Team_Id","Team_Name")]
TeamNames <- unique(TeamNames)
colnames(TeamNames)[2]<- "Team_Name"
TourneyDataTotal <- merge(TourneyDataTotal, TeamNames, by.x = "Team",by.y = "Team_Id",all.x = TRUE)
colnames(TeamNames)[2]<- "Opp_Team_Name"
TourneyDataTotal <- merge(TourneyDataTotal, TeamNames, by.x = "OppTeam",by.y = "Team_Id",all.x = TRUE)
colnames(TeamNames)[2]<- "Team_Name"

TourneyData.cats<-TourneyDataTotal[,c(16,17,36,37)]
TourneyData.nums<-TourneyDataTotal[,-c(16,17,36,37)]

TourneyData.nums[] <- lapply(TourneyData.nums, as.numeric)
TourneyDataTotal=cbind(TourneyData.cats,TourneyData.nums)

#TourneyDataTotal=TourneyDataTotal[,c('Win','SeedNumDiff','AdjDDiff','AdjODiff','toVarl30Diff','draggDiff','MAS_Final_Diff','FTPerctVarl30Diff','ADJEM_SOSDiff')]

#myglm <- glm(Win~ 
#SeedNumDiff +AdjDDiff+AdjODiff+toVarl30Diff+draggDiff
#+MAS_Final_Diff+FTPerctVarl30Diff+ADJEM_SOSDiff
#,data= TourneyDataTotal, family= binomial())
#summary(myglm)
TourneyDataTotal = data.frame(TourneyDataTotal)
datasets = trainVal(data = TourneyDataTotal, colToInd= "Year", sample = 0.95)
gc()


CaretTourneyData=TourneyDataTotal[,c(13,17:271)]
select = c("OppTeam", "Team", "Year", "Id","Conf.x", "Seed.x", "Record.x", "SeedNum.x", "Seed.y", "Conf.y", "Record.y",
           "Team_Name","Opp_Team_Name","After1stWkd")
CaretTourneyData = CaretTourneyData[, !(names(CaretTourneyData) %in% select)]
CaretTourneyData = CaretTourneyData[, order(names(CaretTourneyData))]

CaretTourneyDataTrain=datasets[[1]][,c(13,17:271)]
select = c("OppTeam", "Team", "Year", "Id","Conf.x", "Seed.x", "Record.x", "SeedNum.x", "Seed.y", "Conf.y", "Record.y",
           "Team_Name","Opp_Team_Name","After1stWkd")
CaretTourneyDataTrain = CaretTourneyDataTrain[, !(names(CaretTourneyDataTrain) %in% select)]
CaretTourneyDataTrain = CaretTourneyDataTrain[, order(names(CaretTourneyDataTrain))]

CaretTourneyDataVal=datasets[[2]][,c(13,17:271)]
select = c("OppTeam", "Team", "Year", "Id","Conf.x", "Seed.x", "Record.x", "SeedNum.x", "Seed.y", "Conf.y", "Record.y",
           "Team_Name","Opp_Team_Name","After1stWkd")
CaretTourneyDataVal = CaretTourneyDataVal[, !(names(CaretTourneyDataVal) %in% select)]
CaretTourneyDataVal = CaretTourneyDataVal[, order(names(CaretTourneyDataVal))]

#CaretTourneyData = CaretTourneyData[,c('SeedNumDiff','AdjDDiff','AdjODiff','toVarl30Diff','draggDiff','MAS_Final_Diff','FTPerctVarl30Diff','ADJEM_SOSDiff')]
#myglm <- glm(TourneyDataTotal$Win ~ as.matrix(CaretTourneyData),data=CaretTourneyData, family= binomial())
#summary(myglm)

final_grid3 <- expand.grid(nrounds = 300, eta = 1, lambda = 0.0003, alpha=0.0003)
final_grid4=expand.grid(nrounds=300, eta=.5, max_depth=3, gamma=0,
                        colsample_bytree=0.95,min_child_weight=1,subsample = 1)
final_grid5=expand.grid(alpha = 1, lambda= 0.005)
glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda = 0.009) ## notice the . before the parameter

glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))

glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))

set.seed(33)#32



models <- caretList(
  x=CaretTourneyDataTrain[,-ncol(CaretTourneyDataTrain)],
  y=as.integer(CaretTourneyDataTrain[,ncol(CaretTourneyDataTrain)]),
  continue_on_fail = TRUE,
  
  trControl=trainControl(method="cv",
                         classProbs = TRUE, 
                         # number=50,
                         index = createFolds(as.integer(CaretTourneyDataTrain[,ncol(CaretTourneyDataTrain)]), 
                                             50),
                         savePredictions=TRUE,
                         allowParallel = TRUE,
                         verboseIter = TRUE
  ),
  # tuneList=list(
  #   #    qrf7=caretEnsemble::caretModelSpec(method="xgbTree", tuneGrid = final_grid4),
  #   #    qrf8=caretEnsemble::caretModelSpec(method="xgbLinear", tuneGrid = final_grid3) #5
  #   #   # #5
  #   #   # f.f=caretModelSpec(method="rf", trace=T, tuneLength=5),
  #   #  # gbm.f=caretModelSpec(method="gbm", tuneLength=5),
  #   glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic), ## Elastic, highly correlated with lasso and ridge regressions
  #   
  #   glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
  #   
  #   glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge) ## Ridge
  #   
  # ),
  
  methodList=c("glmnet",
               "glmboost",  # Boosted Generalized Linear Model
               "nnls",
               "rf","pcaNNet",
               "gaussprPoly" ,
               "gcvEarth",
               "wsrf"
               
               
               #"RRFglobal",
               #"RRF"
               #"qrf"
               # Independent Component Regression
               # Stochastic Gradient Boosting
               #"xgbLinear"
               #"gbm"#
               
  )
) #0.95
models

#NCAA.stacked<-caretStack(models, method="glm");NCAA.stacked
NCAA.ensemble<-caretEnsemble(models,trControl=trainControl(
  number=150,
  method = "boot",
  verboseIter = TRUE
))#0.91

NCAA.ensemble

gc()

pred = predict(object=NCAA.ensemble,  CaretTourneyData[,-ncol(CaretTourneyData)] )
cat("r2 for all ALL is: ",cor(pred, CaretTourneyData[,ncol(CaretTourneyData)])^2)
cat("rmse for all ALL is: ",sqrt(mean((as.integer(CaretTourneyData[,ncol(CaretTourneyData)]) -  pred)^2)), "\n")

pred_v = predict(object=NCAA.ensemble,  CaretTourneyDataVal[,-ncol(CaretTourneyDataVal)] )
cat("r2 for Validate ALL is: ",cor(pred_v, CaretTourneyDataVal[,ncol(CaretTourneyDataVal)])^2)
cat("rmse for Validate ALL is: ",sqrt(mean((as.integer(CaretTourneyDataVal[,ncol(CaretTourneyDataVal)]) -  pred_v)^2)), "\n")

pred_t = predict(object=NCAA.ensemble,  CaretTourneyDataTrain[,-ncol(CaretTourneyDataTrain)] )
cat("r2 for train ALL is: ",cor(pred_t, CaretTourneyDataTrain[,ncol(CaretTourneyDataTrain)])^2)
cat("rmse for train ALL is: ",sqrt(mean((as.integer(CaretTourneyDataTrain[,ncol(CaretTourneyDataTrain)]) -  pred_t)^2)), "\n")

a= data.frame(pred_v, CaretTourneyDataVal[,ncol(CaretTourneyDataTrain)])


gc()

#}
SubmissionDataSet <- subset(TourneySeeds, Season == 2016);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2016)#CurrYear
SubmissionDataSet2 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet <- subset(TourneySeeds, Season == 2017);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2017)#CurrYear
SubmissionDataSet3 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet <- subset(TourneySeeds, Season == 2018);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2018)#CurrYear
SubmissionDataSet4 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet <- subset(TourneySeeds, Season == 2019);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2019)#CurrYear
SubmissionDataSet5 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet <- subset(TourneySeeds, Season == 2021);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2021)#CurrYear
SubmissionDataSet6 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet <- subset(TourneySeeds, Season == 2022);dim(SubmissionDataSet) #CurrYear
Webexpand_NCAA_Tournament <- expand.grid(Team = SubmissionDataSet$TeamID, OppTeam = SubmissionDataSet$TeamID)
SubmissionDataSet <- subset(Webexpand_NCAA_Tournament, Team < OppTeam)
SubmissionDataSet <- within(SubmissionDataSet, Year <- 2022)#CurrYear
SubmissionDataSet7 <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))

SubmissionDataSet = rbind(SubmissionDataSet2,SubmissionDataSet3,SubmissionDataSet4,SubmissionDataSet5,
                          SubmissionDataSet6,SubmissionDataSet7)
dim(SubmissionDataSet)

SubmissionDataSet <- within(SubmissionDataSet, Id <- paste(Year,Team,OppTeam, sep = "_"))
SubmissionDataSet <- merge(SubmissionDataSet, TourneySeeds, by.x = c("Year","Team"), by.y = c("Season","TeamID"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, TourneySeeds, by.x = c("Year","OppTeam"), by.y = c("Season","TeamID"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, KenPomFinal[,2:23], by.x = c("Year","Team"),by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, KenPomFinal[,2:23], by.x = c("Year","OppTeam"),by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, sumRegSeason, by.x = c("Year","Team"), by.y = c("Year","Team"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, sumRegSeason, by.x = c("Year","OppTeam"), by.y = c("Year","Team"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, last30regseas, by.x = c("Year","Team"), by.y = c("Year","Team"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, last30regseas, by.x = c("Year","OppTeam"), by.y = c("Year","Team"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, HSFinalT10, by.x = c("Year","Team"), by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, HSFinalT10, by.x = c("Year","OppTeam"), by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, HSFinalT100, by.x = c("Year","Team"), by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, HSFinalT100, by.x = c("Year","OppTeam"), by.y = c("Year","Team_Id"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, FinalRankings, by.x = c("Year","Team"), by.y = c("Season","TeamID"),all.x = TRUE)
SubmissionDataSet <- merge(SubmissionDataSet, FinalRankings, by.x = c("Year","OppTeam"), by.y = c("Season","TeamID"),all.x = TRUE)

######################################################
#
###################################################

SubmissionDataSet <-within (SubmissionDataSet, AdjDDiff<-as.numeric(AdjD.x)-as.numeric(AdjD.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjD_OppDiff<-as.numeric(AdjD_Opp.x)-as.numeric(AdjD_Opp.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjDRankDiff<-as.numeric(AdjDRank.x)-as.numeric(AdjDRank.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjDRank_OppDiff<-as.numeric(AdjDRank_Opp.x)-as.numeric(AdjDRank_Opp.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjODiff<-as.numeric(AdjO.x)-as.numeric(AdjO.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjO_OppDiff<-as.numeric(AdjO_Opp.x)-as.numeric(AdjO_Opp.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjORankDiff<-as.numeric(AdjORank.x)-as.numeric(AdjORank.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjORank_OppDiff<-as.numeric(AdjORank_Opp.x)-as.numeric(AdjORank_Opp.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjTDiff<-as.numeric(AdjT.x)-as.numeric(AdjT.y))
SubmissionDataSet <-within (SubmissionDataSet, AdjTRankDiff<-as.numeric(AdjTRank.x)-as.numeric(AdjTRank.y))
SubmissionDataSet <-within (SubmissionDataSet, ast2tol30Diff<-as.numeric(ast2tol30.x)-as.numeric(ast2tol30.y))
SubmissionDataSet <-within (SubmissionDataSet, astaggDiff<-as.numeric(astagg.x)-as.numeric(astagg.y))
SubmissionDataSet <-within (SubmissionDataSet, astaggl30Diff<-as.numeric(astaggl30.x)-as.numeric(astaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, blkaggDiff<-as.numeric(blkagg.x)-as.numeric(blkagg.y))
SubmissionDataSet <-within (SubmissionDataSet, blkaggl30Diff<-as.numeric(blkaggl30.x)-as.numeric(blkaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, draggDiff<-as.numeric(dragg.x)-as.numeric(dragg.y))
SubmissionDataSet <-within (SubmissionDataSet, draggl30Diff<-as.numeric(draggl30.x)-as.numeric(draggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, FG3PerctVarDiff<-as.numeric(FG3PerctVar.x)-as.numeric(FG3PerctVar.y))
SubmissionDataSet <-within (SubmissionDataSet, FG3PerctVarl30Diff<-as.numeric(FG3PerctVarl30.x)-as.numeric(FG3PerctVarl30.y))
SubmissionDataSet <-within (SubmissionDataSet, fga3aggDiff<-as.numeric(fga3agg.x)-as.numeric(fga3agg.y))
SubmissionDataSet <-within (SubmissionDataSet, fga3aggl30Diff<-as.numeric(fga3aggl30.x)-as.numeric(fga3aggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, fgaaggDiff<-as.numeric(fgaagg.x)-as.numeric(fgaagg.y))
SubmissionDataSet <-within (SubmissionDataSet, fgaaggl30Diff<-as.numeric(fgaaggl30.x)-as.numeric(fgaaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, fgm3aggDiff<-as.numeric(fgm3agg.x)-as.numeric(fgm3agg.y))
SubmissionDataSet <-within (SubmissionDataSet, fgm3aggl30Diff<-as.numeric(fgm3aggl30.x)-as.numeric(fgm3aggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, fgmaggDiff<-as.numeric(fgmagg.x)-as.numeric(fgmagg.y))
SubmissionDataSet <-within (SubmissionDataSet, fgmaggl30Diff<-as.numeric(fgmaggl30.x)-as.numeric(fgmaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, FGPerctVarDiff<-as.numeric(FGPerctVar.x)-as.numeric(FGPerctVar.y))
SubmissionDataSet <-within (SubmissionDataSet, FGPerctVarl30Diff<-as.numeric(FGPerctVarl30.x)-as.numeric(FGPerctVarl30.y))
SubmissionDataSet <-within (SubmissionDataSet, FreshmanT10Diff<-as.numeric(FreshmanT10.x)-as.numeric(FreshmanT10.y))
SubmissionDataSet <-within (SubmissionDataSet, FreshmanT100Diff<-as.numeric(FreshmanT100.x)-as.numeric(FreshmanT100.y))
SubmissionDataSet <-within (SubmissionDataSet, ftaaggDiff<-as.numeric(ftaagg.x)-as.numeric(ftaagg.y))
SubmissionDataSet <-within (SubmissionDataSet, ftaaggl30Diff<-as.numeric(ftaaggl30.x)-as.numeric(ftaaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, ftmaggDiff<-as.numeric(ftmagg.x)-as.numeric(ftmagg.y))
SubmissionDataSet <-within (SubmissionDataSet, ftmaggl30Diff<-as.numeric(ftmaggl30.x)-as.numeric(ftmaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, FTPerctVarDiff<-as.numeric(FTPerctVar.x)-as.numeric(FTPerctVar.y))
SubmissionDataSet <-within (SubmissionDataSet, FTPerctVarl30Diff<-as.numeric(FTPerctVarl30.x)-as.numeric(FTPerctVarl30.y))
SubmissionDataSet <-within (SubmissionDataSet, JuniorT10Diff<-as.numeric(JuniorT10.x)-as.numeric(JuniorT10.y))
SubmissionDataSet <-within (SubmissionDataSet, JuniorT100Diff<-as.numeric(JuniorT100.x)-as.numeric(JuniorT100.y))
SubmissionDataSet <-within (SubmissionDataSet, LuckDiff<-as.numeric(Luck.x)-as.numeric(Luck.y))
SubmissionDataSet <-within (SubmissionDataSet, LuckRankDiff<-as.numeric(LuckRank.x)-as.numeric(LuckRank.y))
SubmissionDataSet <-within (SubmissionDataSet, NCSOS_PythDiff<-as.numeric(NCSOS_Pyth.x)-as.numeric(NCSOS_Pyth.y))
SubmissionDataSet <-within (SubmissionDataSet, NCSOS_PythRankDiff<-as.numeric(NCSOS_PythRank.x)-as.numeric(NCSOS_PythRank.y))
SubmissionDataSet <-within (SubmissionDataSet, oraggDiff<-as.numeric(oragg.x)-as.numeric(oragg.y))
SubmissionDataSet <-within (SubmissionDataSet, oraggl30Diff<-as.numeric(oraggl30.x)-as.numeric(oraggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, pfaggDiff<-as.numeric(pfagg.x)-as.numeric(pfagg.y))
SubmissionDataSet <-within (SubmissionDataSet, pfaggl30Diff<-as.numeric(pfaggl30.x)-as.numeric(pfaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, PythDiff<-as.numeric(Pyth.x)-as.numeric(Pyth.y))
SubmissionDataSet <-within (SubmissionDataSet, ADJEM_SOSDiff<-as.numeric(ADJEM_SOS.x)-as.numeric(ADJEM_SOS.y))
SubmissionDataSet <-within (SubmissionDataSet, PythRank_OppDiff<-as.numeric(PythRank_Opp.x)-as.numeric(PythRank_Opp.y))
SubmissionDataSet <-within (SubmissionDataSet, RankDiff<-as.numeric(Rank.x)-as.numeric(Rank.y))
SubmissionDataSet <-within (SubmissionDataSet, SeedNumDiff<-as.numeric(SeedNum.x)-as.numeric(SeedNum.y))
SubmissionDataSet <-within (SubmissionDataSet, SeniorT10Diff<-as.numeric(SeniorT10.x)-as.numeric(SeniorT10.y))
SubmissionDataSet <-within (SubmissionDataSet, SeniorT100Diff<-as.numeric(SeniorT100.x)-as.numeric(SeniorT100.y))
SubmissionDataSet <-within (SubmissionDataSet, SophomoreT10Diff<-as.numeric(SophomoreT10.x)-as.numeric(SophomoreT10.y))
SubmissionDataSet <-within (SubmissionDataSet, SophomoreT100Diff<-as.numeric(SophomoreT100.x)-as.numeric(SophomoreT100.y))
SubmissionDataSet <-within (SubmissionDataSet, stlaggDiff<-as.numeric(stlagg.x)-as.numeric(stlagg.y))
SubmissionDataSet <-within (SubmissionDataSet, stlaggl30Diff<-as.numeric(stlaggl30.x)-as.numeric(stlaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, toaggDiff<-as.numeric(toagg.x)-as.numeric(toagg.y))
SubmissionDataSet <-within (SubmissionDataSet, toaggl30Diff<-as.numeric(toaggl30.x)-as.numeric(toaggl30.y))
SubmissionDataSet <-within (SubmissionDataSet, toVarl30Diff<-as.numeric(toVarl30.x)-as.numeric(toVarl30.y))
SubmissionDataSet <- within( SubmissionDataSet, PerctOfrom3pt.x<- (3*fgm3agg.x)/(fgm3agg.x*3+fgmagg.x*2+ftmagg.x*1))
SubmissionDataSet <- within( SubmissionDataSet, PerctOfrom3pt.y<- (3*fgm3agg.y)/(fgm3agg.y*3+fgmagg.y*2+ftmagg.y*1))
SubmissionDataSet <- within( SubmissionDataSet, PerctOfrom3ptDiff<- PerctOfrom3pt.x-PerctOfrom3pt.y)
SubmissionDataSet <- within( SubmissionDataSet, PerctOfromFT.x<- (1*ftmagg.x)/(fgm3agg.x*3+fgmagg.x*2+ftmagg.x*1))
SubmissionDataSet <- within( SubmissionDataSet, PerctOfromFT.y<- (1*ftmagg.y)/(fgm3agg.y*3+fgmagg.y*2+ftmagg.y*1))
SubmissionDataSet <- within( SubmissionDataSet, PerctOfromFTDiff<- PerctOfrom3pt.x-PerctOfrom3pt.y)
SubmissionDataSet<- within( SubmissionDataSet, AggresPrct.x<- (ftmagg.x/fgmagg.x)/(fgm3agg.x/fgmagg.x))
SubmissionDataSet <- within( SubmissionDataSet, AggresPrct.y<- (ftmagg.y/fgmagg.y)/(fgm3agg.y/fgmagg.y))
SubmissionDataSet <- within( SubmissionDataSet, AggresPrctDiff<- AggresPrct.x-AggresPrct.y)
#SubmissionDataSet <- within(SubmissionDataSet, After1stWkd <- ifelse(Daynum>140,1,0))
Power5List <- c("B10","ACC","B12","P10","P12","BE","SEC")
SubmissionDataSet <- within(SubmissionDataSet, Power5.x <- ifelse(Conf.x %in% Power5List,1,0))
SubmissionDataSet <- within(SubmissionDataSet, Power5.y <- ifelse(Conf.y %in% Power5List,1,0))
SubmissionDataSet <-within(SubmissionDataSet, Power5Flag <-as.numeric( Power5.x)-as.numeric(Power5.y))
SubmissionDataSet <-within(SubmissionDataSet, RemoveDups <- ifelse(Team>OppTeam,0,1))
SubmissionDataSet <-within(SubmissionDataSet, OffMomentum.x<-as.numeric( OffRtgl30.x)/as.numeric(OffRtg.x))
SubmissionDataSet <-within(SubmissionDataSet, DefMomentum.x<-as.numeric( DffRtgl30.x)/as.numeric(DffRtg.x))
SubmissionDataSet <-within(SubmissionDataSet, OffMomentum.y<-as.numeric( OffRtgl30.y)/as.numeric(OffRtg.y))
SubmissionDataSet <-within(SubmissionDataSet, DefMomentum.y<-as.numeric( DffRtgl30.y)/as.numeric(DffRtg.y))
SubmissionDataSet <-within(SubmissionDataSet, OffMomentumDiff<-as.numeric( OffMomentum.x)-as.numeric(OffMomentum.y))
SubmissionDataSet <-within(SubmissionDataSet, DefMomentumDiff<-as.numeric( DefMomentum.x)-as.numeric(DefMomentum.y))
SubmissionDataSet <-within(SubmissionDataSet, MAS_Final_Diff<-as.numeric( MAS_FinalRanking.x)-as.numeric(MAS_FinalRanking.y))
SubmissionDataSet <-within(SubmissionDataSet, MAS_Min_Diff<-as.numeric( MAS_MinRank.x)-as.numeric(MAS_MinRank.y))
SubmissionDataSet <-within(SubmissionDataSet, MAS_Max_Diff<-as.numeric( MAS_MaxRank.x)-as.numeric(MAS_MaxRank.y))
SubmissionDataSet <-within(SubmissionDataSet, MAS_Var_Diff<-as.numeric( MAS_VarRank.x)-as.numeric(MAS_VarRank.y))
SubmissionDataSet <-within(SubmissionDataSet, COL_Final_Diff<-as.numeric( COL_FinalRanking.x)-as.numeric(COL_FinalRanking.y))
SubmissionDataSet <-within(SubmissionDataSet, COL_Min_Diff<-as.numeric( COL_MinRank.x)-as.numeric(COL_MinRank.y))
SubmissionDataSet <-within(SubmissionDataSet, COL_Max_Diff<-as.numeric( COL_MaxRank.x)-as.numeric(COL_MaxRank.y))
SubmissionDataSet <-within(SubmissionDataSet, COL_Var_Diff<-as.numeric( COL_VarRank.x)-as.numeric(COL_VarRank.y))
SubmissionDataSet <- within(SubmissionDataSet, RPI_Final_Diff<-as.numeric( RPI_FinalRanking.x)-as.numeric(RPI_FinalRanking.y))
SubmissionDataSet <- within(SubmissionDataSet, RPI_Min_Diff<-as.numeric( RPI_MinRank.x)-as.numeric(RPI_MinRank.y))
SubmissionDataSet <- within(SubmissionDataSet, RPI_Max_Diff<-as.numeric( RPI_MaxRank.x)-as.numeric(RPI_MaxRank.y))
SubmissionDataSet <- within(SubmissionDataSet, RPI_Var_Diff<-as.numeric( RPI_VarRank.x)-as.numeric(RPI_VarRank.y))

SubmissionDataSet <- within(SubmissionDataSet, SeedDiff<- SubmissionDataSet$SeedNum.x-SubmissionDataSet$SeedNum.y)
SubmissionDataSet <- within(SubmissionDataSet, Eff_L30_Diff<- (OffRtgl30.x-DffRtgl30.x) -(OffRtgl30.y-DffRtgl30.y))
SubmissionDataSet <- within(SubmissionDataSet, OEff_Diff<- OffRtgl30.x -OffRtgl30.y)
SubmissionDataSet <- within(SubmissionDataSet, DEff_Diff<- DffRtgl30.x -DffRtgl30.y)

TeamNames <- teamsAll[,c("Team_Id","Team_Name")]
TeamNames <- unique(TeamNames)
colnames(TeamNames)[2] <- "Team_Name"
SubmissionDataSet <- merge(SubmissionDataSet, TeamNames, by.x = "Team",by.y = "Team_Id",all.x = TRUE)
colnames(TeamNames)[2] <- "Opp_Team_Name"
SubmissionDataSet <- merge(SubmissionDataSet, TeamNames, by.x = "OppTeam",by.y = "Team_Id",all.x = TRUE)
colnames(TeamNames)[2] <- "Team_Name"

#names(TourneyDataTotal)
#filenameValidate <- paste(FileLocation,"ValidateData.csv",sep="")
#write.csv(SubmissionDataSet,filenameValidate,row.names=FALSE)


SubmissionDataSet[is.na(SubmissionDataSet)] <- 0
SubmissionDataSet.ID = SubmissionDataSet[,"Id"]
SubmissionDataSet1 = SubmissionDataSet

SubmissionDataSet.cats <- SubmissionDataSet1[,c(4,5,7,10,11,30,31,261,262)]
SubmissionDataSet.nums <- SubmissionDataSet1[,-c(4,5,7,10,11,30,31,261,262)]

SubmissionDataSet.nums[] <- lapply(SubmissionDataSet.nums, as.numeric)
SubmissionDataSet = cbind(SubmissionDataSet.cats, SubmissionDataSet.nums)
select = c("OppTeam", "Team", "Year", "Id","Conf.x", "Seed.x", "Record.x", "SeedNum.x", "Seed.y", "Conf.y", "Record.y",
           "Opp_Team_Name","Team_Name"
)
SubmissionDataSet = SubmissionDataSet[ , !(names(SubmissionDataSet) %in% select)]

ttourneyDataTotal = t(TourneyDataTotal)
ttourneyDataTotal = data.frame(ttourneyDataTotal)
ttourneyDataTotal = setDT(ttourneyDataTotal, keep.rownames = TRUE)[]
ttourneyDataTotal = data.frame(ttourneyDataTotal)
ttourneyDataTotal = ttourneyDataTotal[,1:2]

tSubmissionDataSet = t(SubmissionDataSet)
tSubmissionDataSet = data.frame(tSubmissionDataSet)
tSubmissionDataSet = setDT(tSubmissionDataSet, keep.rownames = TRUE)[]
tSubmissionDataSet = data.frame(tSubmissionDataSet)

traits = left_join(tSubmissionDataSet, ttourneyDataTotal,by = "rn"); dim(traits);dim(SubmissionDataSet);traits=traits[,-ncol(tSubmissionDataSet)+1]
traits=data.frame(t(traits))
traits=janitor::row_to_names(traits, row_number=1)
traits = setDT(traits, keep.rownames = FALSE)[]
#traits=data.frame(traits)
traits[] <- lapply(traits, as.character)
traits[] <- lapply(traits, as.numeric)

traits=data.frame(traits)
traits=traits[ , order(names(traits))]

#traits = traits[,c('SeedNumDiff','AdjDDiff','AdjODiff','toVarl30Diff','draggDiff','MAS_Final_Diff','FTPerctVarl30Diff','ADJEM_SOSDiff')]

#write.csv(traits,'traits.csv')
#write.csv(CaretTourneyData,'CaretTourneyData.csv')
#traits.anti = anti_join(tSubmissionDataSet, ttourneyDataTotal,by = "rn"); dim(traits.anti)
#traits.anti = data.frame(t(traits.anti))

#traits = traits[,c("SeedNumDiff" ,"AdjDDiff","AdjODiff","toVarl30Diff","draggDiff","MAS_Final_Diff","FTPerctVarl30Diff","ADJEM_SOSDiff")]

#names(SubmissionDataSet[,137:190])
#pr.nn <- data.frame(compute(nn,SubmissionDataSet[,c("SeedNumDiff","AdjDDiff","AdjODiff","FreshmanT10Diff","SeniorT10Diff","toVarl30Diff","oraggDiff","draggDiff","astaggDiff")]))
#holdout_Pred_ID = data.frame (Id = SubmissionDataSet$Id, pred = pr.nn$net.result)
#write.table(holdout_Pred_ID, 'NNSubmissionTest02.csv',sep = ",", quote = FALSE, row.names = FALSE)


#GLM
#preProcess_range_model <- preProcess(SubmissionDataSet, method='knnImpute')
dim(traits);dim(SubmissionDataSet)
traits.b1=traits[1:1000,]
holdout_predictions = predict(object=NCAA.ensemble,  traits)
#holdout_predictions.ensemble <- predict(object=NCAA.ensembled,SubmissionDataSet,se=T, level=0.99,type='prob')

#holdout_predictions.stacked <- predict(object=NCAA.stacked,SubmissionDataSet,se=T, level=0.99, type='prob')
#cor(holdout_predictions,holdout_predictions.ensemble[,1])
#cor(holdout_predictions,holdout_predictions.stacked[,1])

holdout_predictions = data.frame(Id = SubmissionDataSet.ID , pred =data.frame(holdout_predictions));colnames(holdout_predictions)[2]="Pred"
dim(holdout_predictions)


if(kaggle){
  holdout_predictions = holdout_predictions %>% filter(grepl(x=holdout_predictions$Id, pattern="2022_*"))
  write.csv(holdout_predictions, 'submission.csv', row.names = FALSE)
  
} else{
  #holdout_predictions = holdout_predictions %>% filter(grepl(x=holdout_predictions$Id, pattern="2022_*"))
  
  TeamNames$Team_Id = as.character(TeamNames$Team_Id)
  holdout_predictions =holdout_predictions %>% mutate_all(as.character())
  holdout_predictions = separate(holdout_predictions, col=Id, into=c("Year","TeamA","TeamB"),
                                 sep="_",remove=FALSE)
  
  holdout_predictions = left_join(holdout_predictions, TeamNames, by = c("TeamA"="Team_Id"))
  holdout_predictions = left_join(holdout_predictions, TeamNames, by = c("TeamB"="Team_Id"))
  
  write.csv(holdout_predictions, paste0(init.dir,'submission.csv'), row.names = FALSE)
  
}

stopCluster(cl)



