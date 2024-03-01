####libraries####
library(stargazer)
library(haven)
library(skimr)
library(zoo)
library(mstate)
library(msm)
library(elect)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plyr)
library(readr)
library(ggpubr)
library(haven)
library(modelr)
library(broom)
library(gtools)
library(tableone)
library(psych)
library(margins)
library(ggplot2)
library(readr)
library(data.table)
library(plm)
library(MASS)
library(effects)
library(splines)
library(car)
library(foreign)
library(mitools)
library(Amelia)
library(lmtest)
library(ggthemes)
library(ggsci)
library(msm.stacked)
library(tidybayes)
####notes####
# to cite the cohort https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6934030/
# for another paper frailty fried scale is possible. 
# i think we want to include:
#- hybrid models to test for between level moderators - 
#- binary outcome for sppb as well as continuous , tho continuous will be main analysis, BUT, i am not sure how to put these models into MI (I have to explore this should we decide to do it) - DONE
# alc is not in dataset
# reviewers may ask to remove people unable to do any of sppb test: LOOK at this https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3156629/ 
##we need to use flags if such case comes up (nhats.org/sites/default/files/2020-06/30_PE_Round%201%20Final.pdf) OR we need to remove from dataset round1 those with sppb NA
# or based on  limitations to walk -- questions in PC section TBD
# cognition, not sure if add this one. if this is the decision, do it from the beginning - no for now 
# check age,I think we need to remove if <2
# I have added some more demographic variables to impute the data - gender, eth, education for now
# need to decide on subgroups: age, sex as sensitivty analysis
# do we include anxiety ? no for now
# ADD MORTALITY? 
# mediation is possible for within and between levels. do we have a theory we want to test? social is responsible for sppb because....? or depression >social isolation https://blogs.bmj.com/bmj/2020/04/09/the-effects-of-isolation-on-the-physical-and-mental-health-of-older-adults/
####load the data####
## interviewed person (SP)
NHATS_Round_1_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_1")
NHATS_Round_2_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_2")
NHATS_Round_3_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_3")
NHATS_Round_4_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_4")
NHATS_Round_5_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_5")
NHATS_Round_6_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_6")
NHATS_Round_7_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_7")
NHATS_Round_8_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_8")
NHATS_Round_9_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_9")
NHATS_Round_10_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_10")
NHATS_Round_10_covid_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_10_COVID")
NHATS_Round_11_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_11")
NHATS_Round_12_SP_File <- read_sas("/Users/danielgallardogomez/Downloads/SP_12")


#remove those who die from round 2 - No existe variable fl10spdied en r10covid
#NHATS_Round_2_SP_File%>%
#  filter(fl2spdied==-1)->NHATS_Round_2_SP_File
#NHATS_Round_3_SP_File%>%
#  filter(fl3spdied==-1)->NHATS_Round_3_SP_File
#NHATS_Round_2_SP_File%>%
#  filter(fl2spdied==-1)->NHATS_Round_2_SP_File
#NHATS_Round_4_SP_File%>%
#  filter(fl4spdied==-1)->NHATS_Round_4_SP_File
#NHATS_Round_5_SP_File%>%
#  filter(fl5spdied==-1)->NHATS_Round_5_SP_File
#NHATS_Round_6_SP_File%>%
#  filter(fl6spdied==-1)->NHATS_Round_6_SP_File
#NHATS_Round_7_SP_File%>%
#  filter(fl7spdied==-1)->NHATS_Round_7_SP_File
#NHATS_Round_8_SP_File%>%
# filter(fl8spdied==-1)->NHATS_Round_8_SP_File
#NHATS_Round_9_SP_File%>%
#  filter(fl9spdied==-1)->NHATS_Round_9_SP_File
#NHATS_Round_10_SP_File%>%
#  filter(fl10spdied==-1)->NHATS_Round_10_SP_File
#NHATS_Round_11_SP_File%>%
#  filter(fl11spdied==-1)->NHATS_Round_11_SP_File
#NHATS_Round_12_SP_File%>%
#  filter(fl12spdied==-1)->NHATS_Round_12_SP_File


## other Person file required too
NHATS_Round_1_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_1")
NHATS_Round_2_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_2")
NHATS_Round_3_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_3")
NHATS_Round_4_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_4")
NHATS_Round_5_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_5")
NHATS_Round_6_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_6")
NHATS_Round_7_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_7")
NHATS_Round_8_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_8")
NHATS_Round_9_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_9")
NHATS_Round_10_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_10")
NHATS_Round_11_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_11")
NHATS_Round_12_OP_File <- read_sas("/Users/danielgallardogomez/Downloads/OP_12")

## other Person sensible information - OPDM

#NHATS_Round_1_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_1")
#NHATS_Round_2_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_2")
#NHATS_Round_3_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_3")
#NHATS_Round_4_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_4")
#NHATS_Round_5_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_5")
#NHATS_Round_6_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_6")
#NHATS_Round_7_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_7")
#NHATS_Round_8_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_8")
#NHATS_Round_9_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_9")
#NHATS_Round_10_OP_extra <- read_sas("/Users/danielgallardogomez/DownloadsOPDM_10")
#NHATS_Round_10_covid_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_10_COVID")
#NHATS_Round_11_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_11")
#NHATS_Round_12_OP_extra <- read_sas("/Users/danielgallardogomez/Downloads/OPDM_12")

#sensible info for each round - SPDM 

NHATS_Round_1_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_1")
NHATS_Round_2_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_2")
NHATS_Round_3_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_3")
NHATS_Round_4_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_4")
NHATS_Round_5_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_5")
NHATS_Round_6_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_6")
NHATS_Round_7_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_7")
NHATS_Round_8_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_8")
NHATS_Round_9_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_9")
NHATS_Round_10_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_10")
NHATS_Round_10_covid_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_10_COVID")
NHATS_Round_11_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_11")
NHATS_Round_12_SP_extra <- read_sas("/Users/danielgallardogomez/Downloads/SPDM_12")


#Accelerometer each round (11) - SPACEL 
NHATS_Round_11_ACEL_File <- read_sas("/Users/danielgallardogomez/Downloads/SPACEL_11")
NHATS_Round_12_ACEL_File <- read_sas("/Users/danielgallardogomez/Downloads/SPACEL_12")

####Unión de bases de datos SP####

nhats1<-left_join(NHATS_Round_1_SP_File,NHATS_Round_1_SP_extra, by = "spid")
nhats2<-left_join(NHATS_Round_2_SP_File,NHATS_Round_2_SP_extra, by = "spid")
nhats3<-left_join(NHATS_Round_3_SP_File,NHATS_Round_3_SP_extra, by = "spid")
nhats4<-left_join(NHATS_Round_4_SP_File,NHATS_Round_4_SP_extra, by = "spid")
nhats5<-left_join(NHATS_Round_5_SP_File,NHATS_Round_5_SP_extra, by = "spid")
nhats6<-left_join(NHATS_Round_6_SP_File,NHATS_Round_6_SP_extra, by = "spid")
nhats7<-left_join(NHATS_Round_7_SP_File,NHATS_Round_7_SP_extra, by = "spid")
nhats8<-left_join(NHATS_Round_8_SP_File,NHATS_Round_8_SP_extra, by = "spid")
nhats9<-left_join(NHATS_Round_9_SP_File,NHATS_Round_9_SP_extra, by = "spid")
nhats10<-left_join(NHATS_Round_10_SP_File,NHATS_Round_10_SP_extra,NHATS_Round_10_covid_SP_extra , by = "spid")
nhats11<-left_join(NHATS_Round_11_SP_File,NHATS_Round_11_SP_extra,NHATS_Round_11_SP_accel, by = "spid")
nhats12<-left_join(NHATS_Round_12_SP_File,NHATS_Round_12_SP_extra,NHATS_Round_12_SP_accel, by = "spid")

####create sppb outcome (0-12) for each wave --IF NEED TO REMOVE ROUND1 NAs in sppb, do it here. REPEAT PROCESS FOR ROUND 5####
#0s means ineligible No existe SPPB en r10


nhats1%>%
  mutate(sppb1 = ifelse(r1dnhatssppb <=-1, NA, r1dnhatssppb))->nhats1
table(nhats1$sppb1)
table(nhats1$r1dnhatssppb)
length(nhats1$sppb1[!is.na(nhats1$sppb1)]) #6578

nhats2%>%
  mutate(sppb2 = ifelse(R2DNHATSSPPB <=-1, NA, R2DNHATSSPPB))->nhats2
table(nhats2$sppb2)
table(nhats2$R2DNHATSSPPB)

nhats3%>%
  mutate(sppb3 = ifelse(r3dnhatssppb <=-1, NA, r3dnhatssppb))->nhats3
table(nhats3$sppb3)
table(nhats3$r3dnhatssppb)

nhats4%>%
  mutate(sppb4 = ifelse(r4dnhatssppb <=-1, NA, r4dnhatssppb))->nhats4
table(nhats4$sppb4)
table(nhats4$r4dnhatssppb)

nhats5%>%
  mutate(sppb5 = ifelse(r5dnhatssppb <=-1, NA, r5dnhatssppb))->nhats5
table(nhats5$sppb5)
table(nhats5$r5dnhatssppb)
length(nhats5$sppb5[!is.na(nhats5$sppb5)]) #6578

nhats6%>%
  mutate(sppb6 = ifelse(r6dnhatssppb <=-1, NA, r6dnhatssppb))->nhats6
table(nhats6$sppb6)
table(nhats6$r6dnhatssppb)

nhats7%>%
  mutate(sppb7 = ifelse(r7dnhatssppb <=-1, NA, r7dnhatssppb))->nhats7
table(nhats7$sppb7)
table(nhats7$r7dnhatssppb)

nhats8%>%
  mutate(sppb8 = ifelse(r8dnhatssppb <=-1, NA, r8dnhatssppb))->nhats8
table(nhats8$sppb8)
table(nhats8$r8dnhatssppb)

nhats9%>%
  mutate(sppb9 = ifelse(r9dnhatssppb <=-1, NA, r9dnhatssppb))->nhats9
table(nhats9$sppb9)
table(nhats9$r9dnhatssppb)

nhats11%>%
  mutate(sppb11 = ifelse(r11dnhatssppb <=-1, NA, r11dnhatssppb))->nhats11
table(nhats11$sppb11)
table(nhats11$r11dnhatssppb)

nhats12%>%
  mutate(sppb12 = ifelse(r12dnhatssppb <=-1, NA, r12dnhatssppb))->nhats12


####create social isolation score (1-6) for each wave####
####1####
###first items family and frieds 
snt1<-NHATS_Round_1_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_1_OP_File$op1relatnshp==36  & NHATS_Round_1_OP_File$op1soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_1_OP_File$op1relatnshp>=2 & NHATS_Round_1_OP_File$op1relatnshp<=29 & NHATS_Round_1_OP_File$op1soclntwrk==1, 0, 
                                ifelse(NHATS_Round_1_OP_File$op1relatnshp==91 & NHATS_Round_1_OP_File$op1soclntwrk==1,0,1)))
table(snt1$family)
#add friends
snt1%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats1<-merge(friends_dat,nhats1,by="spid", all=T)
#add family
snt1%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats1<-merge(family_dat,nhats1,by="spid", all=T)
table(nhats1$family)
table(nhats1$friends)
nhats1$family[is.na(nhats1$family)] <- "1"
nhats1$friends[is.na(nhats1$friends)] <- "1"
table(nhats1$family)
table(nhats1$friends)
table(nhats1$fl1noonetalk)
length(unique(nhats1$spid))


#question of visiting
table(nhats1$pa1vistfrfam)
nhats1%>%
  mutate(visit1fmfr = case_when(pa1vistfrfam ==1~ "0",
                                pa1vistfrfam ==2~ "1",
                                pa1vistfrfam <=-1~ "NA"))->nhats1
#sanity check
table(nhats1$visit1fmfr)


#question of church
table(nhats1$pa1attrelser)
nhats1%>%
  mutate(church1 = case_when(pa1attrelser ==1~ "0",
                             pa1attrelser ==2~ "1",
                             pa1attrelser <=-1~ "NA"))->nhats1
#sanity check
table(nhats1$church1)

#question of club
table(nhats1$pa1clbmtgrac)
nhats1%>%
  mutate(club1 = case_when(pa1clbmtgrac ==1~ "0",
                           pa1clbmtgrac ==2~ "1",
                           pa1clbmtgrac <=-1~ "NA"))->nhats1
#sanity check
table(nhats1$club1)


#married questions
#nhats1$hh1martlstat
table(nhats1$hh1martlstat)
nhats1%>%
  mutate(marital1 = case_when(hh1martlstat ==1 | hh1martlstat ==2 ~ "0",
                              hh1martlstat >2~ "1",
                              hh1martlstat <=-1~ "NA"))->nhats1
#sanity check
table(nhats1$marital1)


#this is just for the time being.
nhats1%>%
  mutate(mar_rec1= as.numeric(marital1),
         frn1= as.numeric(friends),
         fam1=as.numeric(family),
         chr1 = as.numeric(church1),
         clb1= as.numeric(club1),
         vis1= as.numeric(visit1fmfr))->nhats1
nhats1$sociso_index = rowSums(nhats1[,c(1272:1277)])
hist(nhats1$sociso_index)
table(nhats1$sociso_index)
length(nhats1$sociso_index[!is.na(nhats1$sociso_index)]) #7589

####2####
###first items family and frieds 
snt2<-NHATS_Round_2_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_2_OP_File$op2relatnshp==36  & NHATS_Round_2_OP_File$op2soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_2_OP_File$op2relatnshp>=2 & NHATS_Round_2_OP_File$op2relatnshp<=29 & NHATS_Round_2_OP_File$op2soclntwrk==1, 0, 
                                ifelse(NHATS_Round_2_OP_File$op2relatnshp==91 & NHATS_Round_2_OP_File$op2soclntwrk==1,0,1)))
table(snt2$family)
#add friends
snt2%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats2<-merge(friends_dat,nhats2,by="spid", all=T)
#add family
snt2%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats2<-merge(family_dat,nhats2,by="spid", all=T)
table(nhats2$family)
table(nhats2$friends)
nhats2$family[is.na(nhats2$family)] <- "1"
nhats2$friends[is.na(nhats2$friends)] <- "1"
table(nhats2$family)
table(nhats2$friends)
table(nhats2$fl2noonetalk)
length(unique(nhats2$spid))

#question of visiting
table(nhats2$pa2vistfrfam)
nhats2%>%
  mutate(visit2fmfr = case_when(pa2vistfrfam ==1~ "0",
                                pa2vistfrfam ==2~ "1",
                                pa2vistfrfam <=-1~ "NA"))->nhats2

#sanity check
table(nhats2$visit2fmfr)
#question of church
table(nhats2$pa2attrelser)
nhats2%>%
  mutate(church2 = case_when(pa2attrelser ==1~ "0",
                             pa2attrelser ==2~ "1",
                             pa2attrelser <=-1~ "NA"))->nhats2
#sanity check
table(nhats2$church2)
#question of club
table(nhats2$pa2clbmtgrac)
nhats2%>%
  mutate(club2 = case_when(pa2clbmtgrac ==1~ "0",
                           pa2clbmtgrac ==2~ "1",
                           pa2clbmtgrac <=-1~ "NA"))->nhats2
#sanity check
table(nhats2$club2)

#married questions
nhats2$hh2dmarstat
table(nhats2$hh2dmarstat)
nhats2%>%
  mutate(marital2 = case_when(hh2dmarstat ==1 | hh2dmarstat ==2 ~ "0",
                              hh2dmarstat >2~ "1",
                              hh2dmarstat <=-1~ "NA"))->nhats2
#sanity check
table(nhats2$marital2)
#this is just for the time being. - ERROR
nhats2%>%
  mutate(mar_rec2= as.numeric(marital2),
         frn2= as.numeric(friends),
         fam2=as.numeric(family),
         chr2 = as.numeric(church2),
         clb2= as.numeric(club2),
         vis2= as.numeric(visit2fmfr))->nhats2
nhats2$sociso_index = rowSums(nhats2[,c(1195:1200)])
hist(nhats2$sociso_index)
table(nhats2$sociso_index)
length(nhats2$sociso_index[!is.na(nhats2$sociso_index)]) #6035


####3####
###first items family and frieds 
snt3<-NHATS_Round_3_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_3_OP_File$op3relatnshp==36  & NHATS_Round_3_OP_File$op3soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_3_OP_File$op3relatnshp>=2 & NHATS_Round_3_OP_File$op3relatnshp<=29 & NHATS_Round_3_OP_File$op3soclntwrk==1, 0, 
                                ifelse(NHATS_Round_3_OP_File$op3relatnshp==91 & NHATS_Round_3_OP_File$op3soclntwrk==1,0,1)))
table(snt3$family)
#add friends
snt3%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats3<-merge(friends_dat,nhats3,by="spid", all=T)
#add family
snt3%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats3<-merge(family_dat,nhats3,by="spid", all=T)
table(nhats3$family)
table(nhats3$friends)
nhats3$family[is.na(nhats3$family)] <- "1"
nhats3$friends[is.na(nhats3$friends)] <- "1"
table(nhats3$family)
table(nhats3$friends)
table(nhats3$fl3noonetalk)
length(unique(nhats3$spid))

#question of visiting
table(nhats3$pa3vistfrfam)
nhats3%>%
  mutate(visit3fmfr = case_when(pa3vistfrfam ==1~ "0",
                                pa3vistfrfam ==2~ "1",
                                pa3vistfrfam <=-1~ "NA"))->nhats3

#sanity check
table(nhats3$visit3fmfr)
#question of church
table(nhats3$pa3attrelser)
nhats3%>%
  mutate(church3 = case_when(pa3attrelser ==1~ "0",
                             pa3attrelser ==2~ "1",
                             pa3attrelser <=-1~ "NA"))->nhats3
#sanity check
table(nhats3$church3)
#question of club
table(nhats3$pa3clbmtgrac)
nhats3%>%
  mutate(club3 = case_when(pa3clbmtgrac ==1~ "0",
                           pa3clbmtgrac ==2~ "1",
                           pa3clbmtgrac <=-1~ "NA"))->nhats3
#sanity check
table(nhats3$club3)

#married questions
nhats3$hh3dmarstat
table(nhats3$hh3dmarstat)
nhats3%>%
  mutate(marital3 = case_when(hh3dmarstat ==1 | hh3dmarstat ==2 ~ "0",
                              hh3dmarstat >2~ "1",
                              hh3dmarstat <=-1~ "NA"))->nhats3
#sanity check
table(nhats3$marital3)
#this is just for the time being. 
nhats3%>%
  mutate(mar_rec3= as.numeric(marital3),
         frn3= as.numeric(friends),
         fam3=as.numeric(family),
         chr3 = as.numeric(church3),
         clb3= as.numeric(club3),
         vis3= as.numeric(visit3fmfr))->nhats3
nhats3$sociso_index = rowSums(nhats3[,c(1190:1195)])
hist(nhats3$sociso_index)
table(nhats3$sociso_index)
length(nhats3$sociso_index[!is.na(nhats3$sociso_index)]) #4864


####4####
###first items family and frieds 
snt4<-NHATS_Round_4_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_4_OP_File$op4relatnshp==36  & NHATS_Round_4_OP_File$op4soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_4_OP_File$op4relatnshp>=2 & NHATS_Round_4_OP_File$op4relatnshp<=29 & NHATS_Round_4_OP_File$op4soclntwrk==1, 0, 
                                ifelse(NHATS_Round_4_OP_File$op4relatnshp==91 & NHATS_Round_4_OP_File$op4soclntwrk==1,0,1)))
table(snt4$family)
#add friends
snt4%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats4<-merge(friends_dat,nhats4,by="spid", all=T)
#add family
snt4%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats4<-merge(family_dat,nhats4,by="spid", all=T)
table(nhats4$family)
table(nhats4$friends)
nhats4$family[is.na(nhats4$family)] <- "1"
nhats4$friends[is.na(nhats4$friends)] <- "1"
table(nhats4$family)
table(nhats4$friends)
table(nhats4$fl4noonetalk)
length(unique(nhats4$spid))

#question of visiting
table(nhats4$pa4vistfrfam)
nhats4%>%
  mutate(visit4fmfr = case_when(pa4vistfrfam ==1~ "0",
                                pa4vistfrfam ==2~ "1",
                                pa4vistfrfam <=-1~ "NA"))->nhats4

#sanity check
table(nhats4$visit4fmfr)
#question of church
table(nhats4$pa4attrelser)
nhats4%>%
  mutate(church4 = case_when(pa4attrelser ==1~ "0",
                             pa4attrelser ==2~ "1",
                             pa4attrelser <=-1~ "NA"))->nhats4
#sanity check
table(nhats4$church4)
#question of club
table(nhats4$pa4clbmtgrac)
nhats4%>%
  mutate(club4 = case_when(pa4clbmtgrac ==1~ "0",
                           pa4clbmtgrac ==2~ "1",
                           pa4clbmtgrac <=-1~ "NA"))->nhats4
#sanity check
table(nhats4$club4)

#married questions
nhats4$hh4dmarstat
table(nhats4$hh4dmarstat)
nhats4%>%
  mutate(marital4 = case_when(hh4dmarstat ==1 | hh4dmarstat ==2 ~ "0",
                              hh4dmarstat >2~ "1",
                              hh4dmarstat <=-1~ "NA"))->nhats4
#sanity check
table(nhats4$marital4)
#this is just for the time being. 
nhats4%>%
  mutate(mar_rec4= as.numeric(marital4),
         frn4= as.numeric(friends),
         fam4=as.numeric(family),
         chr4 = as.numeric(church4),
         clb4= as.numeric(club4),
         vis4= as.numeric(visit4fmfr))->nhats4
nhats4$sociso_index = rowSums(nhats4[,c(1139:1144)])
hist(nhats4$sociso_index)
table(nhats4$sociso_index)
length(nhats4$sociso_index[!is.na(nhats4$sociso_index)]) #4028

####5####
###first items family and frieds 
snt5<-NHATS_Round_5_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_5_OP_File$op5relatnshp==36  & NHATS_Round_5_OP_File$op5soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_5_OP_File$op5relatnshp>=2 & NHATS_Round_5_OP_File$op5relatnshp<=29 & NHATS_Round_5_OP_File$op5soclntwrk==1, 0, 
                                ifelse(NHATS_Round_5_OP_File$op5relatnshp==91 & NHATS_Round_5_OP_File$op5soclntwrk==1,0,1)))
table(snt5$family)
#add friends
snt5%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats5<-merge(friends_dat,nhats5,by="spid", all=T)
#add family
snt5%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats5<-merge(family_dat,nhats5,by="spid", all=T)
table(nhats5$family)
table(nhats5$friends)
nhats5$family[is.na(nhats5$family)] <- "1"
nhats5$friends[is.na(nhats5$friends)] <- "1"
table(nhats5$family)
table(nhats5$friends)
table(nhats5$fl5noonetalk)
length(unique(nhats5$spid))

#question of visiting
table(nhats5$pa5vistfrfam)
nhats5%>%
  mutate(visit5fmfr = case_when(pa5vistfrfam ==1~ "0",
                                pa5vistfrfam ==2~ "1",
                                pa5vistfrfam <=-1~ "NA"))->nhats5

#sanity check
table(nhats5$visit5fmfr)
#question of church
table(nhats5$pa5attrelser)
nhats5%>%
  mutate(church5 = case_when(pa5attrelser ==1~ "0",
                             pa5attrelser ==2~ "1",
                             pa5attrelser <=-1~ "NA"))->nhats5
#sanity check
table(nhats5$church5)
#question of club
table(nhats5$pa5clbmtgrac)
nhats5%>%
  mutate(club5 = case_when(pa5clbmtgrac ==1~ "0",
                           pa5clbmtgrac ==2~ "1",
                           pa5clbmtgrac <=-1~ "NA"))->nhats5
#sanity check
table(nhats5$club5)

#married questions
nhats5$hh5dmarstat
table(nhats5$hh5dmarstat)
nhats5%>%
  mutate(marital5 = case_when(hh5dmarstat ==1 | hh5dmarstat ==2 ~ "0",
                              hh5dmarstat >2~ "1",
                              hh5dmarstat <=-1~ "NA"))->nhats5
#sanity check
table(nhats5$marital5)
#this is just for the time being. 
nhats5%>%
  mutate(mar_rec5= as.numeric(marital5),
         frn5= as.numeric(friends),
         fam5=as.numeric(family),
         chr5 = as.numeric(church5),
         clb5= as.numeric(club5),
         vis5= as.numeric(visit5fmfr))->nhats5
nhats5$sociso_index = rowSums(nhats5[,c(1564:1569)])
hist(nhats5$sociso_index)
table(nhats5$sociso_index)
length(nhats5$sociso_index[!is.na(nhats5$sociso_index)]) #7557 

####6####
###first items family and frieds 
snt6<-NHATS_Round_6_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_6_OP_File$op6relatnshp==36  & NHATS_Round_6_OP_File$op6soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_6_OP_File$op6relatnshp>=2 & NHATS_Round_6_OP_File$op6relatnshp<=29 & NHATS_Round_6_OP_File$op6soclntwrk==1, 0, 
                                ifelse(NHATS_Round_6_OP_File$op6relatnshp==91 & NHATS_Round_6_OP_File$op6soclntwrk==1,0,1)))
table(snt6$family)
#add friends
snt6%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats6<-merge(friends_dat,nhats6,by="spid", all=T)
#add family
snt6%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats6<-merge(family_dat,nhats6,by="spid", all=T)
table(nhats6$family)
table(nhats6$friends)
nhats6$family[is.na(nhats6$family)] <- "1"
nhats6$friends[is.na(nhats6$friends)] <- "1"
table(nhats6$family)
table(nhats6$friends)
table(nhats6$fl6noonetalk)
length(unique(nhats6$spid))

#question of visiting
table(nhats6$pa6vistfrfam)
nhats6%>%
  mutate(visit6fmfr = case_when(pa6vistfrfam ==1~ "0",
                                pa6vistfrfam ==2~ "1",
                                pa6vistfrfam <=-1~ "NA"))->nhats6

#sanity check
table(nhats6$visit6fmfr)
#question of church
table(nhats6$pa6attrelser)
nhats6%>%
  mutate(church6 = case_when(pa6attrelser ==1~ "0",
                             pa6attrelser ==2~ "1",
                             pa6attrelser <=-1~ "NA"))->nhats6
#sanity check
table(nhats6$church6)
#question of club
table(nhats6$pa6clbmtgrac)
nhats6%>%
  mutate(club6 = case_when(pa6clbmtgrac ==1~ "0",
                           pa6clbmtgrac ==2~ "1",
                           pa6clbmtgrac <=-1~ "NA"))->nhats6
#sanity check
table(nhats6$club6)

#married questions
nhats6$hh6dmarstat
table(nhats6$hh6dmarstat)
nhats6%>%
  mutate(marital6 = case_when(hh6dmarstat ==1 | hh6dmarstat ==2 ~ "0",
                              hh6dmarstat >2~ "1",
                              hh6dmarstat <=-1~ "NA"))->nhats6
#sanity check
table(nhats6$marital6)
#this is just for the time being. 
nhats6%>%
  mutate(mar_rec6= as.numeric(marital6),
         frn6= as.numeric(friends),
         fam6=as.numeric(family),
         chr6 = as.numeric(church6),
         clb6= as.numeric(club6),
         vis6= as.numeric(visit6fmfr))->nhats6
nhats6$sociso_index = rowSums(nhats6[,c(1317:1322)])
hist(nhats6$sociso_index)
table(nhats6$sociso_index)
length(nhats6$sociso_index[!is.na(nhats6$sociso_index)]) #6386 - need to subset with at least 2 observations 

####7####
###first items family and frieds 
snt7<-NHATS_Round_7_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_7_OP_File$op7relatnshp==36  & NHATS_Round_7_OP_File$op7soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_7_OP_File$op7relatnshp>=2 & NHATS_Round_7_OP_File$op7relatnshp<=29 & NHATS_Round_7_OP_File$op7soclntwrk==1, 0, 
                                ifelse(NHATS_Round_7_OP_File$op7relatnshp==91 & NHATS_Round_7_OP_File$op7soclntwrk==1,0,1)))
table(snt7$family)
#add friends
snt7%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats7<-merge(friends_dat,nhats7,by="spid", all=T)
#add family
snt7%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats7<-merge(family_dat,nhats7,by="spid", all=T)
table(nhats7$family)
table(nhats7$friends)
nhats7$family[is.na(nhats7$family)] <- "1"
nhats7$friends[is.na(nhats7$friends)] <- "1"
table(nhats7$family)
table(nhats7$friends)
table(nhats7$fl7noonetalk)
length(unique(nhats7$spid))

#question of visiting
table(nhats7$pa7vistfrfam)
nhats7%>%
  mutate(visit7fmfr = case_when(pa7vistfrfam ==1~ "0",
                                pa7vistfrfam ==2~ "1",
                                pa7vistfrfam <=-1~ "NA"))->nhats7

#sanity check
table(nhats7$visit7fmfr)
#question of church
table(nhats7$pa7attrelser)
nhats7%>%
  mutate(church7 = case_when(pa7attrelser ==1~ "0",
                             pa7attrelser ==2~ "1",
                             pa7attrelser <=-1~ "NA"))->nhats7
#sanity check
table(nhats7$church7)
#question of club
table(nhats7$pa7clbmtgrac)
nhats7%>%
  mutate(club7 = case_when(pa7clbmtgrac ==1~ "0",
                           pa7clbmtgrac ==2~ "1",
                           pa7clbmtgrac <=-1~ "NA"))->nhats7
#sanity check
table(nhats7$club7)

#married questions
nhats7$hh7dmarstat
table(nhats7$hh7dmarstat)
nhats7%>%
  mutate(marital7 = case_when(hh7dmarstat ==1 | hh7dmarstat ==2 ~ "0",
                              hh7dmarstat >2~ "1",
                              hh7dmarstat <=-1~ "NA"))->nhats7
#sanity check
table(nhats7$marital7)
#this is just for the time being. 
nhats7%>%
  mutate(mar_rec7= as.numeric(marital7),
         frn7= as.numeric(friends),
         fam7=as.numeric(family),
         chr7 = as.numeric(church7),
         clb7= as.numeric(club7),
         vis7= as.numeric(visit7fmfr))->nhats7
nhats7$sociso_index = rowSums(nhats7[,c(1314:1319)])
hist(nhats7$sociso_index)
table(nhats7$sociso_index)
length(nhats7$sociso_index[!is.na(nhats7$sociso_index)]) #5553 - need to subset with at least 2 observations 

####8####
###first items family and frieds 
snt8<-NHATS_Round_8_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_8_OP_File$op8relatnshp==36  & NHATS_Round_8_OP_File$op8soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_8_OP_File$op8relatnshp>=2 & NHATS_Round_8_OP_File$op8relatnshp<=29 & NHATS_Round_8_OP_File$op8soclntwrk==1, 0, 
                                ifelse(NHATS_Round_8_OP_File$op8relatnshp==91 & NHATS_Round_8_OP_File$op8soclntwrk==1,0,1)))
table(snt8$family)
#add friends
snt8%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats8<-merge(friends_dat,nhats8,by="spid", all=T)
#add family
snt8%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats8<-merge(family_dat,nhats8,by="spid", all=T)
table(nhats8$family)
table(nhats8$friends)
nhats8$family[is.na(nhats8$family)] <- "1"
nhats8$friends[is.na(nhats8$friends)] <- "1"
table(nhats8$family)
table(nhats8$friends)
table(nhats8$fl8noonetalk)
length(unique(nhats8$spid))

#question of visiting
table(nhats8$pa8vistfrfam)
nhats8%>%
  mutate(visit8fmfr = case_when(pa8vistfrfam ==1~ "0",
                                pa8vistfrfam ==2~ "1",
                                pa8vistfrfam <=-1~ "NA"))->nhats8

#sanity check
table(nhats8$visit8fmfr)
#question of church
table(nhats8$pa8attrelser)
nhats8%>%
  mutate(church8 = case_when(pa8attrelser ==1~ "0",
                             pa8attrelser ==2~ "1",
                             pa8attrelser <=-1~ "NA"))->nhats8
#sanity check
table(nhats8$church8)
#question of club
table(nhats8$pa8clbmtgrac)
nhats8%>%
  mutate(club8 = case_when(pa8clbmtgrac ==1~ "0",
                           pa8clbmtgrac ==2~ "1",
                           pa8clbmtgrac <=-1~ "NA"))->nhats8
#sanity check
table(nhats8$club8)

#married questions
nhats8$hh8dmarstat
table(nhats8$hh8dmarstat)
nhats8%>%
  mutate(marital8 = case_when(hh8dmarstat ==1 | hh8dmarstat ==2 ~ "0",
                              hh8dmarstat >2~ "1",
                              hh8dmarstat <=-1~ "NA"))->nhats8
#sanity check
table(nhats8$marital8)
#this is just for the time being. 
nhats8%>%
  mutate(mar_rec8= as.numeric(marital8),
         frn8= as.numeric(friends),
         fam8=as.numeric(family),
         chr8 = as.numeric(church8),
         clb8= as.numeric(club8),
         vis8= as.numeric(visit8fmfr))->nhats8
nhats8$sociso_index = rowSums(nhats8[,c(1344:1349)])
hist(nhats8$sociso_index)
table(nhats8$sociso_index)
length(nhats8$sociso_index[!is.na(nhats8$sociso_index)]) #4932 - need to subset with at least 2 observations 

####9####
###first items family and frieds 
snt9<-NHATS_Round_9_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_9_OP_File$op9relatnshp==36  & NHATS_Round_9_OP_File$op9soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_9_OP_File$op9relatnshp>=2 & NHATS_Round_9_OP_File$op9relatnshp<=29 & NHATS_Round_9_OP_File$op9soclntwrk==1, 0, 
                                ifelse(NHATS_Round_9_OP_File$op9relatnshp==91 & NHATS_Round_9_OP_File$op9soclntwrk==1,0,1)))
table(snt9$family)
#add friends
snt9%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats9<-merge(friends_dat,nhats9,by="spid", all=T)
#add family
snt9%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats9<-merge(family_dat,nhats9,by="spid", all=T)
table(nhats9$family)
table(nhats9$friends)
nhats9$family[is.na(nhats9$family)] <- "1"
nhats9$friends[is.na(nhats9$friends)] <- "1"
table(nhats9$family)
table(nhats9$friends)
table(nhats9$fl9noonetalk)
length(unique(nhats9$spid))

#question of visiting
table(nhats9$pa9vistfrfam)
nhats9%>%
  mutate(visit9fmfr = case_when(pa9vistfrfam ==1~ "0",
                                pa9vistfrfam ==2~ "1",
                                pa9vistfrfam <=-1~ "NA"))->nhats9

#sanity check
table(nhats9$visit9fmfr)
#question of church
table(nhats9$pa9attrelser)
nhats9%>%
  mutate(church9 = case_when(pa9attrelser ==1~ "0",
                             pa9attrelser ==2~ "1",
                             pa9attrelser <=-1~ "NA"))->nhats9
#sanity check
table(nhats9$church9)
#question of club
table(nhats9$pa9clbmtgrac)
nhats9%>%
  mutate(club9 = case_when(pa9clbmtgrac ==1~ "0",
                           pa9clbmtgrac ==2~ "1",
                           pa9clbmtgrac <=-1~ "NA"))->nhats9
#sanity check
table(nhats9$club9)

#married questions
nhats9$hh9dmarstat
table(nhats9$hh9dmarstat)
nhats9%>%
  mutate(marital9 = case_when(hh9dmarstat ==1 | hh9dmarstat ==2 ~ "0",
                              hh9dmarstat >2~ "1",
                              hh9dmarstat <=-1~ "NA"))->nhats9
#sanity check
table(nhats9$marital9)
#this is just for the time being. 
nhats9%>%
  mutate(mar_rec9= as.numeric(marital9),
         frn9= as.numeric(friends),
         fam9=as.numeric(family),
         chr9 = as.numeric(church9),
         clb9= as.numeric(club9),
         vis9= as.numeric(visit9fmfr))->nhats9
nhats9$sociso_index = rowSums(nhats9[,c(1316:1321)])
hist(nhats9$sociso_index)
table(nhats9$sociso_index)
length(nhats9$sociso_index[!is.na(nhats9$sociso_index)]) #4443 - need to subset with at least 2 observations


####10####
###first items family and frieds 
snt10<-NHATS_Round_10_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_10_OP_File$op10relatnshp==36  & NHATS_Round_10_OP_File$op10soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_10_OP_File$op10relatnshp>=2 & NHATS_Round_10_OP_File$op10relatnshp<=29 & NHATS_Round_10_OP_File$op10soclntwrk==1, 0, 
                                ifelse(NHATS_Round_10_OP_File$op10relatnshp==91 & NHATS_Round_10_OP_File$op10soclntwrk==1,0,1)))
table(snt10$family)
#add friends
snt10%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats10<-merge(friends_dat,nhats10,by="spid", all=T)
#add family
snt10%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats10<-merge(family_dat,nhats10,by="spid", all=T)
table(nhats10$family)
table(nhats10$friends)
nhats10$family[is.na(nhats10$family)] <- "1"
nhats10$friends[is.na(nhats10$friends)] <- "1"
table(nhats10$family)
table(nhats10$friends)
table(nhats10$fl9noonetalk)
length(unique(nhats10$spid))

#question of visiting
table(nhats10$pa10vistfrfam)
nhats10%>%
  mutate(visit10fmfr = case_when(pa10vistfrfam ==1~ "0",
                                 pa10vistfrfam ==2~ "1",
                                 pa10vistfrfam <=-1~ "NA"))->nhats10

#sanity check
table(nhats10$visit10fmfr)
#question of church
table(nhats10$pa10attrelser)
nhats10%>%
  mutate(church10 = case_when(pa10attrelser ==1~ "0",
                              pa10attrelser ==2~ "1",
                              pa10attrelser <=-1~ "NA"))->nhats10
#sanity check
table(nhats10$church10)
#question of club
table(nhats10$pa10clbmtgrac)
nhats10%>%
  mutate(club10 = case_when(pa10clbmtgrac ==1~ "0",
                            pa10clbmtgrac ==2~ "1",
                            pa10clbmtgrac <=-1~ "NA"))->nhats10
#sanity check
table(nhats10$club10)

#married questions
nhats10$hh10dmarstat
table(nhats10$hh10dmarstat)
nhats10%>%
  mutate(marital10 = case_when(hh10dmarstat ==1 | hh10dmarstat ==2 ~ "0",
                               hh10dmarstat >2~ "1",
                               hh10dmarstat <=-1~ "NA"))->nhats10
#sanity check
table(nhats10$marital10)
#this is just for the time being. 
nhats10%>%
  mutate(mar_rec10= as.numeric(marital10),
         frn10= as.numeric(friends),
         fam10=as.numeric(family),
         chr10 = as.numeric(church10),
         clb10= as.numeric(club10),
         vis10= as.numeric(visit10fmfr))->nhats10
nhats10$sociso_index = rowSums(nhats10[,c(1033:1038)])
hist(nhats10$sociso_index)
table(nhats10$sociso_index)
length(nhats10$sociso_index[!is.na(nhats10$sociso_index)]) #4443 - need to subset with at least 2 observations

####11####
###first items family and frieds 
snt11<-NHATS_Round_11_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_11_OP_File$op11relatnshp==36  & NHATS_Round_11_OP_File$op11soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_11_OP_File$op11relatnshp>=2 & NHATS_Round_11_OP_File$op11relatnshp<=29 & NHATS_Round_11_OP_File$op11soclntwrk==1, 0, 
                                ifelse(NHATS_Round_11_OP_File$op11relatnshp==91 & NHATS_Round_11_OP_File$op11soclntwrk==1,0,1)))
table(snt11$family)
#add friends
snt11%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats11<-merge(friends_dat,nhats11,by="spid", all=T)
#add family
snt11%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats11<-merge(family_dat,nhats11,by="spid", all=T)
table(nhats11$family)
table(nhats11$friends)
nhats11$family[is.na(nhats11$family)] <- "1"
nhats11$friends[is.na(nhats11$friends)] <- "1"
table(nhats11$family)
table(nhats11$friends)
table(nhats11$fl9noonetalk)
length(unique(nhats11$spid))

#question of visiting
table(nhats11$pa11vistfrfam)
nhats11%>%
  mutate(visit11fmfr = case_when(pa11vistfrfam ==1~ "0",
                                 pa11vistfrfam ==2~ "1",
                                 pa11vistfrfam <=-1~ "NA"))->nhats11

#sanity check
table(nhats11$visit11fmfr)
#question of church
table(nhats11$pa11attrelser)
nhats11%>%
  mutate(church11 = case_when(pa11attrelser ==1~ "0",
                              pa11attrelser ==2~ "1",
                              pa11attrelser <=-1~ "NA"))->nhats11
#sanity check
table(nhats11$church11)
#question of club
table(nhats11$pa11clbmtgrac)
nhats11%>%
  mutate(club11 = case_when(pa11clbmtgrac ==1~ "0",
                            pa11clbmtgrac ==2~ "1",
                            pa11clbmtgrac <=-1~ "NA"))->nhats11
#sanity check
table(nhats11$club11)

#married questions
nhats11$hh11dmarstat
table(nhats11$hh11dmarstat)
nhats11%>%
  mutate(marital11 = case_when(hh11dmarstat ==1 | hh11dmarstat ==2 ~ "0",
                               hh11dmarstat >2~ "1",
                               hh11dmarstat <=-1~ "NA"))->nhats11
#sanity check
table(nhats11$marital11)
#this is just for the time being. 
nhats11%>%
  mutate(mar_rec11= as.numeric(marital11),
         frn11= as.numeric(friends),
         fam11=as.numeric(family),
         chr11 = as.numeric(church11),
         clb11= as.numeric(club11),
         vis11= as.numeric(visit11fmfr))->nhats11
nhats11$sociso_index = rowSums(nhats11[,c(1335:1340)])
hist(nhats11$sociso_index)
table(nhats11$sociso_index)
length(nhats11$sociso_index[!is.na(nhats11$sociso_index)]) #4443 - need to subset with at least 2 observations

####12####
###first items family and frieds 
snt12<-NHATS_Round_12_OP_File%>%
  dplyr::mutate(friends= ifelse(NHATS_Round_12_OP_File$op12relatnshp==36  & NHATS_Round_12_OP_File$op12soclntwrk==1,0,1),
                family = ifelse(NHATS_Round_12_OP_File$op12relatnshp>=2 & NHATS_Round_12_OP_File$op12relatnshp<=29 & NHATS_Round_12_OP_File$op12soclntwrk==1, 0, 
                                ifelse(NHATS_Round_12_OP_File$op12relatnshp==91 & NHATS_Round_12_OP_File$op12soclntwrk==1,0,1)))
table(snt11$family)
#add friends
snt12%>%
  filter(friends==0)->friends_dat
friends_dat<-friends_dat%>%
  dplyr::select(spid, friends)
length(unique(friends_dat$spid))
friends_dat[!duplicated(friends_dat$spid), ]->friends_dat
length(unique(friends_dat$spid))
nhats12<-merge(friends_dat,nhats12,by="spid", all=T)
#add family
snt12%>%
  filter(family==0)->family_dat
family_dat<-family_dat%>%
  dplyr::select(spid, family)
length(unique(family_dat$spid))
family_dat[!duplicated(family_dat$spid), ]->family_dat
length(unique(family_dat$spid))
nhats12<-merge(family_dat,nhats12,by="spid", all=T)
table(nhats12$family)
table(nhats12$friends)
nhats12$family[is.na(nhats12$family)] <- "1"
nhats12$friends[is.na(nhats12$friends)] <- "1"
table(nhats12$family)
table(nhats12$friends)
table(nhats12$fl9noonetalk)
length(unique(nhats12$spid))

#question of visiting
table(nhats12$pa12vistfrfam)
nhats12%>%
  mutate(visit12fmfr = case_when(pa12vistfrfam ==1~ "0",
                                 pa12vistfrfam ==2~ "1",
                                 pa12vistfrfam <=-1~ "NA"))->nhats12

#sanity check
table(nhats12$visit12fmfr)
#question of church
table(nhats12$pa12attrelser)
nhats12%>%
  mutate(church12 = case_when(pa12attrelser ==1~ "0",
                              pa12attrelser ==2~ "1",
                              pa12attrelser <=-1~ "NA"))->nhats12
#sanity check
table(nhats12$church12)
#question of club
table(nhats12$pa12clbmtgrac)
nhats12%>%
  mutate(club12 = case_when(pa12clbmtgrac ==1~ "0",
                            pa12clbmtgrac ==2~ "1",
                            pa12clbmtgrac <=-1~ "NA"))->nhats12
#sanity check
table(nhats12$club12)

#married questions
nhats12$hh12dmarstat
table(nhats12$hh12dmarstat)
nhats12%>%
  mutate(marital12 = case_when(hh12dmarstat ==1 | hh12dmarstat ==2 ~ "0",
                               hh12dmarstat >2~ "1",
                               hh12dmarstat <=-1~ "NA"))->nhats12
#sanity check
table(nhats12$marital12)
#this is just for the time being. 
nhats12%>%
  mutate(mar_rec12= as.numeric(marital12),
         frn12= as.numeric(friends),
         fam12=as.numeric(family),
         chr12 = as.numeric(church12),
         clb12= as.numeric(club12),
         vis12= as.numeric(visit12fmfr))->nhats12
nhats12$sociso_index = rowSums(nhats12[,c(1634:1639)])
hist(nhats12$sociso_index)
table(nhats12$sociso_index)
length(nhats12$sociso_index[!is.na(nhats12$sociso_index)]) #4443 - need to subset with at least 2 observations

# subset sppb and social isolation index for each wave - just that for now, so it facilitates data reading and cheking 
####add covariates from here####
####WC####
#no wc r10
nhats1$wc1<-ifelse(nhats1$wc1wstmsrinc<=-1, NA, nhats1$wc1wstmsrinc) 
nhats2$wc2<-ifelse(nhats2$wc2wstmsrinc<=-1, NA, nhats2$wc2wstmsrinc) 
nhats3$wc3<-ifelse(nhats3$wc3wstmsrinc<=-1, NA, nhats3$wc3wstmsrinc) 
nhats4$wc4<-ifelse(nhats4$wc4wstmsrinc<=-1, NA, nhats4$wc4wstmsrinc) 
nhats5$wc5<-ifelse(nhats5$wc5wstmsrinc<=-1, NA, nhats5$wc5wstmsrinc) 
nhats6$wc6<-ifelse(nhats6$wc6wstmsrinc<=-1, NA, nhats6$wc6wstmsrinc) 
nhats7$wc7<-ifelse(nhats7$wc7wstmsrinc<=-1, NA, nhats7$wc7wstmsrinc) 
nhats8$wc8<-ifelse(nhats8$wc8wstmsrinc<=-1, NA, nhats8$wc8wstmsrinc) 
nhats9$wc9<-ifelse(nhats9$wc9wstmsrinc<=-1, NA, nhats9$wc9wstmsrinc) 
nhats11$wc11 <- ifelse(nhats11$wc11wstmsrinc<=-1, NA, nhats11$wc11wstmsrinc)
nhats12$wc12 <- ifelse(nhats12$wc12wstmsrinc<=-1, NA, nhats12$wc12wstmsrinc)




####Depression and anxiety####
####1#####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats1$hc1depresan1)
nhats1%>%
  mutate(dep1_1 = as.numeric(case_when(hc1depresan1 ==1  ~ "0",
                                       hc1depresan1 ==2 ~ "1",
                                       hc1depresan1 ==3~ "2",
                                       hc1depresan1 ==4~ "3",
                                       hc1depresan1 <=-1~ "NA")))->nhats1
table(nhats1$dep1_1)
table(nhats1$hc1depresan2)
nhats1%>%
  mutate(dep1_2 = as.numeric(case_when(hc1depresan2 ==1  ~ "0",
                                       hc1depresan2 ==2 ~ "1",
                                       hc1depresan2 ==3~ "2",
                                       hc1depresan2 ==4~ "3",
                                       hc1depresan2 <=-1~ "NA")))->nhats1
table(nhats1$dep1_2)
#score: 1+2 
nhats1$dep1_index = rowSums(nhats1[,c(1280:1281)])
hist(nhats1$dep1_index)
table(nhats1$dep1_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats1$dep1_index_rec<-as.factor(ifelse(nhats1$dep1_index>=3,1,0))
table(nhats1$dep1_index_rec)
#anxiety
table(nhats1$hc1depresan3)
nhats1%>%
  mutate(ax1_1 = as.numeric(case_when(hc1depresan3 ==1  ~ "0",
                                      hc1depresan3 ==2 ~ "1",
                                      hc1depresan3 ==3~ "2",
                                      hc1depresan3 ==4~ "3",
                                      hc1depresan3 <=-1~ "NA")))->nhats1
table(nhats1$ax1_1)
table(nhats1$hc1depresan4)
nhats1%>%
  mutate(ax1_2 = as.numeric(case_when(hc1depresan4 ==1  ~ "0",
                                      hc1depresan4 ==2 ~ "1",
                                      hc1depresan4 ==3~ "2",
                                      hc1depresan4 ==4~ "3",
                                      hc1depresan4 <=-1~ "NA")))->nhats1
table(nhats1$ax1_2)
#score: 1+2 
nhats1$ax1_index = rowSums(nhats1[,c(1284:1285)])
hist(nhats1$ax1_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats1$ax1_index_rec<-as.factor(ifelse(nhats1$ax1_index>=3,1,0))
table(nhats1$ax1_index_rec)

####2#####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats2$hc2depresan1)
nhats2%>%
  mutate(dep2_1 = as.numeric(case_when(hc2depresan1 ==1  ~ "0",
                                       hc2depresan1 ==2 ~ "1",
                                       hc2depresan1 ==3~ "2",
                                       hc2depresan1 ==4~ "3",
                                       hc2depresan1 <=-1~ "NA")))->nhats2
table(nhats2$dep2_1)
table(nhats2$hc2depresan2)
nhats2%>%
  mutate(dep2_2 = as.numeric(case_when(hc2depresan2 ==1  ~ "0",
                                       hc2depresan2 ==2 ~ "1",
                                       hc2depresan2 ==3~ "2",
                                       hc2depresan2 ==4~ "3",
                                       hc2depresan2 <=-1~ "NA")))->nhats2
table(nhats2$dep2_2)
#score: 1+2 
nhats2$dep2_index = rowSums(nhats2[,c(1203:1204)])
hist(nhats2$dep2_index)
table(nhats2$dep2_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats2$dep2_index_rec<-as.factor(ifelse(nhats2$dep2_index>=3,1,0))
table(nhats2$dep2_index_rec)
#anxiety
table(nhats2$hc2depresan3)
nhats2%>%
  mutate(ax2_1 = as.numeric(case_when(hc2depresan3 ==1  ~ "0",
                                      hc2depresan3 ==2 ~ "1",
                                      hc2depresan3 ==3~ "2",
                                      hc2depresan3 ==4~ "3",
                                      hc2depresan3 <=-1~ "NA")))->nhats2
table(nhats2$ax2_1)
table(nhats2$hc2depresan4)
nhats2%>%
  mutate(ax2_2 = as.numeric(case_when(hc2depresan4 ==1  ~ "0",
                                      hc2depresan4 ==2 ~ "1",
                                      hc2depresan4 ==3~ "2",
                                      hc2depresan4 ==4~ "3",
                                      hc2depresan4 <=-1~ "NA")))->nhats2
table(nhats2$ax2_2)
#score: 1+2 
nhats2$ax2_index = rowSums(nhats2[,c(1207:1208)])
hist(nhats2$ax2_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats2$ax2_index_rec<-as.factor(ifelse(nhats2$ax2_index>=3,1,0))
table(nhats2$ax2_index_rec)

####3####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats3$hc3depresan1)
nhats3%>%
  mutate(dep3_1 = as.numeric(case_when(hc3depresan1 ==1  ~ "0",
                                       hc3depresan1 ==2 ~ "1",
                                       hc3depresan1 ==3~ "2",
                                       hc3depresan1 ==4~ "3",
                                       hc3depresan1 <=-1~ "NA")))->nhats3
table(nhats3$dep3_1)
table(nhats3$hc3depresan2)
nhats3%>%
  mutate(dep3_2 = as.numeric(case_when(hc3depresan2 ==1  ~ "0",
                                       hc3depresan2 ==2 ~ "1",
                                       hc3depresan2 ==3~ "2",
                                       hc3depresan2 ==4~ "3",
                                       hc3depresan2 <=-1~ "NA")))->nhats3
table(nhats3$dep3_2)
#score: 1+2 
nhats3$dep3_index = rowSums(nhats3[,c(1198:1199)])
hist(nhats3$dep3_index)
table(nhats3$dep3_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats3$dep3_index_rec<-as.factor(ifelse(nhats3$dep3_index>=3,1,0))
table(nhats3$dep3_index_rec)
#anxiety
table(nhats3$hc3depresan3)
nhats3%>%
  mutate(ax3_1 = as.numeric(case_when(hc3depresan3 ==1  ~ "0",
                                      hc3depresan3 ==2 ~ "1",
                                      hc3depresan3 ==3~ "2",
                                      hc3depresan3 ==4~ "3",
                                      hc3depresan3 <=-1~ "NA")))->nhats3
table(nhats3$ax3_1)
table(nhats3$hc3depresan4)
nhats3%>%
  mutate(ax3_2 = as.numeric(case_when(hc3depresan4 ==1  ~ "0",
                                      hc3depresan4 ==2 ~ "1",
                                      hc3depresan4 ==3~ "2",
                                      hc3depresan4 ==4~ "3",
                                      hc3depresan4 <=-1~ "NA")))->nhats3
table(nhats3$ax3_2)
#score: 1+2 
nhats3$ax3_index = rowSums(nhats3[,c(1202:1203)])
hist(nhats3$ax3_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats3$ax3_index_rec<-as.factor(ifelse(nhats3$ax3_index>=3,1,0))
table(nhats3$ax3_index_rec)


####4####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats4$hc4depresan1)
nhats4%>%
  mutate(dep4_1 = as.numeric(case_when(hc4depresan1 ==1  ~ "0",
                                       hc4depresan1 ==2 ~ "1",
                                       hc4depresan1 ==3~ "2",
                                       hc4depresan1 ==4~ "3",
                                       hc4depresan1 <=-1~ "NA")))->nhats4
table(nhats4$dep4_1)
table(nhats4$hc4depresan2)
nhats4%>%
  mutate(dep4_2 = as.numeric(case_when(hc4depresan2 ==1  ~ "0",
                                       hc4depresan2 ==2 ~ "1",
                                       hc4depresan2 ==3~ "2",
                                       hc4depresan2 ==4~ "3",
                                       hc4depresan2 <=-1~ "NA")))->nhats4
table(nhats4$dep4_2)
#score: 1+2 
nhats4$dep4_index = rowSums(nhats4[,c(1147:1148)])
hist(nhats4$dep4_index)
table(nhats4$dep4_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats4$dep4_index_rec<-as.factor(ifelse(nhats4$dep4_index>=3,1,0))
table(nhats4$dep4_index_rec)
#anxiety
table(nhats4$hc4depresan3)
nhats4%>%
  mutate(ax4_1 = as.numeric(case_when(hc4depresan3 ==1  ~ "0",
                                      hc4depresan3 ==2 ~ "1",
                                      hc4depresan3 ==3~ "2",
                                      hc4depresan3 ==4~ "3",
                                      hc4depresan3 <=-1~ "NA")))->nhats4
table(nhats4$ax4_1)
table(nhats4$hc4depresan4)
nhats4%>%
  mutate(ax4_2 = as.numeric(case_when(hc4depresan4 ==1  ~ "0",
                                      hc4depresan4 ==2 ~ "1",
                                      hc4depresan4 ==3~ "2",
                                      hc4depresan4 ==4~ "3",
                                      hc4depresan4 <=-1~ "NA")))->nhats4
table(nhats4$ax4_2)
#score: 1+2 
nhats4$ax4_index = rowSums(nhats4[,c(1151:1152)])
hist(nhats4$ax4_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats4$ax4_index_rec<-as.factor(ifelse(nhats4$ax4_index>=3,1,0))
table(nhats4$ax4_index_rec)

####5####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats5$hc5depresan1)
nhats5%>%
  mutate(dep5_1 = as.numeric(case_when(hc5depresan1 ==1  ~ "0",
                                       hc5depresan1 ==2 ~ "1",
                                       hc5depresan1 ==3~ "2",
                                       hc5depresan1 ==4~ "3",
                                       hc5depresan1 <=-1~ "NA")))->nhats5
table(nhats5$dep5_1)
table(nhats5$hc5depresan2)
nhats5%>%
  mutate(dep5_2 = as.numeric(case_when(hc5depresan2 ==1  ~ "0",
                                       hc5depresan2 ==2 ~ "1",
                                       hc5depresan2 ==3~ "2",
                                       hc5depresan2 ==4~ "3",
                                       hc5depresan2 <=-1~ "NA")))->nhats5
table(nhats5$dep5_2)
#score: 1+2 
nhats5$dep5_index = rowSums(nhats5[,c(1572:1573)])
hist(nhats5$dep5_index)
table(nhats5$dep5_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats5$dep5_index_rec<-as.factor(ifelse(nhats5$dep5_index>=3,1,0))
table(nhats5$dep5_index_rec)
#anxiety
table(nhats5$hc5depresan3)
nhats5%>%
  mutate(ax5_1 = as.numeric(case_when(hc5depresan3 ==1  ~ "0",
                                      hc5depresan3 ==2 ~ "1",
                                      hc5depresan3 ==3~ "2",
                                      hc5depresan3 ==4~ "3",
                                      hc5depresan3 <=-1~ "NA")))->nhats5
table(nhats5$ax5_1)
table(nhats5$hc5depresan5)
nhats5%>%
  mutate(ax5_2 = as.numeric(case_when(hc5depresan4 ==1  ~ "0",
                                      hc5depresan4 ==2 ~ "1",
                                      hc5depresan4 ==3~ "2",
                                      hc5depresan4 ==4~ "3",
                                      hc5depresan4 <=-1~ "NA")))->nhats5
table(nhats5$ax5_2)
#score: 1+2 
nhats5$ax5_index = rowSums(nhats5[,c(1576:1577)])
hist(nhats5$ax5_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats5$ax5_index_rec<-as.factor(ifelse(nhats5$ax5_index>=3,1,0))
table(nhats5$ax5_index_rec)


####6####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats6$hc6depresan1)
nhats6%>%
  mutate(dep6_1 = as.numeric(case_when(hc6depresan1 ==1  ~ "0",
                                       hc6depresan1 ==2 ~ "1",
                                       hc6depresan1 ==3~ "2",
                                       hc6depresan1 ==4~ "3",
                                       hc6depresan1 <=-1~ "NA")))->nhats6
table(nhats6$dep6_1)
table(nhats6$hc6depresan2)
nhats6%>%
  mutate(dep6_2 = as.numeric(case_when(hc6depresan2 ==1  ~ "0",
                                       hc6depresan2 ==2 ~ "1",
                                       hc6depresan2 ==3~ "2",
                                       hc6depresan2 ==4~ "3",
                                       hc6depresan2 <=-1~ "NA")))->nhats6
table(nhats6$dep6_2)
#score: 1+2 
nhats6$dep6_index = rowSums(nhats6[,c(1325:1326)])
hist(nhats6$dep6_index)
table(nhats6$dep6_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats6$dep6_index_rec<-as.factor(ifelse(nhats6$dep6_index>=3,1,0))
table(nhats6$dep6_index_rec)
#anxiety
table(nhats6$hc6depresan3)
nhats6%>%
  mutate(ax6_1 = as.numeric(case_when(hc6depresan3 ==1  ~ "0",
                                      hc6depresan3 ==2 ~ "1",
                                      hc6depresan3 ==3~ "2",
                                      hc6depresan3 ==4~ "3",
                                      hc6depresan3 <=-1~ "NA")))->nhats6
table(nhats6$ax6_1)
table(nhats6$hc6depresan6)
nhats6%>%
  mutate(ax6_2 = as.numeric(case_when(hc6depresan4 ==1  ~ "0",
                                      hc6depresan4 ==2 ~ "1",
                                      hc6depresan4 ==3~ "2",
                                      hc6depresan4 ==4~ "3",
                                      hc6depresan4 <=-1~ "NA")))->nhats6
table(nhats6$ax6_2)
#score: 1+2 
nhats6$ax6_index = rowSums(nhats6[,c(1329:1330)])
hist(nhats6$ax6_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats6$ax6_index_rec<-as.factor(ifelse(nhats6$ax6_index>=3,1,0))
table(nhats6$ax6_index_rec)


####7####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5773373/ ----example for #1
#depression
table(nhats7$hc7depresan1)
nhats7%>%
  mutate(dep7_1 = as.numeric(case_when(hc7depresan1 ==1  ~ "0",
                                       hc7depresan1 ==2 ~ "1",
                                       hc7depresan1 ==3~ "2",
                                       hc7depresan1 ==4~ "3",
                                       hc7depresan1 <=-1~ "NA")))->nhats7
table(nhats7$dep7_1)
table(nhats7$hc7depresan2)
nhats7%>%
  mutate(dep7_2 = as.numeric(case_when(hc7depresan2 ==1  ~ "0",
                                       hc7depresan2 ==2 ~ "1",
                                       hc7depresan2 ==3~ "2",
                                       hc7depresan2 ==4~ "3",
                                       hc7depresan2 <=-1~ "NA")))->nhats7
table(nhats7$dep7_2)
#score: 1+2 
nhats7$dep7_index = rowSums(nhats7[,c(1322:1323)])
hist(nhats7$dep7_index)
table(nhats7$dep7_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats7$dep7_index_rec<-as.factor(ifelse(nhats7$dep7_index>=3,1,0))
table(nhats7$dep7_index_rec)
#anxiety
table(nhats7$hc7depresan3)
nhats7%>%
  mutate(ax7_1 = as.numeric(case_when(hc7depresan3 ==1  ~ "0",
                                      hc7depresan3 ==2 ~ "1",
                                      hc7depresan3 ==3~ "2",
                                      hc7depresan3 ==4~ "3",
                                      hc7depresan3 <=-1~ "NA")))->nhats7
table(nhats7$ax7_1)
table(nhats7$hc7depresan7)
nhats7%>%
  mutate(ax7_2 = as.numeric(case_when(hc7depresan4 ==1  ~ "0",
                                      hc7depresan4 ==2 ~ "1",
                                      hc7depresan4 ==3~ "2",
                                      hc7depresan4 ==4~ "3",
                                      hc7depresan4 <=-1~ "NA")))->nhats7
table(nhats7$ax7_2)
#score: 1+2 
nhats7$ax7_index = rowSums(nhats7[,c(1326:1327)])
hist(nhats7$ax7_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats7$ax7_index_rec<-as.factor(ifelse(nhats7$ax7_index>=3,1,0))
table(nhats7$ax7_index_rec)

####8####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5883383/ ----example for #1
#depression
table(nhats8$hc8depresan1)
nhats8%>%
  mutate(dep8_1 = as.numeric(case_when(hc8depresan1 ==1  ~ "0",
                                       hc8depresan1 ==2 ~ "1",
                                       hc8depresan1 ==3~ "2",
                                       hc8depresan1 ==4~ "3",
                                       hc8depresan1 <=-1~ "NA")))->nhats8
table(nhats8$dep8_1)
table(nhats8$hc8depresan2)
nhats8%>%
  mutate(dep8_2 = as.numeric(case_when(hc8depresan2 ==1  ~ "0",
                                       hc8depresan2 ==2 ~ "1",
                                       hc8depresan2 ==3~ "2",
                                       hc8depresan2 ==4~ "3",
                                       hc8depresan2 <=-1~ "NA")))->nhats8
table(nhats8$dep8_2)
#score: 1+2 
nhats8$dep8_index = rowSums(nhats8[,c(1352:1353)])
hist(nhats8$dep8_index)
table(nhats8$dep8_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats8$dep8_index_rec<-as.factor(ifelse(nhats8$dep8_index>=3,1,0))
table(nhats8$dep8_index_rec)
#anxiety
table(nhats8$hc8depresan3)
nhats8%>%
  mutate(ax8_1 = as.numeric(case_when(hc8depresan3 ==1  ~ "0",
                                      hc8depresan3 ==2 ~ "1",
                                      hc8depresan3 ==3~ "2",
                                      hc8depresan3 ==4~ "3",
                                      hc8depresan3 <=-1~ "NA")))->nhats8
table(nhats8$ax8_1)
table(nhats8$hc8depresan8)
nhats8%>%
  mutate(ax8_2 = as.numeric(case_when(hc8depresan4 ==1  ~ "0",
                                      hc8depresan4 ==2 ~ "1",
                                      hc8depresan4 ==3~ "2",
                                      hc8depresan4 ==4~ "3",
                                      hc8depresan4 <=-1~ "NA")))->nhats8
table(nhats8$ax8_2)
#score: 1+2 
nhats8$ax8_index = rowSums(nhats8[,c(1356:1357)])
hist(nhats8$ax8_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats8$ax8_index_rec<-as.factor(ifelse(nhats8$ax8_index>=3,1,0))
table(nhats8$ax8_index_rec)

####9####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5993393/ ----example for #1
#depression
table(nhats9$hc9depresan1)
nhats9%>%
  mutate(dep9_1 = as.numeric(case_when(hc9depresan1 ==1  ~ "0",
                                       hc9depresan1 ==2 ~ "1",
                                       hc9depresan1 ==3~ "2",
                                       hc9depresan1 ==4~ "3",
                                       hc9depresan1 <=-1~ "NA")))->nhats9
table(nhats9$dep9_1)
table(nhats9$hc9depresan2)
nhats9%>%
  mutate(dep9_2 = as.numeric(case_when(hc9depresan2 ==1  ~ "0",
                                       hc9depresan2 ==2 ~ "1",
                                       hc9depresan2 ==3~ "2",
                                       hc9depresan2 ==4~ "3",
                                       hc9depresan2 <=-1~ "NA")))->nhats9
table(nhats9$dep9_2)
#score: 1+2 
nhats9$dep9_index = rowSums(nhats9[,c(1324:1325)])
hist(nhats9$dep9_index)
table(nhats9$dep9_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats9$dep9_index_rec<-as.factor(ifelse(nhats9$dep9_index>=3,1,0))
table(nhats9$dep9_index_rec)
#anxiety
table(nhats9$hc9depresan3)
nhats9%>%
  mutate(ax9_1 = as.numeric(case_when(hc9depresan3 ==1  ~ "0",
                                      hc9depresan3 ==2 ~ "1",
                                      hc9depresan3 ==3~ "2",
                                      hc9depresan3 ==4~ "3",
                                      hc9depresan3 <=-1~ "NA")))->nhats9
table(nhats9$ax9_1)
table(nhats9$hc9depresan9)
nhats9%>%
  mutate(ax9_2 = as.numeric(case_when(hc9depresan4 ==1  ~ "0",
                                      hc9depresan4 ==2 ~ "1",
                                      hc9depresan4 ==3~ "2",
                                      hc9depresan4 ==4~ "3",
                                      hc9depresan4 <=-1~ "NA")))->nhats9
table(nhats9$ax9_2)
#score: 1+2 
nhats9$ax9_index = rowSums(nhats9[,c(1328:1329)])
hist(nhats9$ax9_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats9$ax9_index_rec<-as.factor(ifelse(nhats9$ax9_index>=3,1,0))
table(nhats9$ax9_index_rec)


####10####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5993393/ ----example for #1
#depression
table(nhats10$hc10depresan1)
nhats10%>%
  mutate(dep10_1 = as.numeric(case_when(hc10depresan1 ==1  ~ "0",
                                        hc10depresan1 ==2 ~ "1",
                                        hc10depresan1 ==3~ "2",
                                        hc10depresan1 ==4~ "3",
                                        hc10depresan1 <=-1~ "NA")))->nhats10
table(nhats10$dep10_1)
table(nhats10$hc10depresan2)
nhats10%>%
  mutate(dep10_2 = as.numeric(case_when(hc10depresan2 ==1  ~ "0",
                                        hc10depresan2 ==2 ~ "1",
                                        hc10depresan2 ==3~ "2",
                                        hc10depresan2 ==4~ "3",
                                        hc10depresan2 <=-1~ "NA")))->nhats10
table(nhats10$dep10_2)
#score: 1+2 
nhats10$dep10_index = rowSums(nhats10[,c(1040:1041)])
hist(nhats10$dep10_index)
table(nhats10$dep10_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats10$dep10_index_rec<-as.factor(ifelse(nhats10$dep10_index>=3,1,0))
table(nhats10$dep10_index_rec)
#anxiety
table(nhats10$hc10depresan3)
nhats10%>%
  mutate(ax10_1 = as.numeric(case_when(hc10depresan3 ==1  ~ "0",
                                       hc10depresan3 ==2 ~ "1",
                                       hc10depresan3 ==3~ "2",
                                       hc10depresan3 ==4~ "3",
                                       hc10depresan3 <=-1~ "NA")))->nhats10
table(nhats10$ax10_1)
table(nhats10$hc10depresan10)
nhats10%>%
  mutate(ax10_2 = as.numeric(case_when(hc10depresan4 ==1  ~ "0",
                                       hc10depresan4 ==2 ~ "1",
                                       hc10depresan4 ==3~ "2",
                                       hc10depresan4 ==4~ "3",
                                       hc10depresan4 <=-1~ "NA")))->nhats10
table(nhats10$ax10_2)
#score: 1+2 
nhats10$ax10_index = rowSums(nhats10[,c(1044:1045)])
hist(nhats10$ax10_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats10$ax10_index_rec<-as.factor(ifelse(nhats10$ax10_index>=3,1,0))
table(nhats10$ax10_index_rec)


####11####
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5993393/ ----example for #1
#depression
table(nhats11$hc11depresan1)
nhats11%>%
  mutate(dep11_1 = as.numeric(case_when(hc11depresan1 ==1  ~ "0",
                                        hc11depresan1 ==2 ~ "1",
                                        hc11depresan1 ==3~ "2",
                                        hc11depresan1 ==4~ "3",
                                        hc11depresan1 <=-1~ "NA")))->nhats11
table(nhats11$dep11_1)
table(nhats11$hc11depresan2)
nhats11%>%
  mutate(dep11_2 = as.numeric(case_when(hc11depresan2 ==1  ~ "0",
                                        hc11depresan2 ==2 ~ "1",
                                        hc11depresan2 ==3~ "2",
                                        hc11depresan2 ==4~ "3",
                                        hc11depresan2 <=-1~ "NA")))->nhats11
table(nhats11$dep11_2)
#score: 1+2 
nhats11$dep11_index = rowSums(nhats11[,c(1342:1343)])
hist(nhats11$dep11_index)
table(nhats11$dep11_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats11$dep11_index_rec<-as.factor(ifelse(nhats11$dep11_index>=3,1,0))
table(nhats11$dep11_index_rec)
#anxiety
table(nhats11$hc11depresan3)
nhats11%>%
  mutate(ax11_1 = as.numeric(case_when(hc11depresan3 ==1  ~ "0",
                                       hc11depresan3 ==2 ~ "1",
                                       hc11depresan3 ==3~ "2",
                                       hc11depresan3 ==4~ "3",
                                       hc11depresan3 <=-1~ "NA")))->nhats11
table(nhats11$ax11_1)
table(nhats11$hc11depresan11)
nhats11%>%
  mutate(ax11_2 = as.numeric(case_when(hc11depresan4 ==1  ~ "0",
                                       hc11depresan4 ==2 ~ "1",
                                       hc11depresan4 ==3~ "2",
                                       hc11depresan4 ==4~ "3",
                                       hc11depresan4 <=-1~ "NA")))->nhats11
table(nhats11$ax11_2)
#score: 1+2 
nhats11$ax11_index = rowSums(nhats11[,c(1347:1348)])
hist(nhats11$ax11_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats11$ax11_index_rec<-as.factor(ifelse(nhats11$ax11_index>=3,1,0))
table(nhats11$ax11_index_rec)

####12####
#depression
table(nhats12$hc12depresan1)
nhats12%>%
  mutate(dep12_1 = as.numeric(case_when(hc12depresan1 ==1  ~ "0",
                                        hc12depresan1 ==2 ~ "1",
                                        hc12depresan1 ==3~ "2",
                                        hc12depresan1 ==4~ "3",
                                        hc12depresan1 <=-1~ "NA")))->nhats12
table(nhats12$dep12_1)
table(nhats12$hc12depresan2)
nhats12%>%
  mutate(dep12_2 = as.numeric(case_when(hc12depresan2 ==1  ~ "0",
                                        hc12depresan2 ==2 ~ "1",
                                        hc12depresan2 ==3~ "2",
                                        hc12depresan2 ==4~ "3",
                                        hc12depresan2 <=-1~ "NA")))->nhats12
table(nhats12$dep12_2)
#score: 1+2 
nhats12$dep12_index = rowSums(nhats12[,c(1642:1643)])
hist(nhats12$dep12_index)
table(nhats12$dep12_index)
#depression was categorized into a binary variable using PHQ-2 scores greater or equal to three as a cut-off for high depressive symptoms
nhats12$dep12_index_rec<-as.factor(ifelse(nhats12$dep12_index>=3,1,0))
table(nhats12$dep12_index_rec)
#anxiety
table(nhats12$hc12depresan3)
nhats12%>%
  mutate(ax12_1 = as.numeric(case_when(hc12depresan3 ==1  ~ "0",
                                       hc12depresan3 ==2 ~ "1",
                                       hc12depresan3 ==3~ "2",
                                       hc12depresan3 ==4~ "3",
                                       hc12depresan3 <=-1~ "NA")))->nhats12
table(nhats12$ax12_1)
table(nhats12$hc12depresan12)
nhats12%>%
  mutate(ax12_2 = as.numeric(case_when(hc12depresan4 ==1  ~ "0",
                                       hc12depresan4 ==2 ~ "1",
                                       hc12depresan4 ==3~ "2",
                                       hc12depresan4 ==4~ "3",
                                       hc12depresan4 <=-1~ "NA")))->nhats12
table(nhats12$ax12_2)
#score: 1+2 
nhats12$ax12_index = rowSums(nhats12[,c(1646:1647)])
hist(nhats12$ax12_index)
#GAD-2 score less than or equal to two identified as low anxiety and GAD-2 scores greater than or equal to three identified as high anxiety.
nhats12$ax12_index_rec<-as.factor(ifelse(nhats12$ax12_index>=3,1,0))
table(nhats12$ax12_index_rec)


####health conditions#### 
##Health conditions hc1disescn1 to hc1disescn10
####1####
nhats1%>%
  mutate(hc1 = as.numeric(case_when(hc1disescn1 ==1  ~ "1",
                                    hc1disescn1 ==2 ~ "0",
                                    hc1disescn1 <=-1~ "NA")))->nhats1

nhats1%>%
  mutate(hc2 = as.numeric(case_when(hc1disescn2 ==1  ~ "1",
                                    hc1disescn2 ==2 ~ "0",
                                    hc1disescn2 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc3 = as.numeric(case_when(hc1disescn3 ==1  ~ "1",
                                    hc1disescn3 ==2 ~ "0",
                                    hc1disescn3 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc4 = as.numeric(case_when(hc1disescn4 ==1  ~ "1",
                                    hc1disescn4 ==2 ~ "0",
                                    hc1disescn4 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc5 = as.numeric(case_when(hc1disescn5 ==1  ~ "1",
                                    hc1disescn5 ==2 ~ "0",
                                    hc1disescn5 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc6 = as.numeric(case_when(hc1disescn6 ==1  ~ "1",
                                    hc1disescn6 ==2 ~ "0",
                                    hc1disescn6 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc7 = as.numeric(case_when(hc1disescn7 ==1  ~ "1",
                                    hc1disescn7 ==2 ~ "0",
                                    hc1disescn7 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc8 = as.numeric(case_when(hc1disescn8 ==1  ~ "1",
                                    hc1disescn8 ==2 ~ "0",
                                    hc1disescn8 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc9 = as.numeric(case_when(hc1disescn9 ==1  ~ "1",
                                    hc1disescn9 ==2 ~ "0",
                                    hc1disescn9 <=-1~ "NA")))->nhats1
nhats1%>%
  mutate(hc10 = as.numeric(case_when(hc1disescn10 ==1  ~ "1",
                                     hc1disescn10 ==2 ~ "0",
                                     hc1disescn10 <=-1~ "NA")))->nhats1
#score - numer of chronic conditions 
nhats1$chr_cond1 = rowSums(nhats1[,c(1288:1297)])
hist(nhats1$chr_cond1)
#1 or more to replicate some previous work
nhats1$chr_cond1<-as.factor(ifelse(nhats1$chr_cond1>=1,1,0))
table(nhats1$chr_cond1)

####2####
nhats2%>%
  mutate(hc1 = as.numeric(case_when(hc2disescn1 ==1  | hc2disescn1 ==7~ "1" ,
                                    hc2disescn1 ==2 ~ "0",
                                    hc2disescn1 <=-1~ "NA")))->nhats2

nhats2%>%
  mutate(hc2 = as.numeric(case_when(hc2disescn2 ==1 | hc2disescn2 ==7 ~ "1",
                                    hc2disescn2 ==2  ~ "0",
                                    hc2disescn2 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc3 = as.numeric(case_when(hc2disescn3 ==1 | hc2disescn3 ==7 ~ "1",
                                    hc2disescn3 ==2  ~ "0",
                                    hc2disescn3 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc4 = as.numeric(case_when(hc2disescn4 ==1 | hc2disescn4 ==7 ~ "1",
                                    hc2disescn4 ==2  ~ "0",
                                    hc2disescn4 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc5 = as.numeric(case_when(hc2disescn5 ==1 | hc2disescn5 ==7 ~ "1",
                                    hc2disescn5 ==2  ~ "0",
                                    hc2disescn5 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc6 = as.numeric(case_when(hc2disescn6 ==1 | hc2disescn6 ==7 ~ "1",
                                    hc2disescn6 ==2  ~ "0",
                                    hc2disescn6 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc7 = as.numeric(case_when(hc2disescn7 ==1  | hc2disescn7 ==7 ~ "1",
                                    hc2disescn7 ==2  ~ "0",
                                    hc2disescn7 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc8 = as.numeric(case_when(hc2disescn8 ==1 | hc2disescn8 ==7 ~ "1",
                                    hc2disescn8 ==2  ~ "0",
                                    hc2disescn8 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc9 = as.numeric(case_when(hc2disescn9 ==1 | hc2disescn9 ==7 ~ "1",
                                    hc2disescn9 ==2  ~ "0",
                                    hc2disescn9 <=-1~ "NA")))->nhats2
nhats2%>%
  mutate(hc10 = as.numeric(case_when(hc2disescn10 ==1 | hc2disescn10 ==7 ~ "1",
                                     hc2disescn10 ==2 ~ "0",
                                     hc2disescn10 <=-1~ "NA")))->nhats2
#score - numer of chronic conditions 
nhats2$chr_cond2 = rowSums(nhats2[,c(1211:1220)])
hist(nhats2$chr_cond2)
table(nhats2$chr_cond2)
#1 or more to replicate some previous work
nhats2$chr_cond2<-as.factor(ifelse(nhats2$chr_cond2>=1,1,0))
table(nhats2$chr_cond2)

####3####
nhats3%>%
  mutate(hc1 = as.numeric(case_when(hc3disescn1 ==1  | hc3disescn1 ==7~ "1" ,
                                    hc3disescn1 ==2 ~ "0",
                                    hc3disescn1 <=-1~ "NA")))->nhats3

nhats3%>%
  mutate(hc2 = as.numeric(case_when(hc3disescn2 ==1 | hc3disescn2 ==7 ~ "1",
                                    hc3disescn2 ==2  ~ "0",
                                    hc3disescn2 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc3 = as.numeric(case_when(hc3disescn3 ==1 | hc3disescn3 ==7 ~ "1",
                                    hc3disescn3 ==2  ~ "0",
                                    hc3disescn3 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc4 = as.numeric(case_when(hc3disescn4 ==1 | hc3disescn4 ==7 ~ "1",
                                    hc3disescn4 ==2  ~ "0",
                                    hc3disescn4 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc5 = as.numeric(case_when(hc3disescn5 ==1 | hc3disescn5 ==7 ~ "1",
                                    hc3disescn5 ==2  ~ "0",
                                    hc3disescn5 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc6 = as.numeric(case_when(hc3disescn6 ==1 | hc3disescn6 ==7 ~ "1",
                                    hc3disescn6 ==2  ~ "0",
                                    hc3disescn6 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc7 = as.numeric(case_when(hc3disescn7 ==1  | hc3disescn7 ==7 ~ "1",
                                    hc3disescn7 ==2  ~ "0",
                                    hc3disescn7 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc8 = as.numeric(case_when(hc3disescn8 ==1 | hc3disescn8 ==7 ~ "1",
                                    hc3disescn8 ==2  ~ "0",
                                    hc3disescn8 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc9 = as.numeric(case_when(hc3disescn9 ==1 | hc3disescn9 ==7 ~ "1",
                                    hc3disescn9 ==2  ~ "0",
                                    hc3disescn9 <=-1~ "NA")))->nhats3
nhats3%>%
  mutate(hc10 = as.numeric(case_when(hc3disescn10 ==1 | hc3disescn10 ==7 ~ "1",
                                     hc3disescn10 ==2 ~ "0",
                                     hc3disescn10 <=-1~ "NA")))->nhats3
#score - numer of chronic conditions 
nhats3$chr_cond3 = rowSums(nhats3[,c(1206:1215)])
hist(nhats3$chr_cond3)
table(nhats3$chr_cond3)
#1 or more to replicate some previous work
nhats3$chr_cond3<-as.factor(ifelse(nhats3$chr_cond3>=1,1,0))
table(nhats3$chr_cond3)

####4####
nhats4%>%
  mutate(hc1 = as.numeric(case_when(hc4disescn1 ==1  | hc4disescn1 ==7~ "1" ,
                                    hc4disescn1 ==2 ~ "0",
                                    hc4disescn1 <=-1~ "NA")))->nhats4

nhats4%>%
  mutate(hc2 = as.numeric(case_when(hc4disescn2 ==1 | hc4disescn2 ==7 ~ "1",
                                    hc4disescn2 ==2  ~ "0",
                                    hc4disescn2 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc3 = as.numeric(case_when(hc4disescn3 ==1 | hc4disescn3 ==7 ~ "1",
                                    hc4disescn3 ==2  ~ "0",
                                    hc4disescn3 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc4 = as.numeric(case_when(hc4disescn4 ==1 | hc4disescn4 ==7 ~ "1",
                                    hc4disescn4 ==2  ~ "0",
                                    hc4disescn4 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc5 = as.numeric(case_when(hc4disescn5 ==1 | hc4disescn5 ==7 ~ "1",
                                    hc4disescn5 ==2  ~ "0",
                                    hc4disescn5 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc6 = as.numeric(case_when(hc4disescn6 ==1 | hc4disescn6 ==7 ~ "1",
                                    hc4disescn6 ==2  ~ "0",
                                    hc4disescn6 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc7 = as.numeric(case_when(hc4disescn7 ==1  | hc4disescn7 ==7 ~ "1",
                                    hc4disescn7 ==2  ~ "0",
                                    hc4disescn7 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc8 = as.numeric(case_when(hc4disescn8 ==1 | hc4disescn8 ==7 ~ "1",
                                    hc4disescn8 ==2  ~ "0",
                                    hc4disescn8 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc9 = as.numeric(case_when(hc4disescn9 ==1 | hc4disescn9 ==7 ~ "1",
                                    hc4disescn9 ==2  ~ "0",
                                    hc4disescn9 <=-1~ "NA")))->nhats4
nhats4%>%
  mutate(hc10 = as.numeric(case_when(hc4disescn10 ==1 | hc4disescn10 ==7 ~ "1",
                                     hc4disescn10 ==2 ~ "0",
                                     hc4disescn10 <=-1~ "NA")))->nhats4
#score - numer of chronic conditions 
nhats4$chr_cond4 = rowSums(nhats4[,c(1155:1164)])
hist(nhats4$chr_cond4)
table(nhats4$chr_cond4)
#1 or more to replicate some previous work
nhats4$chr_cond4<-as.factor(ifelse(nhats4$chr_cond4>=1,1,0))
table(nhats4$chr_cond4)

####5####
nhats5%>%
  mutate(hc1 = as.numeric(case_when(hc5disescn1 ==1  | hc5disescn1 ==7~ "1" ,
                                    hc5disescn1 ==2 ~ "0",
                                    hc5disescn1 <=-1~ "NA")))->nhats5

nhats5%>%
  mutate(hc2 = as.numeric(case_when(hc5disescn2 ==1 | hc5disescn2 ==7 ~ "1",
                                    hc5disescn2 ==2  ~ "0",
                                    hc5disescn2 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc3 = as.numeric(case_when(hc5disescn3 ==1 | hc5disescn3 ==7 ~ "1",
                                    hc5disescn3 ==2  ~ "0",
                                    hc5disescn3 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc4 = as.numeric(case_when(hc5disescn4 ==1 | hc5disescn4 ==7 ~ "1",
                                    hc5disescn4 ==2  ~ "0",
                                    hc5disescn4 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc5 = as.numeric(case_when(hc5disescn5 ==1 | hc5disescn5 ==7 ~ "1",
                                    hc5disescn5 ==2  ~ "0",
                                    hc5disescn5 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc6 = as.numeric(case_when(hc5disescn6 ==1 | hc5disescn6 ==7 ~ "1",
                                    hc5disescn6 ==2  ~ "0",
                                    hc5disescn6 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc7 = as.numeric(case_when(hc5disescn7 ==1  | hc5disescn7 ==7 ~ "1",
                                    hc5disescn7 ==2  ~ "0",
                                    hc5disescn7 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc8 = as.numeric(case_when(hc5disescn8 ==1 | hc5disescn8 ==7 ~ "1",
                                    hc5disescn8 ==2  ~ "0",
                                    hc5disescn8 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc9 = as.numeric(case_when(hc5disescn9 ==1 | hc5disescn9 ==7 ~ "1",
                                    hc5disescn9 ==2  ~ "0",
                                    hc5disescn9 <=-1~ "NA")))->nhats5
nhats5%>%
  mutate(hc10 = as.numeric(case_when(hc5disescn10 ==1 | hc5disescn10 ==7 ~ "1",
                                     hc5disescn10 ==2 ~ "0",
                                     hc5disescn10 <=-1~ "NA")))->nhats5
#score - numer of chronic conditions 
nhats5$chr_cond5 = rowSums(nhats5[,c(1580:1589)])
hist(nhats5$chr_cond5)
table(nhats5$chr_cond5)
#1 or more to replicate some previous work
nhats5$chr_cond5<-as.factor(ifelse(nhats5$chr_cond5>=1,1,0))
table(nhats5$chr_cond5)

####6####
nhats6%>%
  mutate(hc1 = as.numeric(case_when(hc6disescn1 ==1  | hc6disescn1 ==7~ "1" ,
                                    hc6disescn1 ==2 ~ "0",
                                    hc6disescn1 <=-1~ "NA")))->nhats6

nhats6%>%
  mutate(hc2 = as.numeric(case_when(hc6disescn2 ==1 | hc6disescn2 ==7 ~ "1",
                                    hc6disescn2 ==2  ~ "0",
                                    hc6disescn2 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc3 = as.numeric(case_when(hc6disescn3 ==1 | hc6disescn3 ==7 ~ "1",
                                    hc6disescn3 ==2  ~ "0",
                                    hc6disescn3 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc4 = as.numeric(case_when(hc6disescn4 ==1 | hc6disescn4 ==7 ~ "1",
                                    hc6disescn4 ==2  ~ "0",
                                    hc6disescn4 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc5 = as.numeric(case_when(hc6disescn5 ==1 | hc6disescn5 ==7 ~ "1",
                                    hc6disescn5 ==2  ~ "0",
                                    hc6disescn5 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc6 = as.numeric(case_when(hc6disescn6 ==1 | hc6disescn6 ==7 ~ "1",
                                    hc6disescn6 ==2  ~ "0",
                                    hc6disescn6 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc7 = as.numeric(case_when(hc6disescn7 ==1  | hc6disescn7 ==7 ~ "1",
                                    hc6disescn7 ==2  ~ "0",
                                    hc6disescn7 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc8 = as.numeric(case_when(hc6disescn8 ==1 | hc6disescn8 ==7 ~ "1",
                                    hc6disescn8 ==2  ~ "0",
                                    hc6disescn8 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc9 = as.numeric(case_when(hc6disescn9 ==1 | hc6disescn9 ==7 ~ "1",
                                    hc6disescn9 ==2  ~ "0",
                                    hc6disescn9 <=-1~ "NA")))->nhats6
nhats6%>%
  mutate(hc10 = as.numeric(case_when(hc6disescn10 ==1 | hc6disescn10 ==7 ~ "1",
                                     hc6disescn10 ==2 ~ "0",
                                     hc6disescn10 <=-1~ "NA")))->nhats6
#score - numer of chronic conditions 
nhats6$chr_cond6 = rowSums(nhats6[,c(1333:1342)])
hist(nhats6$chr_cond6)
table(nhats6$chr_cond6)
#1 or more to replicate some previous work
nhats6$chr_cond6<-as.factor(ifelse(nhats6$chr_cond6>=1,1,0))
table(nhats6$chr_cond6)


####7####
nhats7%>%
  mutate(hc1 = as.numeric(case_when(hc7disescn1 ==1  | hc7disescn1 ==7~ "1" ,
                                    hc7disescn1 ==2 ~ "0",
                                    hc7disescn1 <=-1~ "NA")))->nhats7

nhats7%>%
  mutate(hc2 = as.numeric(case_when(hc7disescn2 ==1 | hc7disescn2 ==7 ~ "1",
                                    hc7disescn2 ==2  ~ "0",
                                    hc7disescn2 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc3 = as.numeric(case_when(hc7disescn3 ==1 | hc7disescn3 ==7 ~ "1",
                                    hc7disescn3 ==2  ~ "0",
                                    hc7disescn3 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc4 = as.numeric(case_when(hc7disescn4 ==1 | hc7disescn4 ==7 ~ "1",
                                    hc7disescn4 ==2  ~ "0",
                                    hc7disescn4 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc5 = as.numeric(case_when(hc7disescn5 ==1 | hc7disescn5 ==7 ~ "1",
                                    hc7disescn5 ==2  ~ "0",
                                    hc7disescn5 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc6 = as.numeric(case_when(hc7disescn6 ==1 | hc7disescn6 ==7 ~ "1",
                                    hc7disescn6 ==2  ~ "0",
                                    hc7disescn6 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc7 = as.numeric(case_when(hc7disescn7 ==1  | hc7disescn7 ==7 ~ "1",
                                    hc7disescn7 ==2  ~ "0",
                                    hc7disescn7 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc8 = as.numeric(case_when(hc7disescn8 ==1 | hc7disescn8 ==7 ~ "1",
                                    hc7disescn8 ==2  ~ "0",
                                    hc7disescn8 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc9 = as.numeric(case_when(hc7disescn9 ==1 | hc7disescn9 ==7 ~ "1",
                                    hc7disescn9 ==2  ~ "0",
                                    hc7disescn9 <=-1~ "NA")))->nhats7
nhats7%>%
  mutate(hc10 = as.numeric(case_when(hc7disescn10 ==1 | hc7disescn10 ==7 ~ "1",
                                     hc7disescn10 ==2 ~ "0",
                                     hc7disescn10 <=-1~ "NA")))->nhats7
#score - numer of chronic conditions 
nhats7$chr_cond7 = rowSums(nhats7[,c(1330:1339)])
hist(nhats7$chr_cond7)
table(nhats7$chr_cond7)
#1 or more to replicate some previous work
nhats7$chr_cond7<-as.factor(ifelse(nhats7$chr_cond7>=1,1,0))
table(nhats7$chr_cond7)

####8####
nhats8%>%
  mutate(hc1 = as.numeric(case_when(hc8disescn1 ==1  | hc8disescn1 ==7~ "1" ,
                                    hc8disescn1 ==2 ~ "0",
                                    hc8disescn1 <=-1~ "NA")))->nhats8

nhats8%>%
  mutate(hc2 = as.numeric(case_when(hc8disescn2 ==1 | hc8disescn2 ==7 ~ "1",
                                    hc8disescn2 ==2  ~ "0",
                                    hc8disescn2 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc3 = as.numeric(case_when(hc8disescn3 ==1 | hc8disescn3 ==7 ~ "1",
                                    hc8disescn3 ==2  ~ "0",
                                    hc8disescn3 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc4 = as.numeric(case_when(hc8disescn4 ==1 | hc8disescn4 ==7 ~ "1",
                                    hc8disescn4 ==2  ~ "0",
                                    hc8disescn4 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc5 = as.numeric(case_when(hc8disescn5 ==1 | hc8disescn5 ==7 ~ "1",
                                    hc8disescn5 ==2  ~ "0",
                                    hc8disescn5 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc6 = as.numeric(case_when(hc8disescn6 ==1 | hc8disescn6 ==7 ~ "1",
                                    hc8disescn6 ==2  ~ "0",
                                    hc8disescn6 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc7 = as.numeric(case_when(hc8disescn7 ==1  | hc8disescn7 ==7 ~ "1",
                                    hc8disescn7 ==2  ~ "0",
                                    hc8disescn7 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc8 = as.numeric(case_when(hc8disescn8 ==1 | hc8disescn8 ==7 ~ "1",
                                    hc8disescn8 ==2  ~ "0",
                                    hc8disescn8 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc9 = as.numeric(case_when(hc8disescn9 ==1 | hc8disescn9 ==7 ~ "1",
                                    hc8disescn9 ==2  ~ "0",
                                    hc8disescn9 <=-1~ "NA")))->nhats8
nhats8%>%
  mutate(hc10 = as.numeric(case_when(hc8disescn10 ==1 | hc8disescn10 ==7 ~ "1",
                                     hc8disescn10 ==2 ~ "0",
                                     hc8disescn10 <=-1~ "NA")))->nhats8
#score - numer of chronic conditions 
nhats8$chr_cond8 = rowSums(nhats8[,c(1360:1369)])
hist(nhats8$chr_cond8)
table(nhats8$chr_cond8)
#1 or more to replicate some previous work
nhats8$chr_cond8<-as.factor(ifelse(nhats8$chr_cond8>=1,1,0))
table(nhats8$chr_cond8)

####9####
nhats9%>%
  mutate(hc1 = as.numeric(case_when(hc9disescn1 ==1  | hc9disescn1 ==7~ "1" ,
                                    hc9disescn1 ==2 ~ "0",
                                    hc9disescn1 <=-1~ "NA")))->nhats9

nhats9%>%
  mutate(hc2 = as.numeric(case_when(hc9disescn2 ==1 | hc9disescn2 ==7 ~ "1",
                                    hc9disescn2 ==2  ~ "0",
                                    hc9disescn2 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc3 = as.numeric(case_when(hc9disescn3 ==1 | hc9disescn3 ==7 ~ "1",
                                    hc9disescn3 ==2  ~ "0",
                                    hc9disescn3 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc4 = as.numeric(case_when(hc9disescn4 ==1 | hc9disescn4 ==7 ~ "1",
                                    hc9disescn4 ==2  ~ "0",
                                    hc9disescn4 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc5 = as.numeric(case_when(hc9disescn5 ==1 | hc9disescn5 ==7 ~ "1",
                                    hc9disescn5 ==2  ~ "0",
                                    hc9disescn5 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc6 = as.numeric(case_when(hc9disescn6 ==1 | hc9disescn6 ==7 ~ "1",
                                    hc9disescn6 ==2  ~ "0",
                                    hc9disescn6 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc7 = as.numeric(case_when(hc9disescn7 ==1  | hc9disescn7 ==7 ~ "1",
                                    hc9disescn7 ==2  ~ "0",
                                    hc9disescn7 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc8 = as.numeric(case_when(hc9disescn9 ==1 | hc9disescn9 ==7 ~ "1",
                                    hc9disescn9 ==2  ~ "0",
                                    hc9disescn9 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc9 = as.numeric(case_when(hc9disescn9 ==1 | hc9disescn9 ==7 ~ "1",
                                    hc9disescn9 ==2  ~ "0",
                                    hc9disescn9 <=-1~ "NA")))->nhats9
nhats9%>%
  mutate(hc10 = as.numeric(case_when(hc9disescn10 ==1 | hc9disescn10 ==7 ~ "1",
                                     hc9disescn10 ==2 ~ "0",
                                     hc9disescn10 <=-1~ "NA")))->nhats9
#score - numer of chronic conditions 
nhats9$chr_cond9 = rowSums(nhats9[,c(1332:1341)])
hist(nhats9$chr_cond9)
table(nhats9$chr_cond9)
#1 or more to replicate some previous work
nhats9$chr_cond9<-as.factor(ifelse(nhats9$chr_cond9>=1,1,0))
table(nhats9$chr_cond9)


####10####
nhats10%>%
  mutate(hc1 = as.numeric(case_when(hc10disescn1 ==1  | hc10disescn1 ==7~ "1" ,
                                    hc10disescn1 ==2 ~ "0",
                                    hc10disescn1 <=-1~ "NA")))->nhats10

nhats10%>%
  mutate(hc2 = as.numeric(case_when(hc10disescn2 ==1 | hc10disescn2 ==7 ~ "1",
                                    hc10disescn2 ==2  ~ "0",
                                    hc10disescn2 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc3 = as.numeric(case_when(hc10disescn3 ==1 | hc10disescn3 ==7 ~ "1",
                                    hc10disescn3 ==2  ~ "0",
                                    hc10disescn3 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc4 = as.numeric(case_when(hc10disescn4 ==1 | hc10disescn4 ==7 ~ "1",
                                    hc10disescn4 ==2  ~ "0",
                                    hc10disescn4 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc5 = as.numeric(case_when(hc10disescn5 ==1 | hc10disescn5 ==7 ~ "1",
                                    hc10disescn5 ==2  ~ "0",
                                    hc10disescn5 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc6 = as.numeric(case_when(hc10disescn6 ==1 | hc10disescn6 ==7 ~ "1",
                                    hc10disescn6 ==2  ~ "0",
                                    hc10disescn6 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc7 = as.numeric(case_when(hc10disescn7 ==1  | hc10disescn7 ==7 ~ "1",
                                    hc10disescn7 ==2  ~ "0",
                                    hc10disescn7 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc8 = as.numeric(case_when(hc10disescn8 ==1 | hc10disescn8 ==7 ~ "1",
                                    hc10disescn8 ==2  ~ "0",
                                    hc10disescn8 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc9 = as.numeric(case_when(hc10disescn9 ==1 | hc10disescn9 ==7 ~ "1",
                                    hc10disescn9 ==2  ~ "0",
                                    hc10disescn9 <=-1~ "NA")))->nhats10
nhats10%>%
  mutate(hc10 = as.numeric(case_when(hc10disescn10 ==1 | hc10disescn10 ==7 ~ "1",
                                     hc10disescn10 ==2 ~ "0",
                                     hc10disescn10 <=-1~ "NA")))->nhats10
#score - numer of chronic conditions 
nhats10$chr_cond10 = rowSums(nhats10[,c(1048:1057)])
hist(nhats10$chr_cond10)
table(nhats10$chr_cond10)
#1 or more to replicate some previous work
nhats10$chr_cond10<-as.factor(ifelse(nhats10$chr_cond10>=1,1,0))
table(nhats10$chr_cond10)


####11####
nhats11%>%
  mutate(hc1 = as.numeric(case_when(hc11disescn1 ==1  | hc11disescn1 ==7~ "1" ,
                                    hc11disescn1 ==2 ~ "0",
                                    hc11disescn1 <=-1~ "NA")))->nhats11

nhats11%>%
  mutate(hc2 = as.numeric(case_when(hc11disescn2 ==1 | hc11disescn2 ==7 ~ "1",
                                    hc11disescn2 ==2  ~ "0",
                                    hc11disescn2 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc3 = as.numeric(case_when(hc11disescn3 ==1 | hc11disescn3 ==7 ~ "1",
                                    hc11disescn3 ==2  ~ "0",
                                    hc11disescn3 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc4 = as.numeric(case_when(hc11disescn4 ==1 | hc11disescn4 ==7 ~ "1",
                                    hc11disescn4 ==2  ~ "0",
                                    hc11disescn4 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc5 = as.numeric(case_when(hc11disescn5 ==1 | hc11disescn5 ==7 ~ "1",
                                    hc11disescn5 ==2  ~ "0",
                                    hc11disescn5 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc6 = as.numeric(case_when(hc11disescn6 ==1 | hc11disescn6 ==7 ~ "1",
                                    hc11disescn6 ==2  ~ "0",
                                    hc11disescn6 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc7 = as.numeric(case_when(hc11disescn7 ==1  | hc11disescn7 ==7 ~ "1",
                                    hc11disescn7 ==2  ~ "0",
                                    hc11disescn7 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc8 = as.numeric(case_when(hc11disescn8 ==1 | hc11disescn8 ==7 ~ "1",
                                    hc11disescn8 ==2  ~ "0",
                                    hc11disescn8 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc9 = as.numeric(case_when(hc11disescn9 ==1 | hc11disescn9 ==7 ~ "1",
                                    hc11disescn9 ==2  ~ "0",
                                    hc11disescn9 <=-1~ "NA")))->nhats11
nhats11%>%
  mutate(hc10 = as.numeric(case_when(hc11disescn10 ==1 | hc11disescn10 ==7 ~ "1",
                                     hc11disescn10 ==2 ~ "0",
                                     hc11disescn10 <=-1~ "NA")))->nhats11
#score - numer of chronic conditions 
nhats11$chr_cond11 = rowSums(nhats11[,c(1351:1360)])
hist(nhats11$chr_cond11)
table(nhats11$chr_cond11)
#1 or more to replicate some previous work
nhats11$chr_cond11<-as.factor(ifelse(nhats11$chr_cond11>=1,1,0))
table(nhats11$chr_cond11)

####12####
nhats12%>%
  mutate(hc1 = as.numeric(case_when(hc12disescn1 ==1  | hc12disescn1 ==7~ "1" ,
                                    hc12disescn1 ==2 ~ "0",
                                    hc12disescn1 <=-1~ "NA")))->nhats12

nhats12%>%
  mutate(hc2 = as.numeric(case_when(hc12disescn2 ==1 | hc12disescn2 ==7 ~ "1",
                                    hc12disescn2 ==2  ~ "0",
                                    hc12disescn2 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc3 = as.numeric(case_when(hc12disescn3 ==1 | hc12disescn3 ==7 ~ "1",
                                    hc12disescn3 ==2  ~ "0",
                                    hc12disescn3 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc4 = as.numeric(case_when(hc12disescn4 ==1 | hc12disescn4 ==7 ~ "1",
                                    hc12disescn4 ==2  ~ "0",
                                    hc12disescn4 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc5 = as.numeric(case_when(hc12disescn5 ==1 | hc12disescn5 ==7 ~ "1",
                                    hc12disescn5 ==2  ~ "0",
                                    hc12disescn5 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc6 = as.numeric(case_when(hc12disescn6 ==1 | hc12disescn6 ==7 ~ "1",
                                    hc12disescn6 ==2  ~ "0",
                                    hc12disescn6 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc7 = as.numeric(case_when(hc12disescn7 ==1  | hc12disescn7 ==7 ~ "1",
                                    hc12disescn7 ==2  ~ "0",
                                    hc12disescn7 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc8 = as.numeric(case_when(hc12disescn8 ==1 | hc12disescn8 ==7 ~ "1",
                                    hc12disescn8 ==2  ~ "0",
                                    hc12disescn8 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc9 = as.numeric(case_when(hc12disescn9 ==1 | hc12disescn9 ==7 ~ "1",
                                    hc12disescn9 ==2  ~ "0",
                                    hc12disescn9 <=-1~ "NA")))->nhats12
nhats12%>%
  mutate(hc10 = as.numeric(case_when(hc12disescn10 ==1 | hc12disescn10 ==7 ~ "1",
                                     hc12disescn10 ==2 ~ "0",
                                     hc12disescn10 <=-1~ "NA")))->nhats12
#score - numer of chronic conditions 
nhats12$chr_cond12 = rowSums(nhats12[,c(1650:1659)])
hist(nhats12$chr_cond12)
table(nhats12$chr_cond12)
#1 or more to replicate some previous work
nhats12$chr_cond12<-as.factor(ifelse(nhats12$chr_cond12>=1,1,0))
table(nhats12$chr_cond12)

###Dementia duration#### Seguimiento de la demencia= 
#en ronda 1 hay que mirar la edad de diagnostico, en el resto si ha existido en la ronda.
#La fecha fin es la fecha de fallecimiento


###gender####
# get gender for all, and remove -1 from round 1
#nhats1$r1dgender# no NAs
table(is.na(nhats5$r5dgender))
nhats1%>%
  mutate(sex = as.numeric(case_when(r1dgender ==1  ~ "1",
                                    r1dgender ==2  ~ "2")))->nhats1
nhats2[,"sex"] <- NA
nhats3[,"sex"] <- NA
nhats4[,"sex"] <- NA
nhats5%>%
  mutate(sex = as.numeric(case_when(r5dgender ==1  ~ "1",
                                    r5dgender ==2  ~ "2",
                                    r5dgender <=-1  ~ "NA")))->nhats5
table(nhats5$sex)
table(is.na(nhats5$sex))
length(unique(nhats5$spid))
nhats6[,"sex"] <- NA
nhats7[,"sex"] <- NA
nhats8[,"sex"] <- NA
nhats9[,"sex"] <- NA
nhats10[,"sex"] <- NA
nhats11[,"sex"] <- NA

nhats12%>%
  mutate(sex = as.numeric(case_when(r12dgender ==1  ~ "1",
                                    r12dgender ==2  ~ "2",
                                    r12dgender <=-1  ~ "NA")))->nhats12

####residence status####

#1 comm/ 2 other

nhats1$res<-ifelse(nhats1$r1dresid==1,1,2)
nhats2$res<-ifelse(nhats2$r2dresid==1,1,2)
nhats3$res<-ifelse(nhats3$r3dresid==1,1,2)
nhats4$res<-ifelse(nhats4$r4dresid==1,1,2)
nhats5$res<-ifelse(nhats5$r5dresid==1,1,2)
nhats6$res<-ifelse(nhats6$r6dresid==1,1,2)
nhats7$res<-ifelse(nhats7$r7dresid==1,1,2)
nhats8$res<-ifelse(nhats8$r8dresid==1,1,2)
nhats9$res<-ifelse(nhats9$r9dresid==1,1,2)
nhats10$res<-ifelse(nhats10$r10dresid==1,1,2)
nhats11$res<-ifelse(nhats11$r11dresid==1,1,2)
nhats12$res<-ifelse(nhats12$r12dresid==1,1,2)

####weights####

#nhats1$we<-nhats1$w1anfinwgt0
#nhats2$we<-nhats2$w2anfinwgt0
#nhats3$we<-nhats3$w3anfinwgt0
#nhats4$we<-nhats4$w4anfinwgt0
#nhats5$we<-nhats5$w5anfinwgt0
#nhats6$we<-nhats6$w6anfinwgt0
#nhats7$we<-nhats7$w7anfinwgt0
#nhats8$we<-nhats8$w8anfinwgt0
#nhats9$we<-nhats9$w9anfinwgt0
#nhats10$we<-nhats10$w10anfinwgt0
#nhats11$we<-nhats11$w11anfinwgt0
#nhats12$we<-nhats12$w12anfinwgt0

####education####
table(nhats1$el1higstschl)

# get gender for all, and remove -1 from round 1
#nhats1$r1dgender# no NAs
nhats1%>%
  mutate(education = as.numeric(case_when(el1higstschl >=1 & el1higstschl<=3 ~ "1",
                                          el1higstschl >=4 & el1higstschl<=5  ~ "2",
                                          el1higstschl ==6 ~ "3",
                                          el1higstschl >=7 ~ "4",
                                          el1higstschl <=-1  ~ "NA")))->nhats1
table(nhats1$education)
table(nhats1$el1higstschl)
nhats2[,"education"] <- NA
nhats3[,"education"] <- NA
nhats4[,"education"] <- NA
nhats5%>%
  mutate(education = as.numeric(case_when(el5higstschl >=1 & el5higstschl<=3 ~ "1",
                                          el5higstschl >=4 & el5higstschl<=5  ~ "2",
                                          el5higstschl ==6 ~ "3",
                                          el5higstschl >=7 ~ "4",
                                          el5higstschl <=-1  ~ "NA")))->nhats5
table(nhats5$education)

length(unique(nhats5$spid))
nhats6[,"education"] <- NA
nhats7[,"education"] <- NA
nhats8[,"education"] <- NA
nhats9[,"education"] <- NA
nhats10[,"education"] <- NA
nhats11[,"education"] <- NA

nhats11$ia11toti

nhats12%>%
  mutate(education = as.numeric(case_when(el12dhigstschl >=1 & el12dhigstschl<=3 ~ "1",
                                          el12dhigstschl >=4 & el12dhigstschl<=5  ~ "2",
                                          el12dhigstschl ==6 ~ "3",
                                          el12dhigstschl >=7 ~ "4",
                                          el12dhigstschl <=-1  ~ "NA")))->nhats12
table(nhats12$education)

####income, we use total income -lots of missing but hope for the best when imputing, using other demographics ####
# ony present in 1,3,5,7,9,11. we need to replace 2,4,6,8,10 with previous wave. 
nhats1$ia1totinc<-ifelse(nhats1$ia1totinc<=-1, NA, nhats1$ia1totinc) 
nhats1%>%
  mutate(income = as.numeric(case_when(ia1totinc <14000  ~ "1",
                                       ia1totinc >=14000 & ia1totinc<21999 ~ "2",
                                       ia1totinc >=22000 & ia1totinc<35999  ~ "3",
                                       ia1totinc >=36000 & ia1totinc<48999  ~ "4",
                                       ia1totinc >=49000~ "5")))->nhats1  
table(nhats1$income)
nhats2[,"income"] <- NA
nhats3$ia3totinc<-ifelse(nhats3$ia3totinc<=-1, NA, nhats3$ia3totinc) 
nhats3%>%
  mutate(income = as.numeric(case_when(ia3totinc <14000  ~ "1",
                                       ia3totinc >=14000 & ia3totinc<21999 ~ "2",
                                       ia3totinc >=22000 & ia3totinc<35999  ~ "3",
                                       ia3totinc >=36000 & ia3totinc<48999  ~ "4",
                                       ia3totinc >=49000~ "5")))->nhats3  
table(nhats3$income)
nhats4[,"income"] <- NA
nhats5$ia5totinc<-ifelse(nhats5$ia5totinc<=-1, NA, nhats5$ia5totinc) 
nhats5%>%
  mutate(income = as.numeric(case_when(ia5totinc <14000  ~ "1",
                                       ia5totinc >=14000 & ia5totinc<21999 ~ "2",
                                       ia5totinc >=22000 & ia5totinc<35999  ~ "3",
                                       ia5totinc >=36000 & ia5totinc<48999  ~ "4",
                                       ia5totinc >=49000~ "5")))->nhats5  
table(nhats5$income)
nhats6[,"income"] <- NA
nhats7$ia7totinc<-ifelse(nhats7$ia7totinc<=-1, NA, nhats7$ia7totinc) 
nhats7%>%
  mutate(income = as.numeric(case_when(ia7totinc <14000  ~ "1",
                                       ia7totinc >=14000 & ia7totinc<21999 ~ "2",
                                       ia7totinc >=22000 & ia7totinc<35999  ~ "3",
                                       ia7totinc >=36000 & ia7totinc<48999  ~ "4",
                                       ia7totinc >=49000~ "5")))->nhats7  
table(nhats7$income)
nhats8[,"income"] <- NA
nhats9$ia9totinc<-ifelse(nhats9$ia9totinc<=-1, NA, nhats9$ia9totinc) 
nhats9%>%
  mutate(income = as.numeric(case_when(ia9totinc <14000  ~ "1",
                                       ia9totinc >=14000 & ia9totinc<21999 ~ "2",
                                       ia9totinc >=22000 & ia9totinc<35999  ~ "3",
                                       ia9totinc >=36000 & ia9totinc<48999  ~ "4",
                                       ia9totinc >=49000~ "5")))->nhats9  
table(nhats9$income)

nhats10[,"income"] <- NA
nhats11$ia11totinc<-ifelse(nhats11$ia11totinc<=-1, NA, nhats11$ia11totinc) 
nhats11%>%
  mutate(income = as.numeric(case_when(ia11totinc <14000  ~ "1",
                                       ia11totinc >=14000 & ia11totinc<21999 ~ "2",
                                       ia11totinc >=22000 & ia11totinc<35999  ~ "3",
                                       ia11totinc >=36000 & ia11totinc<48999  ~ "4",
                                       ia11totinc >=49000~ "5")))->nhats11  
table(nhats11$income)

nhats12%>%
  mutate(income = as.numeric(case_when(ia12totinc <14000  ~ "1",
                                       ia12totinc >=14000 & ia12totinc<21999 ~ "2",
                                       ia12totinc >=22000 & ia12totinc<35999  ~ "3",
                                       ia12totinc >=36000 & ia12totinc<48999  ~ "4",
                                       ia12totinc >=49000~ "5")))->nhats12  
table(nhats12$income)

####sensory impairment####
# based on Association of Vision Loss With Cognition in Older Adults - JAMA

####hearing issues ####
#hear phone
nhats1$ss1hearphone<-ifelse(nhats1$ss1hearphone<=-1, NA, nhats1$ss1hearphone) 
table(nhats1$ss1hearphone, useNA="always")
nhats2$ss2hearphone<-ifelse(nhats2$ss2hearphone<=-1, NA, nhats2$ss2hearphone) 
table(nhats2$ss2hearphone, useNA="always")
nhats3$ss3hearphone<-ifelse(nhats3$ss3hearphone<=-1, NA, nhats3$ss3hearphone) 
table(nhats3$ss3hearphone, useNA="always")
nhats4$ss4hearphone<-ifelse(nhats4$ss4hearphone<=-1, NA, nhats4$ss4hearphone) 
table(nhats4$ss4hearphone, useNA="always")
nhats5$ss5hearphone<-ifelse(nhats5$ss5hearphone<=-1, NA, nhats5$ss5hearphone) 
table(nhats5$ss5hearphone, useNA="always")
nhats6$ss6hearphone<-ifelse(nhats6$ss6hearphone<=-1, NA, nhats6$ss6hearphone) 
table(nhats6$ss6hearphone, useNA="always")
nhats7$ss7hearphone<-ifelse(nhats7$ss7hearphone<=-1, NA, nhats7$ss7hearphone)
table(nhats7$ss7hearphone, useNA="always")
nhats8$ss8hearphone<-ifelse(nhats8$ss8hearphone<=-1, NA, nhats8$ss8hearphone) 
table(nhats8$ss8hearphone, useNA="always")
nhats9$ss9hearphone<-ifelse(nhats9$ss9hearphone<=-1, NA, nhats9$ss9hearphone) 
table(nhats9$ss9hearphone, useNA="always")
nhats10$ss10hearphone<-ifelse(nhats10$ss10hearphone<=-1, NA, nhats10$ss10hearphone) 
table(nhats10$ss10hearphone, useNA="always")
nhats11$ss11hearphone<-ifelse(nhats11$ss11hearphone<=-1, NA, nhats11$ss11hearphone) 
table(nhats11$ss11hearphone, useNA="always")
nhats12$ss12hearphone<-ifelse(nhats12$ss12hearphone<=-1, NA, nhats12$ss12hearphone) 
table(nhats12$ss12hearphone, useNA="always")

#hear others with tv or radio on
nhats1$ss1convwradi<-ifelse(nhats1$ss1convwradi<=-1, NA, nhats1$ss1convwradi) 
table(nhats1$ss1convwradi, useNA="always")
nhats2$ss2convwradi<-ifelse(nhats2$ss2convwradi<=-1, NA, nhats2$ss2convwradi) 
table(nhats2$ss2convwradi, useNA="always")
nhats3$ss3convwradi<-ifelse(nhats3$ss3convwradi<=-1, NA, nhats3$ss3convwradi) 
table(nhats3$ss3convwradi, useNA="always")
nhats4$ss4convwradi<-ifelse(nhats4$ss4convwradi<=-1, NA, nhats4$ss4convwradi) 
table(nhats4$ss4convwradi, useNA="always")
nhats5$ss5convwradi<-ifelse(nhats5$ss5convwradi<=-1, NA, nhats5$ss5convwradi) 
table(nhats5$ss5convwradi, useNA="always")
nhats6$ss6convwradi<-ifelse(nhats6$ss6convwradi<=-1, NA, nhats6$ss6convwradi) 
table(nhats6$ss6convwradi, useNA="always")
nhats7$ss7convwradi<-ifelse(nhats7$ss7convwradi<=-1, NA, nhats7$ss7convwradi)
table(nhats7$ss7convwradi, useNA="always")
nhats8$ss8convwradi<-ifelse(nhats8$ss8convwradi<=-1, NA, nhats8$ss8convwradi) 
table(nhats8$ss8convwradi, useNA="always")
nhats9$ss9convwradi<-ifelse(nhats9$ss9convwradi<=-1, NA, nhats9$ss9convwradi) 
table(nhats9$ss9convwradi, useNA="always")
nhats10$ss10convwradi<-ifelse(nhats10$ss10convwradi<=-1, NA, nhats10$ss10convwradi) 
table(nhats11$ss10convwradi, useNA="always")
nhats11$ss11convwradi<-ifelse(nhats11$ss11convwradi<=-1, NA, nhats11$ss11convwradi) 
table(nhats11$ss11convwradi, useNA="always")
nhats12$ss12convwradi<-ifelse(nhats12$ss12convwradi<=-1, NA, nhats12$ss12convwradi) 
table(nhats12$ss12convwradi, useNA="always")

nhats1$hear_issues<-ifelse(nhats1$ss1hearphone==2|nhats1$ss1convwradi==2, 1, 0)
table(nhats1$hear_issues, useNA="always")
nhats2$hear_issues<-ifelse(nhats2$ss2hearphone==2|nhats2$ss2convwradi==2, 1, 0)
table(nhats2$hear_issues, useNA="always")
nhats3$hear_issues<-ifelse(nhats3$ss3hearphone==2|nhats3$ss3convwradi==2, 1, 0)
table(nhats3$hear_issues, useNA="always")
nhats4$hear_issues<-ifelse(nhats4$ss4hearphone==2|nhats4$ss4convwradi==2, 1, 0)
table(nhats4$hear_issues, useNA="always")
nhats5$hear_issues<-ifelse(nhats5$ss5hearphone==2|nhats5$ss5convwradi==2, 1, 0)
table(nhats5$hear_issues, useNA="always")
nhats6$hear_issues<-ifelse(nhats6$ss6hearphone==2|nhats6$ss6convwradi==2, 1, 0)
table(nhats6$hear_issues, useNA="always")
nhats7$hear_issues<-ifelse(nhats7$ss7hearphone==2|nhats7$ss7convwradi==2, 1, 0)
table(nhats7$hear_issues, useNA="always")
nhats8$hear_issues<-ifelse(nhats8$ss8hearphone==2|nhats8$ss8convwradi==2, 1, 0)
table(nhats8$hear_issues, useNA="always")
nhats9$hear_issues<-ifelse(nhats9$ss9hearphone==2|nhats9$ss9convwradi==2, 1, 0)
table(nhats9$hear_issues, useNA="always")
nhats10$hear_issues<-ifelse(nhats10$ss10hearphone==2|nhats10$ss10convwradi==2, 1, 0)
table(nhats10$hear_issues, useNA="always")
nhats11$hear_issues<-ifelse(nhats11$ss11hearphone==2|nhats11$ss11convwradi==2, 1, 0)
table(nhats11$hear_issues, useNA="always")
nhats12$hear_issues<-ifelse(nhats12$ss12hearphone==2|nhats12$ss12convwradi==2, 1, 0)
table(nhats12$hear_issues, useNA="always")

#vision 
#distance  
nhats1$ss1seewellst<-ifelse(nhats1$ss1seewellst<=-1, NA, nhats1$ss1seewellst) 
table(nhats1$ss1seewellst, useNA="always")
nhats2$ss2seewellst<-ifelse(nhats2$ss2seewellst<=-1, NA, nhats2$ss2seewellst) 
table(nhats2$ss2seewellst, useNA="always")
nhats3$ss3seewellst<-ifelse(nhats3$ss3seewellst<=-1, NA, nhats3$ss3seewellst) 
table(nhats3$ss3seewellst, useNA="always")
nhats4$ss4seewellst<-ifelse(nhats4$ss4seewellst<=-1, NA, nhats4$ss4seewellst) 
table(nhats4$ss4seewellst, useNA="always")
nhats5$ss5seewellst<-ifelse(nhats5$ss5seewellst<=-1, NA, nhats5$ss5seewellst) 
table(nhats5$ss5seewellst, useNA="always")
nhats6$ss6seewellst<-ifelse(nhats6$ss6seewellst<=-1, NA, nhats6$ss6seewellst) 
table(nhats6$ss6seewellst, useNA="always")
nhats7$ss7seewellst<-ifelse(nhats7$ss7seewellst<=-1, NA, nhats7$ss7seewellst)
table(nhats7$ss7seewellst, useNA="always")
nhats8$ss8seewellst<-ifelse(nhats8$ss8seewellst<=-1, NA, nhats8$ss8seewellst) 
table(nhats8$ss8seewellst, useNA="always")
nhats9$ss9seewellst<-ifelse(nhats9$ss9seewellst<=-1, NA, nhats9$ss9seewellst) 
table(nhats9$ss9seewellst, useNA="always")
nhats10$ss10seewellst<-ifelse(nhats10$ss10seewellst<=-1, NA, nhats10$ss10seewellst) 
table(nhats10$ss10seewellst, useNA="always")
nhats11$ss11seewellst<-ifelse(nhats11$ss11seewellst<=-1, NA, nhats11$ss11seewellst) 
table(nhats11$ss11seewellst, useNA="always")
nhats12$ss12seewellst<-ifelse(nhats12$ss12seewellst<=-1, NA, nhats12$ss12seewellst) 
table(nhats12$ss12seewellst, useNA="always")

#distance  
nhats1$ss1seestvgls<-ifelse(nhats1$ss1seestvgls<=-1, NA, nhats1$ss1seestvgls) 
table(nhats1$ss1seestvgls, useNA="always")
nhats2$ss2seestvgls<-ifelse(nhats2$ss2seestvgls<=-1, NA, nhats2$ss2seestvgls) 
table(nhats2$ss2seestvgls, useNA="always")
nhats3$ss3seestvgls<-ifelse(nhats3$ss3seestvgls<=-1, NA, nhats3$ss3seestvgls) 
table(nhats3$ss3seestvgls, useNA="always")
nhats4$ss4seestvgls<-ifelse(nhats4$ss4seestvgls<=-1, NA, nhats4$ss4seestvgls) 
table(nhats4$ss4seestvgls, useNA="always")
nhats5$ss5seestvgls<-ifelse(nhats5$ss5seestvgls<=-1, NA, nhats5$ss5seestvgls) 
table(nhats5$ss5seestvgls, useNA="always")
nhats6$ss6seestvgls<-ifelse(nhats6$ss6seestvgls<=-1, NA, nhats6$ss6seestvgls) 
table(nhats6$ss6seestvgls, useNA="always")
nhats7$ss7seestvgls<-ifelse(nhats7$ss7seestvgls<=-1, NA, nhats7$ss7seestvgls)
table(nhats7$ss7seestvgls, useNA="always")
nhats8$ss8seestvgls<-ifelse(nhats8$ss8seestvgls<=-1, NA, nhats8$ss8seestvgls) 
table(nhats8$ss8seestvgls, useNA="always")
nhats9$ss9seestvgls<-ifelse(nhats9$ss9seestvgls<=-1, NA, nhats9$ss9seestvgls) 
table(nhats9$ss9seestvgls, useNA="always")
nhats10$ss10seestvgls<-ifelse(nhats10$ss10seestvgls<=-1, NA, nhats10$ss10seestvgls) 
table(nhats10$ss10seestvgls, useNA="always")
nhats11$ss11seestvgls<-ifelse(nhats11$ss11seestvgls<=-1, NA, nhats11$ss11seestvgls) 
table(nhats11$ss11seestvgls, useNA="always")
nhats12$ss12seestvgls<-ifelse(nhats12$ss12seestvgls<=-1, NA, nhats12$ss12seestvgls) 
table(nhats12$ss12seestvgls, useNA="always")

#near  
nhats1$ss1glrednewp<-ifelse(nhats1$ss1glrednewp<=-1, NA, nhats1$ss1glrednewp) 
table(nhats1$ss1glrednewp, useNA="always")
nhats2$ss2glrednewp<-ifelse(nhats2$ss2glrednewp<=-1, NA, nhats2$ss2glrednewp) 
table(nhats2$ss2glrednewp, useNA="always")
nhats3$ss3glrednewp<-ifelse(nhats3$ss3glrednewp<=-1, NA, nhats3$ss3glrednewp) 
table(nhats3$ss3glrednewp, useNA="always")
nhats4$ss4glrednewp<-ifelse(nhats4$ss4glrednewp<=-1, NA, nhats4$ss4glrednewp) 
table(nhats4$ss4glrednewp, useNA="always")
nhats5$ss5glrednewp<-ifelse(nhats5$ss5glrednewp<=-1, NA, nhats5$ss5glrednewp) 
table(nhats5$ss5glrednewp, useNA="always")
nhats6$ss6glrednewp<-ifelse(nhats6$ss6glrednewp<=-1, NA, nhats6$ss6glrednewp) 
table(nhats6$ss6glrednewp, useNA="always")
nhats7$ss7glrednewp<-ifelse(nhats7$ss7glrednewp<=-1, NA, nhats7$ss7glrednewp)
table(nhats7$ss7glrednewp, useNA="always")
nhats8$ss8glrednewp<-ifelse(nhats8$ss8glrednewp<=-1, NA, nhats8$ss8glrednewp) 
table(nhats8$ss8glrednewp, useNA="always")
nhats9$ss9glrednewp<-ifelse(nhats9$ss9glrednewp<=-1, NA, nhats9$ss9glrednewp) 
table(nhats9$ss9glrednewp, useNA="always")
nhats10$ss10glrednewp<-ifelse(nhats10$ss10glrednewp<=-1, NA, nhats10$ss10glrednewp) 
table(nhats10$ss10glrednewp, useNA="always")
nhats11$ss11glrednewp<-ifelse(nhats11$ss11glrednewp<=-1, NA, nhats11$ss11glrednewp) 
table(nhats11$ss11glrednewp, useNA="always")
nhats12$ss12glrednewp<-ifelse(nhats12$ss12glrednewp<=-1, NA, nhats12$ss12glrednewp) 
table(nhats12$ss12glrednewp, useNA="always")

nhats1%>%
  mutate(dis_issues = as.numeric(case_when(ss1seewellst ==2 | ss1seestvgls ==2 |ss1glrednewp==2 ~ "1",
                                           (ss1seewellst ==1 | ss1seestvgls ==1 ) & ss1glrednewp==1 ~ "0")))->nhats1
table(nhats1$dis_issues, useNA = "always")

nhats2%>%
  mutate(dis_issues = as.numeric(case_when(ss2seewellst ==2 | ss2seestvgls ==2 |ss2glrednewp==2 ~ "1",
                                           (ss2seewellst ==1 | ss2seestvgls ==1 ) & ss2glrednewp==1 ~ "0")))->nhats2
table(nhats2$dis_issues, useNA = "always")
nhats3%>%
  mutate(dis_issues = as.numeric(case_when(ss3seewellst ==2 | ss3seestvgls ==2 |ss3glrednewp==2 ~ "1",
                                           (ss3seewellst ==1 | ss3seestvgls ==1 ) & ss3glrednewp==1 ~ "0")))->nhats3
table(nhats3$dis_issues, useNA = "always")
nhats4%>%
  mutate(dis_issues = as.numeric(case_when(ss4seewellst ==2 | ss4seestvgls ==2 |ss4glrednewp==2 ~ "1",
                                           (ss4seewellst ==1 | ss4seestvgls ==1 ) & ss4glrednewp==1 ~ "0")))->nhats4
table(nhats4$dis_issues, useNA = "always")
nhats5%>%
  mutate(dis_issues = as.numeric(case_when(ss5seewellst ==2 | ss5seestvgls ==2 |ss5glrednewp==2 ~ "1",
                                           (ss5seewellst ==1 | ss5seestvgls ==1 ) & ss5glrednewp==1 ~ "0")))->nhats5
table(nhats5$dis_issues, useNA = "always")
nhats6%>%
  mutate(dis_issues = as.numeric(case_when(ss6seewellst ==2 | ss6seestvgls ==2 |ss6glrednewp==2 ~ "1",
                                           (ss6seewellst ==1 | ss6seestvgls ==1 ) & ss6glrednewp==1 ~ "0")))->nhats6
table(nhats6$dis_issues, useNA = "always")
nhats7%>%
  mutate(dis_issues = as.numeric(case_when(ss7seewellst ==2 | ss7seestvgls ==2 |ss7glrednewp==2 ~ "1",
                                           (ss7seewellst ==1 | ss7seestvgls ==1 ) & ss7glrednewp==1 ~ "0")))->nhats7
table(nhats7$dis_issues, useNA = "always")
nhats8%>%
  mutate(dis_issues = as.numeric(case_when(ss8seewellst ==2 | ss8seestvgls ==2 |ss8glrednewp==2 ~ "1",
                                           (ss8seewellst ==1 | ss8seestvgls ==1 ) & ss8glrednewp==1 ~ "0")))->nhats8
table(nhats8$dis_issues, useNA = "always")

nhats9%>%
  mutate(dis_issues = as.numeric(case_when(ss9seewellst ==2 | ss9seestvgls ==2 |ss9glrednewp==2 ~ "1",
                                           (ss9seewellst ==1 | ss9seestvgls ==1 ) & ss9glrednewp==1 ~ "0")))->nhats9
table(nhats9$dis_issues, useNA = "always")

nhats10%>%
  mutate(dis_issues = as.numeric(case_when(ss10seewellst ==2 | ss10seestvgls ==2 |ss10glrednewp==2 ~ "1",
                                           (ss10seewellst ==1 | ss10seestvgls ==1 ) & ss10glrednewp==1 ~ "0")))->nhats10
table(nhats10$dis_issues, useNA = "always")

nhats11%>%
  mutate(dis_issues = as.numeric(case_when(ss11seewellst ==2 | ss11seestvgls ==2 |ss11glrednewp==2 ~ "1",
                                           (ss11seewellst ==1 | ss11seestvgls ==1 ) & ss11glrednewp==1 ~ "0")))->nhats11
table(nhats11$dis_issues, useNA = "always")

nhats12%>%
  mutate(dis_issues = as.numeric(case_when(ss12seewellst ==2 | ss12seestvgls ==2 |ss12glrednewp==2 ~ "1",
                                           (ss12seewellst ==1 | ss12seestvgls ==1 ) & ss12glrednewp==1 ~ "0")))->nhats12
table(nhats12$dis_issues, useNA = "always")

# ###employment status ####
nhats1$emp<-ifelse(nhats1$pa1workfrpay<=-1, NA, ifelse(nhats1$pa1workfrpay==1,1,0) )
nhats2$emp<-ifelse(nhats2$pa2workfrpay<=-1, NA, ifelse(nhats2$pa2workfrpay==1,1,0) )
nhats3$emp<-ifelse(nhats3$pa3workfrpay<=-1, NA, ifelse(nhats3$pa3workfrpay==1,1,0) )
nhats4$emp<-ifelse(nhats4$pa4workfrpay<=-1, NA, ifelse(nhats4$pa4workfrpay==1,1,0) )
nhats5$emp<-ifelse(nhats5$pa5workfrpay<=-1, NA, ifelse(nhats5$pa5workfrpay==1,1,0) )
nhats6$emp<-ifelse(nhats6$pa6workfrpay<=-1, NA, ifelse(nhats6$pa6workfrpay==1,1,0) )
nhats7$emp<-ifelse(nhats7$pa7workfrpay<=-1, NA, ifelse(nhats7$pa7workfrpay==1,1,0) )
nhats8$emp<-ifelse(nhats8$pa8workfrpay<=-1, NA, ifelse(nhats8$pa8workfrpay==1,1,0) )
nhats9$emp<-ifelse(nhats9$pa9workfrpay<=-1, NA, ifelse(nhats9$pa9workfrpay==1,1,0) )
nhats10$emp<-ifelse(nhats10$pa10workfrpay<=-1, NA, ifelse(nhats10$pa10workfrpay==1,1,0) )
nhats11$emp<-ifelse(nhats11$pa11workfrpay<=-1, NA, ifelse(nhats11$pa11workfrpay==1,1,0) )
nhats12$emp<-ifelse(nhats12$pa12workfrpay<=-1, NA, ifelse(nhats12$pa12workfrpay==1,1,0) )


####pain it is coded as 1 yes 2 no so results are positive is assoicated with more sppb - code reversed####
nhats1$ss1painbothr<-ifelse(nhats1$ss1painbothr<=-1, NA, nhats1$ss1painbothr) 
nhats2$ss2painbothr<-ifelse(nhats2$ss2painbothr<=-1, NA, nhats2$ss2painbothr) 
nhats3$ss3painbothr<-ifelse(nhats3$ss3painbothr<=-1, NA, nhats3$ss3painbothr) 
nhats4$ss4painbothr<-ifelse(nhats4$ss4painbothr<=-1, NA, nhats4$ss4painbothr) 
nhats5$ss5painbothr<-ifelse(nhats5$ss5painbothr<=-1, NA, nhats5$ss5painbothr) 
nhats6$ss6painbothr<-ifelse(nhats6$ss6painbothr<=-1, NA, nhats6$ss6painbothr) 
nhats7$ss7painbothr<-ifelse(nhats7$ss7painbothr<=-1, NA, nhats7$ss7painbothr) 
nhats8$ss8painbothr<-ifelse(nhats8$ss8painbothr<=-1, NA, nhats8$ss8painbothr) 
nhats9$ss9painbothr<-ifelse(nhats9$ss9painbothr<=-1, NA, nhats9$ss9painbothr) 
nhats10$ss10painbothr<-ifelse(nhats10$ss10painbothr<=-1, NA, nhats10$ss10painbothr) 
nhats11$ss11painbothr<-ifelse(nhats11$ss11painbothr<=-1, NA, nhats11$ss11painbothr) 
nhats12$ss12painbothr<-ifelse(nhats12$ss12painbothr<=-1, NA, nhats12$ss12painbothr) 


nhats1%>%
  mutate(pain = as.numeric(case_when(ss1painbothr ==1  ~ "1",
                                     ss1painbothr ==2  ~ "0")))->nhats1
table(nhats1$pain)

nhats2%>%
  mutate(pain = as.numeric(case_when(ss2painbothr ==1  ~ "1",
                                     ss2painbothr ==2  ~ "0")))->nhats2
table(nhats2$pain)
nhats3%>%
  mutate(pain = as.numeric(case_when(ss3painbothr ==1  ~ "1",
                                     ss3painbothr ==2  ~ "0")))->nhats3
table(nhats3$pain)
nhats4%>%
  mutate(pain = as.numeric(case_when(ss4painbothr ==1  ~ "1",
                                     ss4painbothr ==2  ~ "0")))->nhats4
table(nhats4$pain)
nhats5%>%
  mutate(pain = as.numeric(case_when(ss5painbothr ==1  ~ "1",
                                     ss5painbothr ==2  ~ "0")))->nhats5
table(nhats5$pain)
nhats6%>%
  mutate(pain = as.numeric(case_when(ss6painbothr ==1  ~ "1",
                                     ss6painbothr ==2  ~ "0")))->nhats6
table(nhats6$pain)
nhats7%>%
  mutate(pain = as.numeric(case_when(ss7painbothr ==1  ~ "1",
                                     ss7painbothr ==2  ~ "0")))->nhats7
table(nhats7$pain)
nhats8%>%
  mutate(pain = as.numeric(case_when(ss8painbothr ==1  ~ "1",
                                     ss8painbothr ==2  ~ "0")))->nhats8
table(nhats8$pain)
nhats9%>%
  mutate(pain = as.numeric(case_when(ss9painbothr ==1  ~ "1",
                                     ss9painbothr ==2  ~ "0")))->nhats9
table(nhats9$pain)

nhats10%>%
  mutate(pain = as.numeric(case_when(ss10painbothr ==1  ~ "1",
                                     ss10painbothr ==2  ~ "0")))->nhats10
table(nhats10$pain)

nhats11%>%
  mutate(pain = as.numeric(case_when(ss11painbothr ==1  ~ "1",
                                     ss11painbothr ==2  ~ "0")))->nhats11
table(nhats11$pain)

nhats12%>%
  mutate(pain = as.numeric(case_when(ss12painbothr ==1  ~ "1",
                                     ss12painbothr ==2  ~ "0")))->nhats12
table(nhats12$pain)

####exercise and physical activity####

nhats1$pa1evrgowalk<-ifelse(nhats1$pa1evrgowalk<=-1, NA, nhats1$pa1evrgowalk) 
nhats2$pa2evrgowalk<-ifelse(nhats2$pa2evrgowalk<=-1, NA, nhats2$pa2evrgowalk) 
nhats3$pa3evrgowalk<-ifelse(nhats3$pa3evrgowalk<=-1, NA, nhats3$pa3evrgowalk) 
nhats4$pa4evrgowalk<-ifelse(nhats4$pa4evrgowalk<=-1, NA, nhats4$pa4evrgowalk) 
nhats5$pa5evrgowalk<-ifelse(nhats5$pa5evrgowalk<=-1, NA, nhats5$pa5evrgowalk) 
nhats6$pa6evrgowalk<-ifelse(nhats6$pa6evrgowalk<=-1, NA, nhats6$pa6evrgowalk) 
nhats7$pa7evrgowalk<-ifelse(nhats7$pa7evrgowalk<=-1, NA, nhats7$pa7evrgowalk) 
nhats8$pa8evrgowalk<-ifelse(nhats8$pa8evrgowalk<=-1, NA, nhats8$pa8evrgowalk) 
nhats9$pa9evrgowalk<-ifelse(nhats9$pa9evrgowalk<=-1, NA, nhats9$pa9evrgowalk) 
nhats10$pa10evrgowalk<-ifelse(nhats10$pa10evrgowalk<=-1, NA, nhats10$pa10evrgowalk) 
nhats11$pa11evrgowalk<-ifelse(nhats11$pa11evrgowalk<=-1, NA, nhats11$pa11evrgowalk) 
nhats12$pa12evrgowalk<-ifelse(nhats12$pa12evrgowalk<=-1, NA, nhats12$pa12evrgowalk) 

nhats1$pa1vigoractv<-ifelse(nhats1$pa1vigoractv<=-1, NA, nhats1$pa1vigoractv) 
nhats2$pa2vigoractv<-ifelse(nhats2$pa2vigoractv<=-1, NA, nhats2$pa2vigoractv) 
nhats3$pa3vigoractv<-ifelse(nhats3$pa3vigoractv<=-1, NA, nhats3$pa3vigoractv) 
nhats4$pa4vigoractv<-ifelse(nhats4$pa4vigoractv<=-1, NA, nhats4$pa4vigoractv) 
nhats5$pa5vigoractv<-ifelse(nhats5$pa5vigoractv<=-1, NA, nhats5$pa5vigoractv) 
nhats6$pa6vigoractv<-ifelse(nhats6$pa6vigoractv<=-1, NA, nhats6$pa6vigoractv) 
nhats7$pa7vigoractv<-ifelse(nhats7$pa7vigoractv<=-1, NA, nhats7$pa7vigoractv) 
nhats8$pa8vigoractv<-ifelse(nhats8$pa8vigoractv<=-1, NA, nhats8$pa8vigoractv) 
nhats9$pa9vigoractv<-ifelse(nhats9$pa9vigoractv<=-1, NA, nhats9$pa9vigoractv) 
nhats10$pa10vigoractv<-ifelse(nhats10$pa10vigoractv<=-1, NA, nhats10$pa10vigoractv) 
nhats11$pa11vigoractv<-ifelse(nhats11$pa11vigoractv<=-1, NA, nhats11$pa11vigoractv) 
nhats12$pa12vigoractv<-ifelse(nhats12$pa12vigoractv<=-1, NA, nhats12$pa12vigoractv) 


nhats1%>%
  mutate(exercise = as.numeric(case_when(pa1vigoractv ==1 | pa1evrgowalk==1 ~ "1",
                                         pa1vigoractv ==2 & pa1evrgowalk==2 ~ "0")))->nhats1
table(nhats1$exercise)
nhats2%>%
  mutate(exercise = as.numeric(case_when(pa2vigoractv ==1 | pa2evrgowalk==1 ~ "1",
                                         pa2vigoractv ==2 & pa2evrgowalk==2 ~ "0")))->nhats2
table(nhats2$exercise)
nhats3%>%
  mutate(exercise = as.numeric(case_when(pa3vigoractv ==1 | pa3evrgowalk==1 ~ "1",
                                         pa3vigoractv ==2 & pa3evrgowalk==2 ~ "0")))->nhats3
table(nhats3$exercise)
nhats4%>%
  mutate(exercise = as.numeric(case_when(pa4vigoractv ==1 | pa4evrgowalk==1 ~ "1",
                                         pa4vigoractv ==2 & pa4evrgowalk==2 ~ "0")))->nhats4
table(nhats4$exercise)
nhats5%>%
  mutate(exercise = as.numeric(case_when(pa5vigoractv ==1 | pa5evrgowalk==1 ~ "1",
                                         pa5vigoractv ==2 & pa5evrgowalk==2 ~ "0")))->nhats5
table(nhats5$exercise)
nhats6%>%
  mutate(exercise = as.numeric(case_when(pa6vigoractv ==1 | pa6evrgowalk==1 ~ "1",
                                         pa6vigoractv ==2 & pa6evrgowalk==2 ~ "0")))->nhats6
table(nhats6$exercise)
nhats7%>%
  mutate(exercise = as.numeric(case_when(pa7vigoractv ==1 | pa7evrgowalk==1 ~ "1",
                                         pa7vigoractv ==2 & pa7evrgowalk==2 ~ "0")))->nhats7
table(nhats7$exercise)
nhats8%>%
  mutate(exercise = as.numeric(case_when(pa8vigoractv ==1 | pa8evrgowalk==1 ~ "1",
                                         pa8vigoractv ==2 & pa8evrgowalk==2 ~ "0")))->nhats8
table(nhats8$exercise)
nhats9%>%
  mutate(exercise = as.numeric(case_when(pa9vigoractv ==1 | pa9evrgowalk==1 ~ "1",
                                         pa9vigoractv ==2 & pa9evrgowalk==2 ~ "0")))->nhats9
table(nhats9$exercise)

nhats10%>%
  mutate(exercise = as.numeric(case_when(pa10vigoractv ==1 | pa10evrgowalk==1 ~ "1",
                                         pa10vigoractv ==2 & pa10evrgowalk==2 ~ "0")))->nhats10
table(nhats10$exercise)

nhats11%>%
  mutate(exercise = as.numeric(case_when(pa11vigoractv ==1 | pa11evrgowalk==1 ~ "1",
                                         pa11vigoractv ==2 & pa11evrgowalk==2 ~ "0")))->nhats11
table(nhats11$exercise)

nhats12%>%
  mutate(exercise = as.numeric(case_when(pa12vigoractv ==1 | pa12evrgowalk==1 ~ "1",
                                         pa12vigoractv ==2 & pa12evrgowalk==2 ~ "0")))->nhats12
table(nhats12$exercise)

####smoking####
nhats1$sd1smokedreg
table(nhats1$sd1smokedreg)
nhats1%>%
  mutate(smk_before1 = as.numeric(case_when(sd1smokedreg ==1  ~ "1",
                                            sd1smokedreg ==2  ~ "0",
                                            sd1smokedreg <=-1~ "NA")))->nhats1

table(nhats1$smk_before)
nhats1$sd1smokesnow
table(nhats1$sd1smokesnow)
nhats1%>%
  mutate(smk_now1 = as.numeric(case_when(sd1smokesnow ==1 ~ "1",
                                         sd1smokesnow ==2  ~ "0",
                                         sd1smokesnow <=-1~ "NA")))->nhats1
table(nhats1$smk_now)


nhats2%>%
  mutate(smk_now2 = as.numeric(case_when(sd2smokesnow ==1 ~ "1",
                                         sd2smokesnow ==2  ~ "0",
                                         sd2smokesnow <=-1~ "NA")))->nhats2
nhats2[,"smk_before2"] <- NA
nhats3%>%
  mutate(smk_now3 = as.numeric(case_when(sd3smokesnow ==1 ~ "1",
                                         sd3smokesnow ==2  ~ "0",
                                         sd3smokesnow <=-1~ "NA")))->nhats3
nhats3[,"smk_before3"] <- NA
nhats4%>%
  mutate(smk_now4 = as.numeric(case_when(sd4smokesnow ==1 ~ "1",
                                         sd4smokesnow ==2  ~ "0",
                                         sd4smokesnow <=-1~ "NA")))->nhats4
nhats4[,"smk_before4"] <- NA
nhats5%>%
  mutate(smk_now5 = as.numeric(case_when(sd5smokesnow ==1 ~ "1",
                                         sd5smokesnow ==2  ~ "0",
                                         sd5smokesnow <=-1~ "NA")))->nhats5
nhats5[,"smk_before5"] <- NA
nhats6%>%
  mutate(smk_now6 = as.numeric(case_when(sd6smokesnow ==1 ~ "1",
                                         sd6smokesnow ==2  ~ "0",
                                         sd6smokesnow <=-1~ "NA")))->nhats6
nhats6[,"smk_before6"] <- NA

nhats7%>%
  mutate(smk_now7 = as.numeric(case_when(sd7smokesnow ==1 ~ "1",
                                         sd7smokesnow ==2  ~ "0",
                                         sd7smokesnow <=-1~ "NA")))->nhats7
nhats7[,"smk_before7"] <- NA

nhats8%>%
  mutate(smk_now8 = as.numeric(case_when(sd8smokesnow ==1 ~ "1",
                                         sd8smokesnow ==2  ~ "0",
                                         sd8smokesnow <=-1~ "NA")))->nhats8
nhats8[,"smk_before8"] <- NA
nhats9%>%
  mutate(smk_now9 = as.numeric(case_when(sd9smokesnow ==1 ~ "1",
                                         sd9smokesnow ==2  ~ "0",
                                         sd9smokesnow <=-1~ "NA")))->nhats9
nhats9[,"smk_before9"] <- NA

nhats10%>%
  mutate(smk_now10 = as.numeric(case_when(sd10smokesnow ==1 ~ "1",
                                          sd10smokesnow ==2  ~ "0",
                                          sd10smokesnow <=-1~ "NA")))->nhats10
nhats10[,"smk_before10"] <- NA

nhats11%>%
  mutate(smk_now11 = as.numeric(case_when(sd11smokesnow ==1 ~ "1",
                                          sd11smokesnow ==2  ~ "0",
                                          sd11smokesnow <=-1~ "NA")))->nhats11
nhats11[,"smk_before11"] <- NA

nhats12%>%
  mutate(smk_now12 = as.numeric(case_when(sd12smokesnow ==1 ~ "1",
                                          sd12smokesnow ==2  ~ "0",
                                          sd12smokesnow <=-1~ "NA")))->nhats12
nhats12[,"smk_before12"] <- NA

####mortality status ####
nhats1[,"mort_status"] <- 0
nhats2%>%
  mutate(mort_status= ifelse(fl2spdied==-1,0,1))->nhats2
nhats3%>%
  mutate(mort_status= ifelse(fl3spdied==-1,0,1))->nhats3
nhats2%>%
  mutate(mort_status= ifelse(fl2spdied==-1,0,1))->nhats2
nhats4%>%
  mutate(mort_status= ifelse(fl4spdied==-1,0,1))->nhats4
nhats5%>%
  mutate(mort_status= ifelse(fl5spdied==-1,0,1))->nhats5
nhats6%>%
  mutate(mort_status= ifelse(fl6spdied==-1,0,1))->nhats6
nhats7%>%
  mutate(mort_status= ifelse(fl7spdied==-1,0,1))->nhats7
nhats8%>%
  mutate(mort_status= ifelse(fl8spdied==-1,0,1))->nhats8
nhats9%>%
  mutate(mort_status= ifelse(fl9spdied==-1,0,1))->nhats9
nhats10%>%
  mutate(mort_status= ifelse(fl10spdied==-1,0,1))->nhats10
nhats11%>%
  mutate(mort_status= ifelse(fl11spdied==-1,0,1))->nhats11
nhats12%>%
  mutate(mort_status= ifelse(fl12spdied==-1,0,1))->nhats12

####hospital#### 
nhats1%>%
  mutate(hosp1 = as.numeric(case_when(hc1hosptstay ==1 ~ "1",
                                      hc1hosptstay ==2  ~ "0",
                                      hc1hosptstay <=-1~ "NA")))->nhats1
table(nhats1$hosp1)

nhats2%>%
  mutate(hosp2 = as.numeric(case_when(hc2hosptstay ==1 ~ "1",
                                      hc2hosptstay ==2  ~ "0",
                                      hc2hosptstay <=-1~ "NA")))->nhats2
table(nhats2$hosp2)

nhats3%>%
  mutate(hosp3 = as.numeric(case_when(hc3hosptstay ==1 ~ "1",
                                      hc3hosptstay ==2  ~ "0",
                                      hc3hosptstay <=-1~ "NA")))->nhats3
table(nhats3$hosp3)

nhats4%>%
  mutate(hosp4 = as.numeric(case_when(hc4hosptstay ==1 ~ "1",
                                      hc4hosptstay ==2  ~ "0",
                                      hc4hosptstay <=-1~ "NA")))->nhats4
table(nhats4$hosp4)

nhats5%>%
  mutate(hosp5 = as.numeric(case_when(hc5hosptstay ==1 ~ "1",
                                      hc5hosptstay ==2  ~ "0",
                                      hc5hosptstay <=-1~ "NA")))->nhats5
table(nhats5$hosp5)

nhats6%>%
  mutate(hosp6 = as.numeric(case_when(hc6hosptstay ==1 ~ "1",
                                      hc6hosptstay ==2  ~ "0",
                                      hc6hosptstay <=-1~ "NA")))->nhats6
table(nhats6$hosp6)

nhats7%>%
  mutate(hosp7 = as.numeric(case_when(hc7hosptstay ==1 ~ "1",
                                      hc7hosptstay ==2  ~ "0",
                                      hc7hosptstay <=-1~ "NA")))->nhats7
table(nhats7$hosp7)

nhats8%>%
  mutate(hosp8 = as.numeric(case_when(hc8hosptstay ==1 ~ "1",
                                      hc8hosptstay ==2  ~ "0",
                                      hc8hosptstay <=-1~ "NA")))->nhats8
table(nhats8$hosp8)

nhats9%>%
  mutate(hosp9 = as.numeric(case_when(hc9hosptstay ==1 ~ "1",
                                      hc9hosptstay ==2  ~ "0",
                                      hc9hosptstay <=-1~ "NA")))->nhats9
table(nhats9$hosp9)

nhats10%>%
  mutate(hosp10 = as.numeric(case_when(hc10hosptstay ==1 ~ "1",
                                       hc10hosptstay ==2  ~ "0",
                                       hc10hosptstay <=-1~ "NA")))->nhats10
table(nhats10$hosp10)

nhats11%>%
  mutate(hosp11 = as.numeric(case_when(hc11hosptstay ==1 ~ "1",
                                       hc11hosptstay ==2  ~ "0",
                                       hc11hosptstay <=-1~ "NA")))->nhats11
table(nhats11$hosp11)

nhats12%>%
  mutate(hosp12 = as.numeric(case_when(hc12hosptstay ==1 ~ "1",
                                       hc12hosptstay ==2  ~ "0",
                                       hc12hosptstay <=-1~ "NA")))->nhats12
table(nhats12$hosp12)

####hospital number of stays####

table(nhats1$hc1hosovrnht)
table(nhats2$hc2hosovrnht)
table(nhats3$hc3hosovrnht)
table(nhats4$hc4hosovrnht)
table(nhats5$hc5hosovrnht)
table(nhats6$hc6hosovrnht)
table(nhats7$hc7hosovrnht)
table(nhats8$hc8hosovrnht)
table(nhats9$hc9hosovrnht)
table(nhats10$hc10hosovrnht)
table(nhats11$hc11hosovrnht)
table(nhats12$hc12hosovrnht)

nhats1%>%
  mutate(hosp_stay_n1 = as.numeric(case_when(hc1hosovrnht ==1 ~ "1",
                                             hc1hosovrnht ==2  ~ "2",
                                             hc1hosovrnht ==3  ~ "3",
                                             hc1hosovrnht ==4  ~ "4",
                                             hc1hosovrnht >=5  ~ "5",
                                             hc1hosptstay ==2  ~ "0",
                                             hc1hosovrnht <=-1~ "NA")))->nhats1
table(nhats1$hosp_stay_n1)

nhats2%>%
  mutate(hosp_stay_n2 = as.numeric(case_when(hc2hosovrnht ==1 ~ "1",
                                             hc2hosovrnht ==2  ~ "2",
                                             hc2hosovrnht ==3  ~ "3",
                                             hc2hosovrnht ==4  ~ "4",
                                             hc2hosovrnht >=5  ~ "5",
                                             hc2hosptstay ==2  ~ "0",
                                             hc2hosovrnht <=-1~ "NA")))->nhats2
table(nhats2$hosp_stay_n2)

nhats3%>%
  mutate(hosp_stay_n3 = as.numeric(case_when(hc3hosovrnht ==1 ~ "1",
                                             hc3hosovrnht ==2  ~ "2",
                                             hc3hosovrnht ==3  ~ "3",
                                             hc3hosovrnht ==4  ~ "4",
                                             hc3hosovrnht >=5  ~ "5",
                                             hc3hosptstay ==2  ~ "0",
                                             hc3hosovrnht <=-1~ "NA")))->nhats3
table(nhats3$hosp_stay_n3)

nhats4%>%
  mutate(hosp_stay_n4 = as.numeric(case_when(hc4hosovrnht ==1 ~ "1",
                                             hc4hosovrnht ==2  ~ "2",
                                             hc4hosovrnht ==3  ~ "3",
                                             hc4hosovrnht ==4  ~ "4",
                                             hc4hosovrnht >=5  ~ "5",
                                             hc4hosptstay ==2  ~ "0",
                                             hc4hosovrnht <=-1~ "NA")))->nhats4
table(nhats4$hosp_stay_n4)

nhats5%>%
  mutate(hosp_stay_n5 = as.numeric(case_when(hc5hosovrnht ==1 ~ "1",
                                             hc5hosovrnht ==2  ~ "2",
                                             hc5hosovrnht ==3  ~ "3",
                                             hc5hosovrnht ==4  ~ "4",
                                             hc5hosovrnht >=5  ~ "5",
                                             hc5hosptstay ==2  ~ "0",
                                             hc5hosovrnht <=-1~ "NA")))->nhats5
table(nhats6$hosp_stay_n6)

nhats6%>%
  mutate(hosp_stay_n6 = as.numeric(case_when(hc6hosovrnht ==1 ~ "1",
                                             hc6hosovrnht ==2  ~ "2",
                                             hc6hosovrnht ==3  ~ "3",
                                             hc6hosovrnht ==4  ~ "4",
                                             hc6hosovrnht >=5  ~ "5",
                                             hc6hosptstay ==2  ~ "0",
                                             hc6hosovrnht <=-1~ "NA")))->nhats6
table(nhats6$hosp_stay_n6)

nhats7%>%
  mutate(hosp_stay_n7 = as.numeric(case_when(hc7hosovrnht ==1 ~ "1",
                                             hc7hosovrnht ==2  ~ "2",
                                             hc7hosovrnht ==3  ~ "3",
                                             hc7hosovrnht ==4  ~ "4",
                                             hc7hosovrnht >=5  ~ "5",
                                             hc7hosptstay ==2  ~ "0",
                                             hc7hosovrnht <=-1~ "NA")))->nhats7
table(nhats7$hosp_stay_n7)

nhats8%>%
  mutate(hosp_stay_n8 = as.numeric(case_when(hc8hosovrnht ==1 ~ "1",
                                             hc8hosovrnht ==2  ~ "2",
                                             hc8hosovrnht ==3  ~ "3",
                                             hc8hosovrnht ==4  ~ "4",
                                             hc8hosovrnht >=5  ~ "5",
                                             hc8hosptstay ==2  ~ "0",
                                             hc8hosovrnht <=-1~ "NA")))->nhats8
table(nhats8$hosp_stay_n8)

nhats9%>%
  mutate(hosp_stay_n9 = as.numeric(case_when(hc9hosovrnht ==1 ~ "1",
                                             hc9hosovrnht ==2  ~ "2",
                                             hc9hosovrnht ==3  ~ "3",
                                             hc9hosovrnht ==4  ~ "4",
                                             hc9hosovrnht >=5  ~ "5",
                                             hc9hosptstay ==2  ~ "0",
                                             hc9hosovrnht <=-1~ "NA")))->nhats9
table(nhats9$hosp_stay_n9)

nhats10%>%
  mutate(hosp_stay_n10 = as.numeric(case_when(hc10hosovrnht ==1 ~ "1",
                                              hc10hosovrnht ==2  ~ "2",
                                              hc10hosovrnht ==3  ~ "3",
                                              hc10hosovrnht ==4  ~ "4",
                                              hc10hosovrnht >=5  ~ "5",
                                              hc10hosptstay ==2  ~ "0",
                                              hc10hosovrnht <=-1~ "NA")))->nhats10
table(nhats10$hosp_stay_n10)

nhats11%>%
  mutate(hosp_stay_n11 = as.numeric(case_when(hc11hosovrnht ==1 ~ "1",
                                              hc11hosovrnht ==2  ~ "2",
                                              hc11hosovrnht ==3  ~ "3",
                                              hc11hosovrnht ==4  ~ "4",
                                              hc11hosovrnht >=5  ~ "5",
                                              hc11hosptstay ==2  ~ "0",
                                              hc11hosovrnht <=-1~ "NA")))->nhats11
table(nhats11$hosp_stay_n11)

nhats12%>%
  mutate(hosp_stay_n12 = as.numeric(case_when(hc12hosovrnht ==1 ~ "1",
                                              hc12hosovrnht ==2  ~ "2",
                                              hc12hosovrnht ==3  ~ "3",
                                              hc12hosovrnht ==4  ~ "4",
                                              hc12hosovrnht >=5  ~ "5",
                                              hc12hosptstay ==2  ~ "0",
                                              hc12hosovrnht <=-1~ "NA")))->nhats12
table(nhats12$hosp_stay_n12)

####DEMENTIA#### 

#####Dementia Yes-No#### 
#Aqui cada dementia viene asociada a ronda, ej,dementia1. en health contitions
#hc9 es demencia pero no distingue rondas, siempre es hc9 en todas

nhats1%>%
  mutate(dementia1 = as.numeric(case_when(hc1disescn9 ==1  ~ "1",
                                          hc1disescn9 ==2 ~ "0",
                                          hc1disescn9 <=-1~ "NA")))->nhats1
table(nhats1$dementia1)

nhats2%>%
  mutate(dementia2 = as.numeric(case_when(hc2disescn9 ==1 | hc2disescn9 ==7 ~ "1",
                                          hc2disescn9 ==2  ~ "0",
                                          hc2disescn9 <=-1~ "NA")))->nhats2
table(nhats2$dementia2)

nhats3%>%
  mutate(dementia3 = as.numeric(case_when(hc3disescn9 ==1 | hc3disescn9 ==7 ~ "1",
                                          hc3disescn9 ==2  ~ "0",
                                          hc3disescn9 <=-1~ "NA")))->nhats3
table(nhats3$dementia3)

nhats4%>%
  mutate(dementia4 = as.numeric(case_when(hc4disescn9 ==1 | hc4disescn9 ==7 ~ "1",
                                          hc4disescn9 ==2  ~ "0",
                                          hc4disescn9 <=-1~ "NA")))->nhats4
table(nhats4$dementia4)

nhats5%>%
  mutate(dementia5 = as.numeric(case_when(hc5disescn9 ==1 | hc5disescn9 ==7 ~ "1",
                                          hc5disescn9 ==2  ~ "0",
                                          hc5disescn9 <=-1~ "NA")))->nhats5
table(nhats5$dementia5)

nhats6%>%
  mutate(dementia6 = as.numeric(case_when(hc6disescn9 ==1 | hc6disescn9 ==7 ~ "1",
                                          hc6disescn9 ==2  ~ "0",
                                          hc6disescn9 <=-1~ "NA")))->nhats6
table(nhats6$dementia6)

nhats7%>%
  mutate(dementia7 = as.numeric(case_when(hc7disescn9 ==1 | hc7disescn9 ==7 ~ "1",
                                          hc7disescn9 ==2  ~ "0",
                                          hc7disescn9 <=-1~ "NA")))->nhats7
table(nhats7$dementia7)

nhats8%>%
  mutate(dementia8 = as.numeric(case_when(hc8disescn9 ==1 | hc8disescn9 ==7 ~ "1",
                                          hc8disescn9 ==2  ~ "0",
                                          hc8disescn9 <=-1~ "NA")))->nhats8
table(nhats8$dementia8)

nhats9%>%
  mutate(dementia9 = as.numeric(case_when(hc9disescn9 ==1 | hc9disescn9 ==7 ~ "1",
                                          hc9disescn9 ==2  ~ "0",
                                          hc9disescn9 <=-1~ "NA")))->nhats9
table(nhats9$dementia9)

nhats10%>%
  mutate(dementia10 = as.numeric(case_when(hc10disescn9 ==1 | hc10disescn9 ==7 ~ "1",
                                           hc10disescn9 ==2  ~ "0",
                                           hc10disescn9 <=-1~ "NA")))->nhats10
table(nhats10$dementia10)

nhats11%>%
  mutate(dementia11 = as.numeric(case_when(hc11disescn9 ==1 | hc11disescn9 ==7 ~ "1",
                                           hc11disescn9 ==2  ~ "0",
                                           hc11disescn9 <=-1~ "NA")))->nhats11
table(nhats11$dementia11)

nhats12%>%
  mutate(dementia12 = as.numeric(case_when(hc12disescn9 ==1 | hc12disescn9 ==7 ~ "1",
                                           hc12disescn9 ==2  ~ "0",
                                           hc12disescn9 <=-1~ "NA")))->nhats12
table(nhats12$dementia12)

#####age#### 

nhats1$r1dintvwrage
nhats2$r2dintvwrage
nhats3$r3dintvwrage
nhats4$r4dintvwrage
nhats5$r5dintvwrage
nhats6$r6dintvwrage
nhats7$r7dintvwrage
nhats8$r8dintvwrage
nhats9$r9dintvwrage
nhats10$r10dintvwrage
nhats11$r11dintvwrage
nhats12$r12dintvwrage

#####Dementia age diagnosis r1 + r2-11 a partir de la edad en la que se hizo la entrevista#### 

nhats1$hc1dementage

nhats2%>%
  mutate(hc2dementage = case_when(dementia2 ==1 ~ r2dintvwrage))->nhats2

nhats3%>%
  mutate(hc3dementage = case_when(dementia3 ==1 ~ r3dintvwrage))->nhats3

nhats4%>%
  mutate(hc4dementage = case_when(dementia4 ==1 ~ r4dintvwrage))->nhats4

nhats5%>%
  mutate(hc5dementage = case_when(dementia5 ==1 ~ r5dintvwrage))->nhats5

nhats6%>%
  mutate(hc6dementage = case_when(dementia6 ==1 ~ r6dintvwrage))->nhats6

nhats7%>%
  mutate(hc7dementage = case_when(dementia7 ==1 ~ r7dintvwrage))->nhats7

nhats8%>%
  mutate(hc8dementage = case_when(dementia8 ==1 ~ r8dintvwrage))->nhats8

nhats9%>%
  mutate(hc9dementage = case_when(dementia9 ==1 ~ r9dintvwrage))->nhats9

nhats10%>%
  mutate(hc10dementage = case_when(dementia10 ==1 ~ r10dintvwrage))->nhats10

nhats11%>%
  mutate(hc11dementage = case_when(dementia11 ==1 ~ r11dintvwrage))->nhats11

nhats12%>%
  mutate(hc12dementage = case_when(dementia12 ==1 ~ r12dintvwrage))->nhats12

#unique(nhats2$hc2dementage)

#head(nhats2)

#mean(is.na(nhats2$hc2dementage))

#identical(nhats2$r2dintvwragwe[nhats2$dementia2 == 1], nhats2$hc2dementage)

#En las rondas 2 a 11 tras unir bases de datos. 
#mutate(hc2dementage = En caso de que aparezca un si en dementia2 = r2dintvwrage (esta variable esta en SP_extra)


####Died#### 

nhats1[,"mort_status"] <- 0
nhats2%>%
  mutate(mort_status= ifelse(fl2spdied==-1,0,1))->nhats2
nhats3%>%
  mutate(mort_status= ifelse(fl3spdied==-1,0,1))->nhats3
nhats2%>%
  mutate(mort_status= ifelse(fl2spdied==-1,0,1))->nhats2
nhats4%>%
  mutate(mort_status= ifelse(fl4spdied==-1,0,1))->nhats4
nhats5%>%
  mutate(mort_status= ifelse(fl5spdied==-1,0,1))->nhats5
nhats6%>%
  mutate(mort_status= ifelse(fl6spdied==-1,0,1))->nhats6
nhats7%>%
  mutate(mort_status= ifelse(fl7spdied==-1,0,1))->nhats7
nhats8%>%
  mutate(mort_status= ifelse(fl8spdied==-1,0,1))->nhats8
nhats9%>%
  mutate(mort_status= ifelse(fl9spdied==-1,0,1))->nhats9
nhats10%>%
  mutate(mort_status= ifelse(fl10spdied==-1,0,1))->nhats10
nhats11%>%
  mutate(mort_status= ifelse(fl11spdied==-1,0,1))->nhats11
nhats12%>%
  mutate(mort_status= ifelse(fl12spdied==-1,0,1))->nhats12


####Frailty Index####


######weak_grip_strength-r2dnhatsgrb######
#creo antes bmi de cada ronda

#Se necesita gender en todas las olas

data_merged <- merge(nhats1, nhats2, by = "spid")
nhats2$r2dgender <- data_merged$r1dgender

data_merged <- merge(nhats1, nhats3, by = "spid")
nhats3$r3dgender <- data_merged$r1dgender.x

data_merged <- merge(nhats1, nhats4, by = "spid")
nhats4$r4dgender <- data_merged$r1dgender.x

data_merged <- merge(nhats5, nhats6, by = "spid")
nhats6$r6dgender <- data_merged$r5dgender.x

data_merged <- merge(nhats5, nhats7, by = "spid")
nhats7$r7dgender <- data_merged$r5dgender.x

data_merged <- merge(nhats5, nhats8, by = "spid")
nhats8$r8dgender <- data_merged$r5dgender.x

data_merged <- merge(nhats5, nhats9, by = "spid")
nhats9$r9dgender <- data_merged$r5dgender.x

data_merged <- merge(nhats5, nhats10, by = "spid")
nhats10$r10dgender <- data_merged$r5dgender.x

data_merged <- merge(nhats5, nhats11, by = "spid")
nhats11$r11dgender <- data_merged$r5dgender.x



#removing -1 for heigh-feet

nhats1$hw1howtallft<-ifelse(nhats1$hw1howtallft<=-1, NA, nhats1$hw1howtallft) 
nhats2$hw2howtallft<-ifelse(nhats2$hw2howtallft<=-1, NA, nhats2$hw2howtallft) 
nhats3$hw3howtallft<-ifelse(nhats3$hw3howtallft<=-1, NA, nhats3$hw3howtallft) 
nhats4$hw4howtallft<-ifelse(nhats4$hw4howtallft<=-1, NA, nhats4$hw4howtallft) 
nhats5$hw5howtallft<-ifelse(nhats5$hw5howtallft<=-1, NA, nhats5$hw5howtallft) 
nhats6$hw6howtallft<-ifelse(nhats6$hw6howtallft<=-1, NA, nhats6$hw6howtallft) 
nhats7$hw7howtallft<-ifelse(nhats7$hw7howtallft<=-1, NA, nhats7$hw7howtallft) 
nhats8$hw8howtallft<-ifelse(nhats8$hw8howtallft<=-1, NA, nhats8$hw8howtallft) 
nhats9$hw9howtallft<-ifelse(nhats9$hw9howtallft<=-1, NA, nhats9$hw9howtallft) 
nhats10$hw10howtallft<-ifelse(nhats10$hw10howtallft<=-1, NA, nhats10$hw10howtallft) 
nhats11$hw11howtallft<-ifelse(nhats11$hw11howtallft<=-1, NA, nhats11$hw11howtallft) 
nhats12$hw12howtallft<-ifelse(nhats12$hw12howtallft<=-1, NA, nhats12$hw12howtallft) 

#removing -1 for heigh-inclhes
nhats1$hw1howtallin<-ifelse(nhats1$hw1howtallin<=-1, NA, nhats1$hw1howtallin) 
nhats2$hw2howtallin<-ifelse(nhats2$hw2howtallin<=-1, NA, nhats2$hw2howtallin) 
nhats3$hw3howtallin<-ifelse(nhats3$hw3howtallin<=-1, NA, nhats3$hw3howtallin) 
nhats4$hw4howtallin<-ifelse(nhats4$hw4howtallin<=-1, NA, nhats4$hw4howtallin) 
nhats5$hw5howtallin<-ifelse(nhats5$hw5howtallin<=-1, NA, nhats5$hw5howtallin) 
nhats6$hw6howtallin<-ifelse(nhats6$hw6howtallin<=-1, NA, nhats6$hw6howtallin) 
nhats7$hw7howtallin<-ifelse(nhats7$hw7howtallin<=-1, NA, nhats7$hw7howtallin) 
nhats8$hw8howtallin<-ifelse(nhats8$hw8howtallin<=-1, NA, nhats8$hw8howtallin) 
nhats9$hw9howtallin<-ifelse(nhats9$hw9howtallin<=-1, NA, nhats9$hw9howtallin) 
nhats10$hw10howtallin<-ifelse(nhats10$hw10howtallin<=-1, NA, nhats10$hw10howtallin) 
nhats11$hw11howtallin<-ifelse(nhats11$hw11howtallin<=-1, NA, nhats11$hw11howtallin) 
nhats12$hw12howtallin<-ifelse(nhats12$hw12howtallin<=-1, NA, nhats12$hw12howtallin) 

#calculate heigh in m
nhats1%>%
  mutate(altura1 = as.numeric(hw1howtallft*0.3048 + hw1howtallin * 0.0254)) -> nhats1

nhats2%>%
  mutate(altura2 = as.numeric(hw2howtallft*0.3048 + hw2howtallin * 0.0254)) -> nhats2

nhats3%>%
  mutate(altura3 = as.numeric(hw3howtallft*0.3048 + hw3howtallin * 0.0254)) -> nhats3

nhats4%>%
  mutate(altura4 = as.numeric(hw4howtallft*0.3048 + hw4howtallin * 0.0254)) -> nhats4

nhats5%>%
  mutate(altura5 = as.numeric(hw5howtallft*0.3048 + hw5howtallin * 0.0254)) -> nhats5

nhats6%>%
  mutate(altura6 = as.numeric(hw6howtallft*0.3048 + hw6howtallin * 0.0254)) -> nhats6

nhats7%>%
  mutate(altura7 = as.numeric(hw7howtallft*0.3048 + hw7howtallin * 0.0254)) -> nhats7

nhats8%>%
  mutate(altura8 = as.numeric(hw8howtallft*0.3048 + hw8howtallin * 0.0254)) -> nhats8

nhats9%>%
  mutate(altura9 = as.numeric(hw9howtallft*0.3048 + hw9howtallin * 0.0254)) -> nhats9

nhats10%>%
  mutate(altura10 = as.numeric(hw10howtallft*0.3048 + hw10howtallin * 0.0254)) -> nhats10

nhats11%>%
  mutate(altura11 = as.numeric(hw11howtallft*0.3048 + hw11howtallin * 0.0254)) -> nhats11

nhats12%>%
  mutate(altura12 = as.numeric(hw12howtallft*0.3048 + hw12howtallin * 0.0254)) -> nhats12

#removing -1 for weigh
nhats1$hw1currweigh<-ifelse(nhats1$hw1currweigh<=-1, NA, nhats1$hw1currweigh) 
nhats2$hw2currweigh<-ifelse(nhats2$hw2currweigh<=-1, NA, nhats2$hw2currweigh) 
nhats3$hw3currweigh<-ifelse(nhats3$hw3currweigh<=-1, NA, nhats3$hw3currweigh) 
nhats4$hw4currweigh<-ifelse(nhats4$hw4currweigh<=-1, NA, nhats4$hw4currweigh) 
nhats5$hw5currweigh<-ifelse(nhats5$hw5currweigh<=-1, NA, nhats5$hw5currweigh) 
nhats6$hw6currweigh<-ifelse(nhats6$hw6currweigh<=-1, NA, nhats6$hw6currweigh) 
nhats7$hw7currweigh<-ifelse(nhats7$hw7currweigh<=-1, NA, nhats7$hw7currweigh) 
nhats8$hw8currweigh<-ifelse(nhats8$hw8currweigh<=-1, NA, nhats8$hw8currweigh) 
nhats9$hw9currweigh<-ifelse(nhats9$hw9currweigh<=-1, NA, nhats9$hw9currweigh) 
nhats10$hw10currweigh<-ifelse(nhats10$hw10currweigh<=-1, NA, nhats10$hw10currweigh) 
nhats11$hw11currweigh<-ifelse(nhats11$hw11currweigh<=-1, NA, nhats11$hw11currweigh) 
nhats12$hw12currweigh<-ifelse(nhats12$hw12currweigh<=-1, NA, nhats12$hw12currweigh) 

nhats1%>%
  mutate(peso1 = as.numeric(hw1currweigh/2.205)) -> nhats1

nhats2%>%
  mutate(peso2 = as.numeric(hw2currweigh/2.205)) -> nhats2

nhats3%>%
  mutate(peso3 = as.numeric(hw3currweigh/2.205)) -> nhats3

nhats4%>%
  mutate(peso4 = as.numeric(hw4currweigh/2.205)) -> nhats4

nhats5%>%
  mutate(peso5 = as.numeric(hw5currweigh/2.205)) -> nhats5

nhats6%>%
  mutate(peso6 = as.numeric(hw6currweigh/2.205)) -> nhats6

nhats7%>%
  mutate(peso7 = as.numeric(hw7currweigh/2.205)) -> nhats7

nhats8%>%
  mutate(peso8 = as.numeric(hw8currweigh/2.205)) -> nhats8

nhats9%>%
  mutate(peso9 = as.numeric(hw9currweigh/2.205)) -> nhats9

nhats10%>%
  mutate(peso10 = as.numeric(hw10currweigh/2.205)) -> nhats10

nhats11%>%
  mutate(peso11 = as.numeric(hw11currweigh/2.205)) -> nhats11

nhats12%>%
  mutate(peso12 = as.numeric(hw12currweigh/2.205)) -> nhats12

#creo BMI

nhats1%>%
  mutate(bmi1 = as.numeric(peso1/altura1^2)) -> nhats1

nhats2%>%
  mutate(bmi2 = as.numeric(peso2/altura2^2)) -> nhats2

nhats3%>%
  mutate(bmi3 = as.numeric(peso3/altura3^2)) -> nhats3

nhats4%>%
  mutate(bmi4 = as.numeric(peso4/altura4^2)) -> nhats4

nhats5%>%
  mutate(bmi5 = as.numeric(peso5/altura5^2)) -> nhats5

nhats6%>%
  mutate(bmi6 = as.numeric(peso6/altura6^2)) -> nhats6

nhats7%>%
  mutate(bmi7 = as.numeric(peso7/altura7^2)) -> nhats7

nhats8%>%
  mutate(bmi8 = as.numeric(peso8/altura8^2)) -> nhats8

nhats9%>%
  mutate(bmi9 = as.numeric(peso9/altura9^2)) -> nhats9

nhats10%>%
  mutate(bmi10 = as.numeric(peso10/altura10^2)) -> nhats10

nhats11%>%
  mutate(bmi11 = as.numeric(peso11/altura11^2)) -> nhats11

nhats12%>%
  mutate(bmi12 = as.numeric(peso12/altura12^2)) -> nhats12

#mejor valor de handgrip

nhats1%>%
  mutate(handgrip1 = pmax(gr1grp1rdng, gr1grp2rdng)) -> nhats1

nhats2%>%
  mutate(handgrip2 = pmax(gr2grp1rdng, gr2grp2rdng)) -> nhats2

nhats3%>%
  mutate(handgrip3 = pmax(gr3grp1rdng, gr3grp2rdng)) -> nhats3

nhats4%>%
  mutate(handgrip4 = pmax(gr4grp1rdng, gr4grp2rdng)) -> nhats4

nhats5%>%
  mutate(handgrip5 = pmax(gr5grp1rdng, gr5grp2rdng)) -> nhats5

nhats6%>%
  mutate(handgrip6 = pmax(gr6grp1rdng, gr6grp2rdng)) -> nhats6

nhats7%>%
  mutate(handgrip7 = pmax(gr7grp1rdng, gr7grp2rdng)) -> nhats7

nhats8%>%
  mutate(handgrip8 = pmax(gr8grp1rdng, gr8grp2rdng)) -> nhats8

nhats9%>%
  mutate(handgrip9 = pmax(gr9grp1rdng, gr9grp2rdng)) -> nhats9

data_merged13 <- merge(nhats9, nhats10, by = "spid")
nhats10$handgrip10 <- data_merged13$handgrip9

nhats11%>%
  mutate(handgrip11 = pmax(gr11grp1rdng, gr11grp2rdng)) -> nhats11

nhats12%>%
  mutate(handgrip12 = pmax(gr12grp1rdng, gr12grp2rdng)) -> nhats12

#elimino -1 en handgrip

nhats1$handgrip1<-ifelse(nhats1$handgrip1<=-1, NA, nhats1$handgrip1) 
nhats2$handgrip2<-ifelse(nhats2$handgrip2<=-1, NA, nhats2$handgrip2) 
nhats3$handgrip3<-ifelse(nhats3$handgrip3<=-1, NA, nhats3$handgrip3) 
nhats4$handgrip4<-ifelse(nhats4$handgrip4<=-1, NA, nhats4$handgrip4) 
nhats5$handgrip5<-ifelse(nhats5$handgrip5<=-1, NA, nhats5$handgrip5) 
nhats6$handgrip6<-ifelse(nhats6$handgrip6<=-1, NA, nhats6$handgrip6) 
nhats7$handgrip7<-ifelse(nhats7$handgrip7<=-1, NA, nhats7$handgrip7) 
nhats8$handgrip8<-ifelse(nhats8$handgrip8<=-1, NA, nhats8$handgrip8) 
nhats9$handgrip9<-ifelse(nhats9$handgrip9<=-1, NA, nhats9$handgrip9) 
nhats10$handgrip10<-ifelse(nhats10$handgrip10<=-1, NA, nhats10$handgrip10) 
nhats11$handgrip11<-ifelse(nhats11$handgrip11<=-1, NA, nhats11$handgrip11) 
nhats12$handgrip12<-ifelse(nhats12$handgrip12<=-1, NA, nhats12$handgrip12) 


#weak_strength
nhats1%>%
  mutate(weak_strength1 = as.numeric(case_when(r1dgender ==1 & bmi1 <= 24 & handgrip1 <= 23.2 ~ "1",
                                               r1dgender ==1 & bmi1 <= 24 & handgrip1 > 23.2 ~ "0",
                                               r1dgender ==1 &  bmi1 >= 24.1 & bmi1 <= 28 & handgrip1 <= 24 ~ "1",
                                               r1dgender ==1 &  bmi1 >= 24.1 & bmi1 <= 28 & handgrip1 > 24 ~ "0",
                                               r1dgender ==1 & bmi1 > 28 & handgrip1 <= 25.6 ~ "1",
                                               r1dgender ==1 & bmi1 > 28 & handgrip1 > 25.6 ~ "0",
                                               r1dgender ==2 & bmi1 <= 23 & handgrip1 <= 13.6 ~ "1",
                                               r1dgender ==2 & bmi1 <= 23 & handgrip1 > 13.6 ~ "0",
                                               r1dgender ==2 &  bmi1 >= 23.1 & bmi1 <= 26 & handgrip1 <= 13.84 ~ "1",
                                               r1dgender ==2 &  bmi1 >= 23.1 & bmi1 <= 26 & handgrip1 > 13.84 ~ "0",
                                               r1dgender ==2 &  bmi1 >= 26.1 & bmi1 <= 29 & handgrip1 <= 14.4 ~ "1",
                                               r1dgender ==2 &  bmi1 >= 26.1 & bmi1 <= 29 & handgrip1 > 14.4 ~ "0",
                                               r1dgender ==2 & bmi1 > 29 & handgrip1 <= 16.8 ~ "1",
                                               r1dgender ==2 & bmi1 > 29 & handgrip1 > 16.8 ~ "0")))-> nhats1

table(nhats1$weak_strength1)

nhats2%>%
  mutate(weak_strength2 = as.numeric(case_when(r2dgender ==1 & bmi2 <= 24 & handgrip2 <= 23.2 ~ "1",
                                               r2dgender ==1 & bmi2 <= 24 & handgrip2 > 23.2 ~ "0",
                                               r2dgender ==1 &  bmi2 >= 24.1 & bmi2 <= 28 & handgrip2 <= 24 ~ "1",
                                               r2dgender ==1 &  bmi2 >= 24.1 & bmi2 <= 28 & handgrip2 > 24 ~ "0",
                                               r2dgender ==1 & bmi2 > 28 & handgrip2 <= 25.6 ~ "1",
                                               r2dgender ==1 & bmi2 > 28 & handgrip2 > 25.6 ~ "0",
                                               r2dgender ==2 & bmi2 <= 23 & handgrip2 <= 13.6 ~ "1",
                                               r2dgender ==2 & bmi2 <= 23 & handgrip2 > 13.6 ~ "0",
                                               r2dgender ==2 &  bmi2 >= 23.1 & bmi2 <= 26 & handgrip2 <= 13.84 ~ "1",
                                               r2dgender ==2 &  bmi2 >= 23.1 & bmi2 <= 26 & handgrip2 > 13.84 ~ "0",
                                               r2dgender ==2 &  bmi2 >= 26.1 & bmi2 <= 29 & handgrip2 <= 14.4 ~ "1",
                                               r2dgender ==2 &  bmi2 >= 26.1 & bmi2 <= 29 & handgrip2 > 14.4 ~ "0",
                                               r2dgender ==2 & bmi2 > 29 & handgrip2 <= 16.8 ~ "1",
                                               r2dgender ==2 & bmi2 > 29 & handgrip2 > 16.8 ~ "0")))-> nhats2

table(nhats2$weak_strength2)

nhats3%>%
  mutate(weak_strength3 = as.numeric(case_when(r3dgender ==1 & bmi3 <= 24 & handgrip3 <= 23.2 ~ "1",
                                               r3dgender ==1 & bmi3 <= 24 & handgrip3 > 23.2 ~ "0",
                                               r3dgender ==1 &  bmi3 >= 24.1 & bmi3 <= 28 & handgrip3 <= 24 ~ "1",
                                               r3dgender ==1 &  bmi3 >= 24.1 & bmi3 <= 28 & handgrip3 > 24 ~ "0",
                                               r3dgender ==1 & bmi3 > 28 & handgrip3 <= 25.6 ~ "1",
                                               r3dgender ==1 & bmi3 > 28 & handgrip3 > 25.6 ~ "0",
                                               r3dgender ==2 & bmi3 <= 23 & handgrip3 <= 13.6 ~ "1",
                                               r3dgender ==2 & bmi3 <= 23 & handgrip3 > 13.6 ~ "0",
                                               r3dgender ==2 &  bmi3 >= 23.1 & bmi3 <= 26 & handgrip3 <= 13.84 ~ "1",
                                               r3dgender ==2 &  bmi3 >= 23.1 & bmi3 <= 26 & handgrip3 > 13.84 ~ "0",
                                               r3dgender ==2 &  bmi3 >= 26.1 & bmi3 <= 29 & handgrip3 <= 14.4 ~ "1",
                                               r3dgender ==2 &  bmi3 >= 26.1 & bmi3 <= 29 & handgrip3 > 14.4 ~ "0",
                                               r3dgender ==2 & bmi3 > 29 & handgrip3 <= 16.8 ~ "1",
                                               r3dgender ==2 & bmi3 > 29 & handgrip3 > 16.8 ~ "0")))-> nhats3

table(nhats3$weak_strength3)

nhats4%>%
  mutate(weak_strength4 = as.numeric(case_when(r4dgender ==1 & bmi4 <= 24 & handgrip4 <= 23.2 ~ "1",
                                               r4dgender ==1 & bmi4 <= 24 & handgrip4 > 23.2 ~ "0",
                                               r4dgender ==1 &  bmi4 >= 24.1 & bmi4 <= 28 & handgrip4 <= 24 ~ "1",
                                               r4dgender ==1 &  bmi4 >= 24.1 & bmi4 <= 28 & handgrip4 > 24 ~ "0",
                                               r4dgender ==1 & bmi4 > 28 & handgrip4 <= 25.6 ~ "1",
                                               r4dgender ==1 & bmi4 > 28 & handgrip4 > 25.6 ~ "0",
                                               r4dgender ==2 & bmi4 <= 23 & handgrip4 <= 13.6 ~ "1",
                                               r4dgender ==2 & bmi4 <= 23 & handgrip4 > 13.6 ~ "0",
                                               r4dgender ==2 &  bmi4 >= 23.1 & bmi4 <= 26 & handgrip4 <= 13.84 ~ "1",
                                               r4dgender ==2 &  bmi4 >= 23.1 & bmi4 <= 26 & handgrip4 > 13.84 ~ "0",
                                               r4dgender ==2 &  bmi4 >= 26.1 & bmi4 <= 29 & handgrip4 <= 14.4 ~ "1",
                                               r4dgender ==2 &  bmi4 >= 26.1 & bmi4 <= 29 & handgrip4 > 14.4 ~ "0",
                                               r4dgender ==2 & bmi4 > 29 & handgrip4 <= 16.8 ~ "1",
                                               r4dgender ==2 & bmi4 > 29 & handgrip4 > 16.8 ~ "0")))-> nhats4

table(nhats4$weak_strength4)

nhats5%>%
  mutate(weak_strength5 = as.numeric(case_when(r5dgender ==1 & bmi5 <= 24 & handgrip5 <= 23.2 ~ "1",
                                               r5dgender ==1 & bmi5 <= 24 & handgrip5 > 23.2 ~ "0",
                                               r5dgender ==1 &  bmi5 >= 24.1 & bmi5 <= 28 & handgrip5 <= 24 ~ "1",
                                               r5dgender ==1 &  bmi5 >= 24.1 & bmi5 <= 28 & handgrip5 > 24 ~ "0",
                                               r5dgender ==1 & bmi5 > 28 & handgrip5 <= 25.6 ~ "1",
                                               r5dgender ==1 & bmi5 > 28 & handgrip5 > 25.6 ~ "0",
                                               r5dgender ==2 & bmi5 <= 23 & handgrip5 <= 13.6 ~ "1",
                                               r5dgender ==2 & bmi5 <= 23 & handgrip5 > 13.6 ~ "0",
                                               r5dgender ==2 &  bmi5 >= 23.1 & bmi5 <= 26 & handgrip5 <= 13.84 ~ "1",
                                               r5dgender ==2 &  bmi5 >= 23.1 & bmi5 <= 26 & handgrip5 > 13.84 ~ "0",
                                               r5dgender ==2 &  bmi5 >= 26.1 & bmi5 <= 29 & handgrip5 <= 14.4 ~ "1",
                                               r5dgender ==2 &  bmi5 >= 26.1 & bmi5 <= 29 & handgrip5 > 14.4 ~ "0",
                                               r5dgender ==2 & bmi5 > 29 & handgrip5 <= 16.8 ~ "1",
                                               r5dgender ==2 & bmi5 > 29 & handgrip5 > 16.8 ~ "0")))-> nhats5

table(nhats5$weak_strength5)

nhats6%>%
  mutate(weak_strength6 = as.numeric(case_when(r6dgender ==1 & bmi6 <= 24 & handgrip6 <= 23.2 ~ "1",
                                               r6dgender ==1 & bmi6 <= 24 & handgrip6 > 23.2 ~ "0",
                                               r6dgender ==1 &  bmi6 >= 24.1 & bmi6 <= 28 & handgrip6 <= 24 ~ "1",
                                               r6dgender ==1 &  bmi6 >= 24.1 & bmi6<= 28 & handgrip6 > 24 ~ "0",
                                               r6dgender ==1 & bmi6 > 28 & handgrip6 <= 25.6 ~ "1",
                                               r6dgender ==1 & bmi6 > 28 & handgrip6> 25.6 ~ "0",
                                               r6dgender ==2 & bmi6 <= 23 & handgrip6 <= 13.6 ~ "1",
                                               r6dgender ==2 & bmi6 <= 23 & handgrip6 > 13.6 ~ "0",
                                               r6dgender ==2 &  bmi6 >= 23.1 & bmi6 <= 26 & handgrip6 <= 13.84 ~ "1",
                                               r6dgender ==2 &  bmi6 >= 23.1 & bmi6 <= 26 & handgrip6 > 13.84 ~ "0",
                                               r6dgender ==2 &  bmi6 >= 26.1 & bmi6 <= 29 & handgrip6 <= 14.4 ~ "1",
                                               r6dgender ==2 &  bmi6 >= 26.1 & bmi6 <= 29 & handgrip6 > 14.4 ~ "0",
                                               r6dgender ==2 & bmi6 > 29 & handgrip6 <= 16.8 ~ "1",
                                               r6dgender ==2 & bmi6 > 29 & handgrip6 > 16.8 ~ "0")))-> nhats6

table(nhats6$weak_strength6)

nhats7%>%
  mutate(weak_strength7 = as.numeric(case_when(r7dgender ==1 & bmi7 <= 24 & handgrip7 <= 23.2 ~ "1",
                                               r7dgender ==1 & bmi7 <= 24 & handgrip7 > 23.2 ~ "0",
                                               r7dgender ==1 &  bmi7 >= 24.1 & bmi7 <= 28 & handgrip7 <= 24 ~ "1",
                                               r7dgender ==1 &  bmi7 >= 24.1 & bmi7 <= 28 & handgrip7 > 24 ~ "0",
                                               r7dgender ==1 & bmi7 > 28 & handgrip7 <= 25.6 ~ "1",
                                               r7dgender ==1 & bmi7 > 28 & handgrip7 > 25.6 ~ "0",
                                               r7dgender ==2 & bmi7 <= 23 & handgrip7 <= 13.6 ~ "1",
                                               r7dgender ==2 & bmi7 <= 23 & handgrip7 > 13.6 ~ "0",
                                               r7dgender ==2 &  bmi7 >= 23.1 & bmi7 <= 26 & handgrip7 <= 13.84 ~ "1",
                                               r7dgender ==2 &  bmi7 >= 23.1 & bmi7 <= 26 & handgrip7 > 13.84 ~ "0",
                                               r7dgender ==2 &  bmi7 >= 26.1 & bmi7 <= 29 & handgrip7 <= 14.4 ~ "1",
                                               r7dgender ==2 &  bmi7 >= 26.1 & bmi7 <= 29 & handgrip7 > 14.4 ~ "0",
                                               r7dgender ==2 & bmi7 > 29 & handgrip7 <= 16.8 ~ "1",
                                               r7dgender ==2 & bmi7 > 29 & handgrip7 > 16.8 ~ "0")))-> nhats7

table(nhats7$weak_strength7)

nhats8%>%
  mutate(weak_strength8 = as.numeric(case_when(r8dgender ==1 & bmi8 <= 24 & handgrip8 <= 23.2 ~ "1",
                                               r8dgender ==1 & bmi8 <= 24 & handgrip8 > 23.2 ~ "0",
                                               r8dgender ==1 &  bmi8 >= 24.1 & bmi8 <= 28 & handgrip8 <= 24 ~ "1",
                                               r8dgender ==1 &  bmi8 >= 24.1 & bmi8 <= 28 & handgrip8 > 24 ~ "0",
                                               r8dgender ==1 & bmi8 > 28 & handgrip8 <= 25.6 ~ "1",
                                               r8dgender ==1 & bmi8 > 28 & handgrip8 > 25.6 ~ "0",
                                               r8dgender ==2 & bmi8 <= 23 & handgrip8 <= 13.6 ~ "1",
                                               r8dgender ==2 & bmi8 <= 23 & handgrip8 > 13.6 ~ "0",
                                               r8dgender ==2 &  bmi8 >= 23.1 & bmi8 <= 26 & handgrip8 <= 13.84 ~ "1",
                                               r8dgender ==2 &  bmi8 >= 23.1 & bmi8 <= 26 & handgrip8 > 13.84 ~ "0",
                                               r8dgender ==2 &  bmi8 >= 26.1 & bmi8 <= 29 & handgrip8 <= 14.4 ~ "1",
                                               r8dgender ==2 &  bmi8 >= 26.1 & bmi8 <= 29 & handgrip8 > 14.4 ~ "0",
                                               r8dgender ==2 & bmi8 > 29 & handgrip8 <= 16.8 ~ "1",
                                               r8dgender ==2 & bmi8 > 29 & handgrip8 > 16.8 ~ "0")))-> nhats8

table(nhats8$weak_strength8)

nhats9%>%
  mutate(weak_strength9 = as.numeric(case_when(r9dgender ==1 & bmi9 <= 24 & handgrip9 <= 23.2 ~ "1",
                                               r9dgender ==1 & bmi9 <= 24 & handgrip9 > 23.2 ~ "0",
                                               r9dgender ==1 &  bmi9 >= 24.1 & bmi9 <= 28 & handgrip9 <= 24 ~ "1",
                                               r9dgender ==1 &  bmi9 >= 24.1 & bmi9 <= 28 & handgrip9 > 24 ~ "0",
                                               r9dgender ==1 & bmi9 > 28 & handgrip9 <= 25.6 ~ "1",
                                               r9dgender ==1 & bmi9 > 28 & handgrip9 > 25.6 ~ "0",
                                               r9dgender ==2 & bmi9 <= 23 & handgrip9 <= 13.6 ~ "1",
                                               r9dgender ==2 & bmi9 <= 23 & handgrip9 > 13.6 ~ "0",
                                               r9dgender ==2 &  bmi9 >= 23.1 & bmi9 <= 26 & handgrip9 <= 13.84 ~ "1",
                                               r9dgender ==2 &  bmi9 >= 23.1 & bmi9 <= 26 & handgrip9 > 13.84 ~ "0",
                                               r9dgender ==2 &  bmi9 >= 26.1 & bmi9 <= 29 & handgrip9 <= 14.4 ~ "1",
                                               r9dgender ==2 &  bmi9 >= 26.1 & bmi9 <= 29 & handgrip9 > 14.4 ~ "0",
                                               r9dgender ==2 & bmi9 > 29 & handgrip9 <= 16.8 ~ "1",
                                               r9dgender ==2 & bmi9 > 29 & handgrip9 > 16.8 ~ "0")))-> nhats9

table(nhats9$weak_strength9)

nhats10%>%
  mutate(weak_strength10 = as.numeric(case_when(r10dgender ==1 & bmi10 <= 24 & handgrip10 <= 23.2 ~ "1",
                                                r10dgender ==1 & bmi10 <= 24 & handgrip10 > 23.2 ~ "0",
                                                r10dgender ==1 &  bmi10 >= 24.1 & bmi10 <= 28 & handgrip10 <= 24 ~ "1",
                                                r10dgender ==1 &  bmi10 >= 24.1 & bmi10 <= 28 & handgrip10 > 24 ~ "0",
                                                r10dgender ==1 & bmi10 > 28 & handgrip10 <= 25.6 ~ "1",
                                                r10dgender ==1 & bmi10 > 28 & handgrip10 > 25.6 ~ "0",
                                                r10dgender ==2 & bmi10 <= 23 & handgrip10 <= 13.6 ~ "1",
                                                r10dgender ==2 & bmi10 <= 23 & handgrip10 > 13.6 ~ "0",
                                                r10dgender ==2 &  bmi10 >= 23.1 & bmi10 <= 26 & handgrip10 <= 13.84 ~ "1",
                                                r10dgender ==2 &  bmi10 >= 23.1 & bmi10 <= 26 & handgrip10 > 13.84 ~ "0",
                                                r10dgender ==2 &  bmi10 >= 26.1 & bmi10 <= 29 & handgrip10 <= 14.4 ~ "1",
                                                r10dgender ==2 &  bmi10 >= 26.1 & bmi10 <= 29 & handgrip10 > 14.4 ~ "0",
                                                r10dgender ==2 & bmi10 > 29 & handgrip10 <= 16.8 ~ "1",
                                                r10dgender ==2 & bmi10 > 29 & handgrip10 > 16.8 ~ "0")))-> nhats10

table(nhats10$weak_strength10)

nhats11%>%
  mutate(weak_strength11 = as.numeric(case_when(r11dgender ==1 & bmi11 <= 24 & handgrip11 <= 23.2 ~ "1",
                                                r11dgender ==1 & bmi11 <= 24 & handgrip11 > 23.2 ~ "0",
                                                r11dgender ==1 &  bmi11 >= 24.1 & bmi11 <= 28 & handgrip11 <= 24 ~ "1",
                                                r11dgender ==1 &  bmi11 >= 24.1 & bmi11 <= 28 & handgrip11 > 24 ~ "0",
                                                r11dgender ==1 & bmi11 > 28 & handgrip11 <= 25.6 ~ "1",
                                                r11dgender ==1 & bmi11 > 28 & handgrip11 > 25.6 ~ "0",
                                                r11dgender ==2 & bmi11 <= 23 & handgrip11 <= 13.6 ~ "1",
                                                r11dgender ==2 & bmi11 <= 23 & handgrip11 > 13.6 ~ "0",
                                                r11dgender ==2 &  bmi11 >= 23.1 & bmi11 <= 26 & handgrip11 <= 13.84 ~ "1",
                                                r11dgender ==2 &  bmi11 >= 23.1 & bmi11 <= 26 & handgrip11 > 13.84 ~ "0",
                                                r11dgender ==2 &  bmi11 >= 26.1 & bmi11 <= 29 & handgrip11 <= 14.4 ~ "1",
                                                r11dgender ==2 &  bmi11 >= 26.1 & bmi11 <= 29 & handgrip11 > 14.4 ~ "0",
                                                r11dgender ==2 & bmi11 > 29 & handgrip11 <= 16.8 ~ "1",
                                                r11dgender ==2 & bmi11 > 29 & handgrip11 > 16.8 ~ "0")))-> nhats11

table(nhats11$weak_strength11)

nhats12%>%
  mutate(weak_strength12 = as.numeric(case_when(r12dgender ==1 & bmi12 <= 24 & handgrip12 <= 23.2 ~ "1",
                                                r12dgender ==1 & bmi12 <= 24 & handgrip12 > 23.2 ~ "0",
                                                r12dgender ==1 &  bmi12 >= 24.1 & bmi12 <= 28 & handgrip12 <= 24 ~ "1",
                                                r12dgender ==1 &  bmi12 >= 24.1 & bmi12 <= 28 & handgrip12 > 24 ~ "0",
                                                r12dgender ==1 & bmi12 > 28 & handgrip12 <= 25.6 ~ "1",
                                                r12dgender ==1 & bmi12 > 28 & handgrip12 > 25.6 ~ "0",
                                                r12dgender ==2 & bmi12 <= 23 & handgrip12 <= 13.6 ~ "1",
                                                r12dgender ==2 & bmi12 <= 23 & handgrip12 > 13.6 ~ "0",
                                                r12dgender ==2 &  bmi12 >= 23.1 & bmi12 <= 26 & handgrip12 <= 13.84 ~ "1",
                                                r12dgender ==2 &  bmi12 >= 23.1 & bmi12 <= 26 & handgrip12 > 13.84 ~ "0",
                                                r12dgender ==2 &  bmi12 >= 26.1 & bmi12 <= 29 & handgrip12 <= 14.4 ~ "1",
                                                r12dgender ==2 &  bmi12 >= 26.1 & bmi12 <= 29 & handgrip12 > 14.4 ~ "0",
                                                r12dgender ==2 & bmi12 > 29 & handgrip12 <= 16.8 ~ "1",
                                                r12dgender ==2 & bmi12 > 29 & handgrip12 > 16.8 ~ "0")))-> nhats12

table(nhats12$weak_strength12)

######slow_walking_speed######

#mejor valor de walking

nhats1%>%
  mutate(walk1 = pmin(wa1wlkc1secs, wa1wlkc2secs)) -> nhats1

nhats2%>%
  mutate(walk2 = pmin(wa2wlkc1secs, wa2wlkc2secs)) -> nhats2

nhats3%>%
  mutate(walk3 = pmin(wa3wlkc1secs, wa3wlkc2secs)) -> nhats3

nhats4%>%
  mutate(walk4 = pmin(wa4wlkc1secs, wa4wlkc2secs)) -> nhats4

nhats5%>%
  mutate(walk5 = pmin(wa5wlkc1secs, wa5wlkc2secs)) -> nhats5

nhats6%>%
  mutate(walk6 = pmin(wa6wlkc1secs, wa6wlkc2secs)) -> nhats6

nhats7%>%
  mutate(walk7 = pmin(wa7wlkc1secs, wa7wlkc2secs)) -> nhats7

nhats8%>%
  mutate(walk8 = pmin(wa8wlkc1secs, wa8wlkc2secs)) -> nhats8

nhats9%>%
  mutate(walk9 = pmin(wa9wlkc1secs, wa9wlkc2secs)) -> nhats9

data_merged14 <- merge(nhats9, nhats10, by = "spid")
nhats10$walk10 <- data_merged14$walk9

nhats11%>%
  mutate(walk11 = pmin(wa11wlkc1secs, wa11wlkc2secs)) -> nhats11

nhats12%>%
  mutate(walk12 = pmin(wa12wlkc1secs, wa12wlkc2secs)) -> nhats12

#elimino -1 en walk

nhats1$walk1<-ifelse(nhats1$walk1<=-1, NA, nhats1$walk1) 
nhats2$walk2<-ifelse(nhats2$walk2<=-1, NA, nhats2$walk2) 
nhats3$walk3<-ifelse(nhats3$walk3<=-1, NA, nhats3$walk3) 
nhats4$walk4<-ifelse(nhats4$walk4<=-1, NA, nhats4$walk4) 
nhats5$walk5<-ifelse(nhats5$walk5<=-1, NA, nhats5$walk5) 
nhats6$walk6<-ifelse(nhats6$walk6<=-1, NA, nhats6$walk6) 
nhats7$walk7<-ifelse(nhats7$walk7<=-1, NA, nhats7$walk7) 
nhats8$walk8<-ifelse(nhats8$walk8<=-1, NA, nhats8$walk8) 
nhats9$walk9<-ifelse(nhats9$walk9<=-1, NA, nhats9$walk9) 
nhats10$walk10<-ifelse(nhats10$walk10<=-1, NA, nhats10$walk10) 
nhats11$walk11<-ifelse(nhats11$walk11<=-1, NA, nhats11$walk11) 
nhats12$walk12<-ifelse(nhats12$walk12<=-1, NA, nhats12$walk12) 

#slow_walk altura1 and walk1
nhats1%>%
  mutate(slow_walk1 = as.numeric(case_when(r1dgender ==1 & altura1 <= 1.73 & walk1 > 5.6 ~ "1",
                                           r1dgender ==1 & altura1 <= 1.73 & walk1 <= 5.6 ~ "0",
                                           r1dgender ==1 & altura1 > 1.73 & walk1 > 4.8 ~ "1",
                                           r1dgender ==1 & altura1 > 1.73 & walk1 <= 4.8 ~ "0",
                                           r1dgender ==2 & altura1 <= 1.59 & walk1 > 5.6 ~ "1",
                                           r1dgender ==2 & altura1 <= 1.59 & walk1 <= 5.6 ~ "0",
                                           r1dgender ==2 & altura1 > 1.59 & walk1 > 4.8 ~ "1",
                                           r1dgender ==2 & altura1 > 1.59 & walk1 <= 4.8 ~ "0")))-> nhats1

table(nhats1$slow_walk1)

nhats2%>%
  mutate(slow_walk2 = as.numeric(case_when(r2dgender ==1 & altura2 <= 1.73 & walk2 > 5.6 ~ "1",
                                           r2dgender ==1 & altura2 <= 1.73 & walk2 <= 5.6 ~ "0",
                                           r2dgender ==1 & altura2 > 1.73 & walk2 > 4.8 ~ "1",
                                           r2dgender ==1 & altura2 > 1.73 & walk2 <= 4.8 ~ "0",
                                           r2dgender ==2 & altura2 <= 1.59 & walk2 > 5.6 ~ "1",
                                           r2dgender ==2 & altura2 <= 1.59 & walk2 <= 5.6 ~ "0",
                                           r2dgender ==2 & altura2 > 1.59 & walk2 > 4.8 ~ "1",
                                           r2dgender ==2 & altura2 > 1.59 & walk2 <= 4.8 ~ "0")))-> nhats2

table(nhats2$slow_walk2)

nhats3%>%
  mutate(slow_walk3 = as.numeric(case_when(r3dgender ==1 & altura3 <= 1.73 & walk3 > 5.6 ~ "1",
                                           r3dgender ==1 & altura3 <= 1.73 & walk3 <= 5.6 ~ "0",
                                           r3dgender ==1 & altura3 > 1.73 & walk3 > 4.8 ~ "1",
                                           r3dgender ==1 & altura3 > 1.73 & walk3 <= 4.8 ~ "0",
                                           r3dgender ==2 & altura3 <= 1.59 & walk3 > 5.6 ~ "1",
                                           r3dgender ==2 & altura3 <= 1.59 & walk3 <= 5.6 ~ "0",
                                           r3dgender ==2 & altura3 > 1.59 & walk3 > 4.8 ~ "1",
                                           r3dgender ==2 & altura3 > 1.59 & walk3 <= 4.8 ~ "0")))-> nhats3

table(nhats3$slow_walk3)

nhats4%>%
  mutate(slow_walk4 = as.numeric(case_when(r4dgender ==1 & altura4 <= 1.73 & walk4 > 5.6 ~ "1",
                                           r4dgender ==1 & altura4 <= 1.73 & walk4 <= 5.6 ~ "0",
                                           r4dgender ==1 & altura4 > 1.73 & walk4 > 4.8 ~ "1",
                                           r4dgender ==1 & altura4 > 1.73 & walk4 <= 4.8 ~ "0",
                                           r4dgender ==2 & altura4 <= 1.59 & walk4 > 5.6 ~ "1",
                                           r4dgender ==2 & altura4 <= 1.59 & walk4 <= 5.6 ~ "0",
                                           r4dgender ==2 & altura4 > 1.59 & walk4 > 4.8 ~ "1",
                                           r4dgender ==2 & altura4 > 1.59 & walk4 <= 4.8 ~ "0")))-> nhats4

table(nhats4$slow_walk4)

nhats5%>%
  mutate(slow_walk5 = as.numeric(case_when(r5dgender ==1 & altura5 <= 1.73 & walk5 > 5.6 ~ "1",
                                           r5dgender ==1 & altura5 <= 1.73 & walk5 <= 5.6 ~ "0",
                                           r5dgender ==1 & altura5 > 1.73 & walk5 > 4.8 ~ "1",
                                           r5dgender ==1 & altura5 > 1.73 & walk5 <= 4.8 ~ "0",
                                           r5dgender ==2 & altura5 <= 1.59 & walk5 > 5.6 ~ "1",
                                           r5dgender ==2 & altura5 <= 1.59 & walk5 <= 5.6 ~ "0",
                                           r5dgender ==2 & altura5 > 1.59 & walk5 > 4.8 ~ "1",
                                           r5dgender ==2 & altura5 > 1.59 & walk5 <= 4.8 ~ "0")))-> nhats5

table(nhats5$slow_walk5)

nhats6%>%
  mutate(slow_walk6 = as.numeric(case_when(r6dgender ==1 & altura6 <= 1.73 & walk6 > 5.6 ~ "1",
                                           r6dgender ==1 & altura6 <= 1.73 & walk6 <= 5.6 ~ "0",
                                           r6dgender ==1 & altura6 > 1.73 & walk6 > 4.8 ~ "1",
                                           r6dgender ==1 & altura6 > 1.73 & walk6 <= 4.8 ~ "0",
                                           r6dgender ==2 & altura6 <= 1.59 & walk6 > 5.6 ~ "1",
                                           r6dgender ==2 & altura6 <= 1.59 & walk6 <= 5.6 ~ "0",
                                           r6dgender ==2 & altura6 > 1.59 & walk6 > 4.8 ~ "1",
                                           r6dgender ==2 & altura6 > 1.59 & walk6 <= 4.8 ~ "0")))-> nhats6

table(nhats6$slow_walk6)

nhats7%>%
  mutate(slow_walk7 = as.numeric(case_when(r7dgender ==1 & altura7 <= 1.73 & walk7 > 5.6 ~ "1",
                                           r7dgender ==1 & altura7 <= 1.73 & walk7 <= 5.6 ~ "0",
                                           r7dgender ==1 & altura7 > 1.73 & walk7 > 4.8 ~ "1",
                                           r7dgender ==1 & altura7 > 1.73 & walk7 <= 4.8 ~ "0",
                                           r7dgender ==2 & altura7 <= 1.59 & walk7 > 5.6 ~ "1",
                                           r7dgender ==2 & altura7 <= 1.59 & walk7 <= 5.6 ~ "0",
                                           r7dgender ==2 & altura7 > 1.59 & walk7 > 4.8 ~ "1",
                                           r7dgender ==2 & altura7 > 1.59 & walk7 <= 4.8 ~ "0")))-> nhats7

table(nhats7$slow_walk7)

nhats8%>%
  mutate(slow_walk8 = as.numeric(case_when(r8dgender ==1 & altura8 <= 1.73 & walk8 > 5.6 ~ "1",
                                           r8dgender ==1 & altura8 <= 1.73 & walk8 <= 5.6 ~ "0",
                                           r8dgender ==1 & altura8 > 1.73 & walk8 > 4.8 ~ "1",
                                           r8dgender ==1 & altura8 > 1.73 & walk8 <= 4.8 ~ "0",
                                           r8dgender ==2 & altura8 <= 1.59 & walk8 > 5.6 ~ "1",
                                           r8dgender ==2 & altura8 <= 1.59 & walk8 <= 5.6 ~ "0",
                                           r8dgender ==2 & altura8 > 1.59 & walk8 > 4.8 ~ "1",
                                           r8dgender ==2 & altura8 > 1.59 & walk8 <= 4.8 ~ "0")))-> nhats8

table(nhats8$slow_walk8)

nhats9%>%
  mutate(slow_walk9 = as.numeric(case_when(r9dgender ==1 & altura9 <= 1.73 & walk9 > 5.6 ~ "1",
                                           r9dgender ==1 & altura9 <= 1.73 & walk9 <= 5.6 ~ "0",
                                           r9dgender ==1 & altura9 > 1.73 & walk9 > 4.8 ~ "1",
                                           r9dgender ==1 & altura9 > 1.73 & walk9 <= 4.8 ~ "0",
                                           r9dgender ==2 & altura9 <= 1.59 & walk9 > 5.6 ~ "1",
                                           r9dgender ==2 & altura9 <= 1.59 & walk9 <= 5.6 ~ "0",
                                           r9dgender ==2 & altura9 > 1.59 & walk9 > 4.8 ~ "1",
                                           r9dgender ==2 & altura9 > 1.59 & walk9 <= 4.8 ~ "0")))-> nhats9

table(nhats9$slow_walk9)

nhats10%>%
  mutate(slow_walk10 = as.numeric(case_when(r10dgender ==1 & altura10 <= 1.73 & walk10 > 5.6 ~ "1",
                                            r10dgender ==1 & altura10 <= 1.73 & walk10 <= 5.6 ~ "0",
                                            r10dgender ==1 & altura10 > 1.73 & walk10 > 4.8 ~ "1",
                                            r10dgender ==1 & altura10 > 1.73 & walk10 <= 4.8 ~ "0",
                                            r10dgender ==2 & altura10 <= 1.59 & walk10 > 5.6 ~ "1",
                                            r10dgender ==2 & altura10 <= 1.59 & walk10 <= 5.6 ~ "0",
                                            r10dgender ==2 & altura10 > 1.59 & walk10 > 4.8 ~ "1",
                                            r10dgender ==2 & altura10 > 1.59 & walk10 <= 4.8 ~ "0")))-> nhats10

table(nhats10$slow_walk10)

nhats11%>%
  mutate(slow_walk11 = as.numeric(case_when(r11dgender ==1 & altura11 <= 1.73 & walk11 > 5.6 ~ "1",
                                            r11dgender ==1 & altura11 <= 1.73 & walk11 <= 5.6 ~ "0",
                                            r11dgender ==1 & altura11 > 1.73 & walk11 > 4.8 ~ "1",
                                            r11dgender ==1 & altura11 > 1.73 & walk11 <= 4.8 ~ "0",
                                            r11dgender ==2 & altura11 <= 1.59 & walk11 > 5.6 ~ "1",
                                            r11dgender ==2 & altura11 <= 1.59 & walk11 <= 5.6 ~ "0",
                                            r11dgender ==2 & altura11 > 1.59 & walk11 > 4.8 ~ "1",
                                            r11dgender ==2 & altura11 > 1.59 & walk11 <= 4.8 ~ "0")))-> nhats11

table(nhats11$slow_walk11)

nhats12%>%
  mutate(slow_walk12 = as.numeric(case_when(r12dgender ==1 & altura12 <= 1.73 & walk12 > 5.6 ~ "1",
                                            r12dgender ==1 & altura12 <= 1.73 & walk12 <= 5.6 ~ "0",
                                            r12dgender ==1 & altura12 > 1.73 & walk12 > 4.8 ~ "1",
                                            r12dgender ==1 & altura12 > 1.73 & walk12 <= 4.8 ~ "0",
                                            r12dgender ==2 & altura12 <= 1.59 & walk12 > 5.6 ~ "1",
                                            r12dgender ==2 & altura12 <= 1.59 & walk12 <= 5.6 ~ "0",
                                            r12dgender ==2 & altura12 > 1.59 & walk12 > 4.8 ~ "1",
                                            r12dgender ==2 & altura12 > 1.59 & walk12 <= 4.8 ~ "0")))-> nhats12

table(nhats12$slow_walk12)


######weight_loss-hw1trytolose######

nhats1%>%
  mutate(weight_loss1 = as.numeric(case_when(hw1trytolose ==1 ~ "1",
                                             hw1trytolose ==2  ~ "0",
                                             hw1trytolose <=-1~ "0")))->nhats1
table(nhats1$weight_loss1)

nhats2%>%
  mutate(weight_loss2 = as.numeric(case_when(hw2trytolose ==1 ~ "1",
                                             hw2trytolose ==2  ~ "0",
                                             hw2trytolose <=-1~ "0")))->nhats2
table(nhats2$weight_loss2)

nhats3%>%
  mutate(weight_loss3 = as.numeric(case_when(hw3trytolose ==1 ~ "1",
                                             hw3trytolose ==2  ~ "0",
                                             hw3trytolose <=-1~ "0")))->nhats3
table(nhats3$weight_loss3)

nhats4%>%
  mutate(weight_loss4 = as.numeric(case_when(hw4trytolose ==1 ~ "1",
                                             hw4trytolose ==2  ~ "0",
                                             hw4trytolose <=-1~ "0")))->nhats4
table(nhats4$weight_loss4)

nhats5%>%
  mutate(weight_loss5 = as.numeric(case_when(hw5trytolose ==1 ~ "1",
                                             hw5trytolose ==2  ~ "0",
                                             hw5trytolose <=-1~ "0")))->nhats5
table(nhats5$weight_loss5)

nhats6%>%
  mutate(weight_loss6 = as.numeric(case_when(hw6trytolose ==1 ~ "1",
                                             hw6trytolose ==2  ~ "0",
                                             hw6trytolose <=-1~ "0")))->nhats6
table(nhats6$weight_loss6)

nhats7%>%
  mutate(weight_loss7 = as.numeric(case_when(hw7trytolose ==1 ~ "1",
                                             hw7trytolose ==2  ~ "0",
                                             hw7trytolose <=-1~ "0")))->nhats7

table(nhats7$weight_loss7)


nhats8%>%
  mutate(weight_loss8 = as.numeric(case_when(hw8trytolose ==1 ~ "1",
                                             hw8trytolose ==2  ~ "0",
                                             hw8trytolose <=-1~ "0")))->nhats8
table(nhats8$weight_loss8)

nhats9%>%
  mutate(weight_loss9 = as.numeric(case_when(hw9trytolose ==1 ~ "1",
                                             hw9trytolose ==2  ~ "0",
                                             hw9trytolose <=-1~ "0")))->nhats9
table(nhats9$weight_loss9)

nhats10%>%
  mutate(weight_loss10 = as.numeric(case_when(hw10trytolose ==1 ~ "1",
                                              hw10trytolose ==2  ~ "0",
                                              hw10trytolose <=-1~ "0")))->nhats10
table(nhats10$weight_loss10)

nhats11%>%
  mutate(weight_loss11 = as.numeric(case_when(hw11trytolose ==1 ~ "1",
                                              hw11trytolose ==2  ~ "0",
                                              hw11trytolose <=-1~ "0")))->nhats11
table(nhats11$weight_loss11)

nhats12%>%
  mutate(weight_loss12 = as.numeric(case_when(hw12trytolose ==1 ~ "1",
                                              hw12trytolose ==2  ~ "0",
                                              hw12trytolose <=-1~ "0")))->nhats12
table(nhats12$weight_loss12)

######exhaustion-ss1lowenergy######
nhats1%>%
  mutate(exhaustion1 = as.numeric(case_when(ss1lowenergy ==1 ~ "1",
                                            ss1lowenergy ==2  ~ "0",
                                            ss1lowenergy <=-1~ "0")))->nhats1
table(nhats1$exhaustion1)

nhats2%>%
  mutate(exhaustion2 = as.numeric(case_when(ss2lowenergy ==1 ~ "1",
                                            ss2lowenergy ==2  ~ "0",
                                            ss2lowenergy <=-1~ "0")))->nhats2
table(nhats2$exhaustion2)

nhats3%>%
  mutate(exhaustion3 = as.numeric(case_when(ss3lowenergy ==1 ~ "1",
                                            ss3lowenergy ==2  ~ "0",
                                            ss3lowenergy <=-1~ "0")))->nhats3
table(nhats3$exhaustion3)

nhats4%>%
  mutate(exhaustion4 = as.numeric(case_when(ss4lowenergy ==1 ~ "1",
                                            ss4lowenergy ==2  ~ "0",
                                            ss4lowenergy <=-1~ "0")))->nhats4
table(nhats4$exhaustion4)

nhats5%>%
  mutate(exhaustion5 = as.numeric(case_when(ss5lowenergy ==1 ~ "1",
                                            ss5lowenergy ==2  ~ "0",
                                            ss5lowenergy <=-1~ "0")))->nhats5
table(nhats5$exhaustion5)

nhats6%>%
  mutate(exhaustion6 = as.numeric(case_when(ss6lowenergy ==1 ~ "1",
                                            ss6lowenergy ==2  ~ "0",
                                            ss6lowenergy <=-1~ "0")))->nhats6
table(nhats6$exhaustion6)

nhats7%>%
  mutate(exhaustion7 = as.numeric(case_when(ss7lowenergy ==1 ~ "1",
                                            ss7lowenergy ==2  ~ "0",
                                            ss7lowenergy <=-1~ "0")))->nhats7
table(nhats7$exhaustion7)

nhats8%>%
  mutate(exhaustion8 = as.numeric(case_when(ss8lowenergy ==1 ~ "1",
                                            ss8lowenergy ==2  ~ "0",
                                            ss8lowenergy <=-1~ "0")))->nhats8
table(nhats8$exhaustion8)

nhats9%>%
  mutate(exhaustion9 = as.numeric(case_when(ss9lowenergy ==1 ~ "1",
                                            ss9lowenergy ==2  ~ "0",
                                            ss9lowenergy <=-1~ "0")))->nhats9
table(nhats9$exhaustion9)

nhats10%>%
  mutate(exhaustion10 = as.numeric(case_when(ss10lowenergy ==1 ~ "1",
                                             ss10lowenergy ==2  ~ "0",
                                             ss10lowenergy <=-1~ "0")))->nhats10
table(nhats10$exhaustion10)

nhats11%>%
  mutate(exhaustion11 = as.numeric(case_when(ss11lowenergy ==1 ~ "1",
                                             ss11lowenergy ==2  ~ "0",
                                             ss11lowenergy <=-1~ "0")))->nhats11
table(nhats11$exhaustion11)

nhats12%>%
  mutate(exhaustion12 = as.numeric(case_when(ss12lowenergy ==1 ~ "1",
                                             ss12lowenergy ==2  ~ "0",
                                             ss12lowenergy <=-1~ "0")))->nhats12
table(nhats12$exhaustion12)

######low_physical_activity-pa1vigoractv######
nhats1%>%
  mutate(low_phy1 = as.numeric(case_when(pa1vigoractv ==1 ~ "0",
                                         pa1vigoractv ==2  ~ "1",
                                         pa1vigoractv <=-1~ "0")))->nhats1
table(nhats1$low_phy1)

nhats2%>%
  mutate(low_phy2 = as.numeric(case_when(pa2vigoractv ==1 ~ "0",
                                         pa2vigoractv ==2  ~ "1",
                                         pa2vigoractv <=-1~ "0")))->nhats2
table(nhats2$low_phy2)

nhats3%>%
  mutate(low_phy3 = as.numeric(case_when(pa3vigoractv ==1 ~ "0",
                                         pa3vigoractv ==2  ~ "1",
                                         pa3vigoractv <=-1~ "0")))->nhats3
table(nhats3$low_phy3)

nhats4%>%
  mutate(low_phy4 = as.numeric(case_when(pa4vigoractv ==1 ~ "0",
                                         pa4vigoractv ==2  ~ "1",
                                         pa4vigoractv <=-1~ "0")))->nhats4
table(nhats4$low_phy4)

nhats5%>%
  mutate(low_phy5 = as.numeric(case_when(pa5vigoractv ==1 ~ "0",
                                         pa5vigoractv ==2  ~ "1",
                                         pa5vigoractv <=-1~ "0")))->nhats5
table(nhats5$low_phy5)

nhats6%>%
  mutate(low_phy6 = as.numeric(case_when(pa6vigoractv ==1 ~ "0",
                                         pa6vigoractv ==2  ~ "1",
                                         pa6vigoractv <=-1~ "0")))->nhats6
table(nhats6$low_phy6)

nhats7%>%
  mutate(low_phy7 = as.numeric(case_when(pa7vigoractv ==1 ~ "0",
                                         pa7vigoractv ==2  ~ "1",
                                         pa7vigoractv <=-1~ "0")))->nhats7
table(nhats7$low_phy7)

nhats8%>%
  mutate(low_phy8 = as.numeric(case_when(pa8vigoractv ==1 ~ "0",
                                         pa8vigoractv ==2  ~ "1",
                                         pa8vigoractv <=-1~ "0")))->nhats8
table(nhats8$low_phy8)

nhats9%>%
  mutate(low_phy9 = as.numeric(case_when(pa9vigoractv ==1 ~ "0",
                                         pa9vigoractv ==2  ~ "1",
                                         pa9vigoractv <=-1~ "0")))->nhats9
table(nhats9$low_phy9)

nhats10%>%
  mutate(low_phy10 = as.numeric(case_when(pa10vigoractv ==1 ~ "0",
                                          pa10vigoractv ==2  ~ "1",
                                          pa10vigoractv <=-1~ "0")))->nhats10
table(nhats10$low_phy10)

nhats11%>%
  mutate(low_phy11 = as.numeric(case_when(pa11vigoractv ==1 ~ "0",
                                          pa11vigoractv ==2  ~ "1",
                                          pa11vigoractv <=-1~ "0")))->nhats11
table(nhats11$low_phy11)

nhats12%>%
  mutate(low_phy12 = as.numeric(case_when(pa12vigoractv ==1 ~ "0",
                                          pa12vigoractv ==2  ~ "1",
                                          pa12vigoractv <=-1~ "0")))->nhats12
table(nhats12$low_phy12)

######Calculate Fried frailty index######
nhats1%>%
  mutate(frailty1 = as.numeric (weak_strength1 +  slow_walk1 + weight_loss1 + exhaustion1 + low_phy1 ))->nhats1

table(nhats1$frailty1)

nhats2%>%
  mutate(frailty2 = as.numeric (weak_strength2 +  slow_walk2 + weight_loss2 + exhaustion2 + low_phy2 ))->nhats2

table(nhats2$frailty2)

nhats3%>%
  mutate(frailty3 = as.numeric (weak_strength3 +  slow_walk3 + weight_loss3 + exhaustion3 + low_phy3 ))->nhats3

table(nhats3$frailty3)

nhats4%>%
  mutate(frailty4 = as.numeric (weak_strength4 +  slow_walk4 + weight_loss4 + exhaustion4 + low_phy4 ))->nhats4

table(nhats4$frailty4)

nhats5%>%
  mutate(frailty5 = as.numeric (weak_strength5 +  slow_walk5 + weight_loss5 + exhaustion5 + low_phy5 ))->nhats5

table(nhats5$frailty5)

nhats6%>%
  mutate(frailty6 = as.numeric (weak_strength6 +  slow_walk6 + weight_loss6 + exhaustion6 + low_phy6 ))->nhats6

table(nhats6$frailty6)

nhats7%>%
  mutate(frailty7 = as.numeric (weak_strength7 +  slow_walk7 + weight_loss7 + exhaustion7 + low_phy7 ))->nhats7

table(nhats7$frailty7)

nhats8%>%
  mutate(frailty8 = as.numeric (weak_strength8 +  slow_walk8 + weight_loss8 + exhaustion8 + low_phy8 ))->nhats8

table(nhats8$frailty8)

nhats9%>%
  mutate(frailty9 = as.numeric (weak_strength9 +  slow_walk9 + weight_loss9 + exhaustion9 + low_phy9 ))->nhats9

table(nhats9$frailty9)

nhats10%>%
  mutate(frailty10 = as.numeric (weak_strength10 +  slow_walk10 + weight_loss10 + exhaustion10 + low_phy10 ))->nhats10

table(nhats10$frailty10)

nhats11%>%
  mutate(frailty11 = as.numeric (weak_strength11 +  slow_walk11 + weight_loss11 + exhaustion11 + low_phy11 ))->nhats11

table(nhats11$frailty11)

nhats12%>%
  mutate(frailty12 = as.numeric (weak_strength12 +  slow_walk12 + weight_loss12 + exhaustion12 + low_phy12 ))->nhats12

table(nhats12$frailty12)

####Frailty category - 5  severe frailty,; 3-4 = moderate frailty; 1-2= mild frailty####

nhats1%>%
  mutate(frailty_category1 = as.numeric(case_when(frailty1 == 0 ~ "0",
                                                  frailty1 == 1 ~ "1",
                                                  frailty1 == 2 ~ "1",
                                                  frailty1 == 3 ~ "2",
                                                  frailty1 == 4 ~ "2",
                                                  frailty1 == 5 ~ "2")))->nhats1
table(nhats1$frailty_category1)

nhats2%>%
  mutate(frailty_category2 = as.numeric(case_when(frailty2 == 0 ~ "0",
                                                  frailty2 == 1 ~ "1",
                                                  frailty2 == 2 ~ "1",
                                                  frailty2 == 3 ~ "2",
                                                  frailty2 == 4 ~ "2",
                                                  frailty2 == 5 ~ "2")))->nhats2
table(nhats2$frailty_category2)

nhats3%>%
  mutate(frailty_category3 = as.numeric(case_when(frailty3 == 0 ~ "0",
                                                  frailty3 == 1 ~ "1",
                                                  frailty3 == 2 ~ "1",
                                                  frailty3 == 3 ~ "2",
                                                  frailty3 == 4 ~ "2",
                                                  frailty3 == 5 ~ "2")))->nhats3
table(nhats3$frailty_category3)

nhats4%>%
  mutate(frailty_category4 = as.numeric(case_when(frailty4 == 0 ~ "0",
                                                  frailty4 == 1 ~ "1",
                                                  frailty4 == 2 ~ "1",
                                                  frailty4 == 3 ~ "2",
                                                  frailty4 == 4 ~ "2",
                                                  frailty4 == 5 ~ "2")))->nhats4
table(nhats4$frailty_category4)

nhats5%>%
  mutate(frailty_category5 = as.numeric(case_when(frailty5 == 0 ~ "0",
                                                  frailty5 == 1 ~ "1",
                                                  frailty5 == 2 ~ "1",
                                                  frailty5 == 3 ~ "2",
                                                  frailty5 == 4 ~ "2",
                                                  frailty5 == 5 ~ "2")))->nhats5
table(nhats5$frailty_category5)

nhats6%>%
  mutate(frailty_category6 = as.numeric(case_when(frailty6 == 0 ~ "0",
                                                  frailty6 == 1 ~ "1",
                                                  frailty6 == 2 ~ "1",
                                                  frailty6 == 3 ~ "2",
                                                  frailty6 == 4 ~ "2",
                                                  frailty6 == 5 ~ "2")))->nhats6
table(nhats6$frailty_category6)

nhats7%>%
  mutate(frailty_category7 = as.numeric(case_when(frailty7 == 0 ~ "0",
                                                  frailty7 == 1 ~ "1",
                                                  frailty7 == 2 ~ "1",
                                                  frailty7 == 3 ~ "2",
                                                  frailty7 == 4 ~ "2",
                                                  frailty7 == 5 ~ "2")))->nhats7
table(nhats7$frailty_category7)

nhats8%>%
  mutate(frailty_category8 = as.numeric(case_when(frailty8 == 0 ~ "0",
                                                  frailty8 == 1 ~ "1",
                                                  frailty8 == 2 ~ "1",
                                                  frailty8 == 3 ~ "2",
                                                  frailty8 == 4 ~ "2",
                                                  frailty8 == 5 ~ "2")))->nhats8
table(nhats8$frailty_category8)

nhats9%>%
  mutate(frailty_category9 = as.numeric(case_when(frailty9 == 0 ~ "0",
                                                  frailty9 == 1 ~ "1",
                                                  frailty9 == 2 ~ "1",
                                                  frailty9 == 3 ~ "2",
                                                  frailty9 == 4 ~ "2",
                                                  frailty9 == 5 ~ "2")))->nhats9
table(nhats9$frailty_category9)

nhats10%>%
  mutate(frailty_category10 = as.numeric(case_when(frailty10 == 0 ~ "0",
                                                   frailty10 == 1 ~ "1",
                                                   frailty10 == 2 ~ "1",
                                                   frailty10 == 3 ~ "2",
                                                   frailty10 == 4 ~ "2",
                                                   frailty10 == 5 ~ "2")))->nhats10
table(nhats10$frailty_category10)

nhats11%>%
  mutate(frailty_category11 = as.numeric(case_when(frailty11 == 0 ~ "0",
                                                   frailty11 == 1 ~ "1",
                                                   frailty11 == 2 ~ "1",
                                                   frailty11 == 3 ~ "2",
                                                   frailty11 == 4 ~ "2",
                                                   frailty11 == 5 ~ "2")))->nhats11
table(nhats11$frailty_category11)

nhats12%>%
  mutate(frailty_category12 = as.numeric(case_when(frailty12 == 0 ~ "0",
                                                   frailty12 == 1 ~ "1",
                                                   frailty12 == 2 ~ "1",
                                                   frailty12 == 3 ~ "2",
                                                   frailty12 == 4 ~ "2",
                                                   frailty12 == 5 ~ "2")))->nhats12
table(nhats12$frailty_category12)

####Sleep#####
#####over 30 min fall sleep#####
nhats1%>%
  mutate(sleep30_1 = as.numeric(case_when(hc1aslep30mn ==1 ~ "1",
                                          hc1aslep30mn ==2  ~ "2",
                                          hc1aslep30mn ==3  ~ "3",
                                          hc1aslep30mn ==4  ~ "4",
                                          hc1aslep30mn >=5  ~ "5",
                                          hc1aslep30mn <=-1~ "NA")))->nhats1
table(nhats1$sleep30_1)

nhats2%>%
  mutate(sleep30_2 = as.numeric(case_when(hc2aslep30mn ==1 ~ "1",
                                          hc2aslep30mn ==2  ~ "2",
                                          hc2aslep30mn ==3  ~ "3",
                                          hc2aslep30mn ==4  ~ "4",
                                          hc2aslep30mn >=5  ~ "5",
                                          hc2aslep30mn <=-1~ "NA")))->nhats2
table(nhats2$sleep30_2)

nhats3%>%
  mutate(sleep30_3 = as.numeric(case_when(hc3aslep30mn ==1 ~ "1",
                                          hc3aslep30mn ==2  ~ "2",
                                          hc3aslep30mn ==3  ~ "3",
                                          hc3aslep30mn ==4  ~ "4",
                                          hc3aslep30mn >=5  ~ "5",
                                          hc3aslep30mn <=-1~ "NA")))->nhats3
table(nhats3$sleep30_3)

nhats4%>%
  mutate(sleep30_4 = as.numeric(case_when(hc4aslep30mn ==1 ~ "1",
                                          hc4aslep30mn ==2  ~ "2",
                                          hc4aslep30mn ==3  ~ "3",
                                          hc4aslep30mn ==4  ~ "4",
                                          hc4aslep30mn >=5  ~ "5",
                                          hc4aslep30mn <=-1~ "NA")))->nhats4
table(nhats4$sleep30_4)

nhats5%>%
  mutate(sleep30_5 = as.numeric(case_when(hc5aslep30mn ==1 ~ "1",
                                          hc5aslep30mn ==2  ~ "2",
                                          hc5aslep30mn ==3  ~ "3",
                                          hc5aslep30mn ==4  ~ "4",
                                          hc5aslep30mn >=5  ~ "5",
                                          hc5aslep30mn <=-1~ "NA")))->nhats5
table(nhats5$sleep30_5)

nhats6%>%
  mutate(sleep30_6 = as.numeric(case_when(hc6aslep30mn ==1 ~ "1",
                                          hc6aslep30mn ==2  ~ "2",
                                          hc6aslep30mn ==3  ~ "3",
                                          hc6aslep30mn ==4  ~ "4",
                                          hc6aslep30mn >=5  ~ "5",
                                          hc6aslep30mn <=-1~ "NA")))->nhats6
table(nhats6$sleep30_6)

nhats7%>%
  mutate(sleep30_7 = as.numeric(case_when(hc7aslep30mn ==1 ~ "1",
                                          hc7aslep30mn ==2  ~ "2",
                                          hc7aslep30mn ==3  ~ "3",
                                          hc7aslep30mn ==4  ~ "4",
                                          hc7aslep30mn >=5  ~ "5",
                                          hc7aslep30mn <=-1~ "NA")))->nhats7
table(nhats7$sleep30_7)

nhats8%>%
  mutate(sleep30_8 = as.numeric(case_when(hc8aslep30mn ==1 ~ "1",
                                          hc8aslep30mn ==2  ~ "2",
                                          hc8aslep30mn ==3  ~ "3",
                                          hc8aslep30mn ==4  ~ "4",
                                          hc8aslep30mn >=5  ~ "5",
                                          hc8aslep30mn <=-1~ "NA")))->nhats8
table(nhats8$sleep30_8)

nhats9%>%
  mutate(sleep30_9 = as.numeric(case_when(hc9aslep30mn ==1 ~ "1",
                                          hc9aslep30mn ==2  ~ "2",
                                          hc9aslep30mn ==3  ~ "3",
                                          hc9aslep30mn ==4  ~ "4",
                                          hc9aslep30mn >=5  ~ "5",
                                          hc9aslep30mn <=-1~ "NA")))->nhats9
table(nhats9$sleep30_9)

nhats10%>%
  mutate(sleep30_10 = as.numeric(case_when(hc10aslep30mn ==1 ~ "1",
                                           hc10aslep30mn ==2  ~ "2",
                                           hc10aslep30mn ==3  ~ "3",
                                           hc10aslep30mn ==4  ~ "4",
                                           hc10aslep30mn >=5  ~ "5",
                                           hc10aslep30mn <=-1~ "NA")))->nhats10
table(nhats10$sleep30_10)

nhats11%>%
  mutate(sleep30_11 = as.numeric(case_when(hc11aslep30mn ==1 ~ "1",
                                           hc11aslep30mn ==2  ~ "2",
                                           hc11aslep30mn ==3  ~ "3",
                                           hc11aslep30mn ==4  ~ "4",
                                           hc11aslep30mn >=5  ~ "5",
                                           hc11aslep30mn <=-1~ "NA")))->nhats11
table(nhats11$sleep30_11)

nhats12%>%
  mutate(sleep30_12 = as.numeric(case_when(hc12aslep30mn ==1 ~ "1",
                                           hc12aslep30mn ==2  ~ "2",
                                           hc12aslep30mn ==3  ~ "3",
                                           hc12aslep30mn ==4  ~ "4",
                                           hc12aslep30mn >=5  ~ "5",
                                           hc12aslep30mn <=-1~ "NA")))->nhats12
table(nhats12$sleep30_12)

#####trouble falling back sleep#####
nhats1%>%
  mutate(sleepback_1 = as.numeric(case_when(hc1trbfalbck ==1 ~ "1",
                                            hc1trbfalbck ==2  ~ "2",
                                            hc1trbfalbck ==3  ~ "3",
                                            hc1trbfalbck ==4  ~ "4",
                                            hc1trbfalbck ==5  ~ "5",
                                            hc1trbfalbck >=7  ~ "7",
                                            hc1trbfalbck <=-1~ "NA")))->nhats1
table(nhats1$sleepback_1)

nhats2%>%
  mutate(sleepback_2 = as.numeric(case_when(hc2trbfalbck ==1 ~ "1",
                                            hc2trbfalbck ==2  ~ "2",
                                            hc2trbfalbck ==3  ~ "3",
                                            hc2trbfalbck ==4  ~ "4",
                                            hc2trbfalbck ==5  ~ "5",
                                            hc2trbfalbck >=7  ~ "7",
                                            hc2trbfalbck <=-1~ "NA")))->nhats2
table(nhats2$sleepback_2)

nhats3%>%
  mutate(sleepback_3 = as.numeric(case_when(hc3trbfalbck ==1 ~ "1",
                                            hc3trbfalbck ==2  ~ "2",
                                            hc3trbfalbck ==3  ~ "3",
                                            hc3trbfalbck ==4  ~ "4",
                                            hc3trbfalbck ==5  ~ "5",
                                            hc3trbfalbck >=7  ~ "7",
                                            hc3trbfalbck <=-1~ "NA")))->nhats3
table(nhats3$sleepback_3)

nhats4%>%
  mutate(sleepback_4 = as.numeric(case_when(hc4trbfalbck ==1 ~ "1",
                                            hc4trbfalbck ==2  ~ "2",
                                            hc4trbfalbck ==3  ~ "3",
                                            hc4trbfalbck ==4  ~ "4",
                                            hc4trbfalbck ==5  ~ "5",
                                            hc4trbfalbck >=7  ~ "7",
                                            hc4trbfalbck <=-1~ "NA")))->nhats4
table(nhats4$sleepback_4)

nhats5%>%
  mutate(sleepback_5 = as.numeric(case_when(hc5trbfalbck ==1 ~ "1",
                                            hc5trbfalbck ==2  ~ "2",
                                            hc5trbfalbck ==3  ~ "3",
                                            hc5trbfalbck ==4  ~ "4",
                                            hc5trbfalbck ==5  ~ "5",
                                            hc5trbfalbck >=7  ~ "7",
                                            hc5trbfalbck <=-1~ "NA")))->nhats5
table(nhats5$sleepback_5)

nhats6%>%
  mutate(sleepback_6 = as.numeric(case_when(hc6trbfalbck ==1 ~ "1",
                                            hc6trbfalbck ==2  ~ "2",
                                            hc6trbfalbck ==3  ~ "3",
                                            hc6trbfalbck ==4  ~ "4",
                                            hc6trbfalbck ==5  ~ "5",
                                            hc6trbfalbck >=7  ~ "7",
                                            hc6trbfalbck <=-1~ "NA")))->nhats6
table(nhats6$sleepback_6)

nhats7%>%
  mutate(sleepback_7 = as.numeric(case_when(hc7trbfalbck ==1 ~ "1",
                                            hc7trbfalbck ==2  ~ "2",
                                            hc7trbfalbck ==3  ~ "3",
                                            hc7trbfalbck ==4  ~ "4",
                                            hc7trbfalbck ==5  ~ "5",
                                            hc7trbfalbck >=7  ~ "7",
                                            hc7trbfalbck <=-1~ "NA")))->nhats7
table(nhats7$sleepback_7)

nhats8%>%
  mutate(sleepback_8 = as.numeric(case_when(hc8trbfalbck ==1 ~ "1",
                                            hc8trbfalbck ==2  ~ "2",
                                            hc8trbfalbck ==3  ~ "3",
                                            hc8trbfalbck ==4  ~ "4",
                                            hc8trbfalbck ==5  ~ "5",
                                            hc8trbfalbck >=7  ~ "7",
                                            hc8trbfalbck <=-1~ "NA")))->nhats8
table(nhats8$sleepback_8)

nhats9%>%
  mutate(sleepback_9 = as.numeric(case_when(hc9trbfalbck ==1 ~ "1",
                                            hc9trbfalbck ==2  ~ "2",
                                            hc9trbfalbck ==3  ~ "3",
                                            hc9trbfalbck ==4  ~ "4",
                                            hc9trbfalbck ==5  ~ "5",
                                            hc9trbfalbck >=7  ~ "7",
                                            hc9trbfalbck <=-1~ "NA")))->nhats9
table(nhats9$sleepback_9)

nhats10%>%
  mutate(sleepback_10 = as.numeric(case_when(hc10trbfalbck ==1 ~ "1",
                                             hc10trbfalbck ==2  ~ "2",
                                             hc10trbfalbck ==3  ~ "3",
                                             hc10trbfalbck ==4  ~ "4",
                                             hc10trbfalbck ==5  ~ "5",
                                             hc10trbfalbck >=7  ~ "7",
                                             hc10trbfalbck <=-1~ "NA")))->nhats10
table(nhats10$sleepback_10)

nhats11%>%
  mutate(sleepback_11 = as.numeric(case_when(hc11trbfalbck ==1 ~ "1",
                                             hc11trbfalbck ==2  ~ "2",
                                             hc11trbfalbck ==3  ~ "3",
                                             hc11trbfalbck ==4  ~ "4",
                                             hc11trbfalbck ==5  ~ "5",
                                             hc11trbfalbck >=7  ~ "7",
                                             hc11trbfalbck <=-1~ "NA")))->nhats11
table(nhats11$sleepback_11)

nhats12%>%
  mutate(sleepback_12 = as.numeric(case_when(hc12trbfalbck ==1 ~ "1",
                                             hc12trbfalbck ==2  ~ "2",
                                             hc12trbfalbck ==3  ~ "3",
                                             hc12trbfalbck ==4  ~ "4",
                                             hc12trbfalbck ==5  ~ "5",
                                             hc12trbfalbck >=7  ~ "7",
                                             hc12trbfalbck <=-1~ "NA")))->nhats12
table(nhats12$sleepback_12)

#####medicate help sleep#####
nhats1%>%
  mutate(sleepdrug_1 = as.numeric(case_when(hc1sleepmed ==1 ~ "1",
                                            hc1sleepmed ==2  ~ "2",
                                            hc1sleepmed ==3  ~ "3",
                                            hc1sleepmed ==4  ~ "4",
                                            hc1sleepmed >=5  ~ "5",
                                            hc1sleepmed <=-1~ "NA")))->nhats1
table(nhats1$sleepdrug_1)

nhats2%>%
  mutate(sleepdrug_2 = as.numeric(case_when(hc2sleepmed ==1 ~ "1",
                                            hc2sleepmed ==2  ~ "2",
                                            hc2sleepmed ==3  ~ "3",
                                            hc2sleepmed ==4  ~ "4",
                                            hc2sleepmed >=5  ~ "5",
                                            hc2sleepmed <=-1~ "NA")))->nhats2
table(nhats2$sleepdrug_2)

nhats3%>%
  mutate(sleepdrug_3 = as.numeric(case_when(hc3sleepmed ==1 ~ "1",
                                            hc3sleepmed ==2  ~ "2",
                                            hc3sleepmed ==3  ~ "3",
                                            hc3sleepmed ==4  ~ "4",
                                            hc3sleepmed >=5  ~ "5",
                                            hc3sleepmed <=-1~ "NA")))->nhats3
table(nhats3$sleepdrug_3)

nhats4%>%
  mutate(sleepdrug_4 = as.numeric(case_when(hc4sleepmed ==1 ~ "1",
                                            hc4sleepmed ==2  ~ "2",
                                            hc4sleepmed ==3  ~ "3",
                                            hc4sleepmed ==4  ~ "4",
                                            hc4sleepmed >=5  ~ "5",
                                            hc4sleepmed <=-1~ "NA")))->nhats4
table(nhats4$sleepdrug_4)

nhats5%>%
  mutate(sleepdrug_5 = as.numeric(case_when(hc5sleepmed ==1 ~ "1",
                                            hc5sleepmed ==2  ~ "2",
                                            hc5sleepmed ==3  ~ "3",
                                            hc5sleepmed ==4  ~ "4",
                                            hc5sleepmed >=5  ~ "5",
                                            hc5sleepmed <=-1~ "NA")))->nhats5
table(nhats5$sleepdrug_5)

nhats6%>%
  mutate(sleepdrug_6 = as.numeric(case_when(hc6sleepmed ==1 ~ "1",
                                            hc6sleepmed ==2  ~ "2",
                                            hc6sleepmed ==3  ~ "3",
                                            hc6sleepmed ==4  ~ "4",
                                            hc6sleepmed >=5  ~ "5",
                                            hc6sleepmed <=-1~ "NA")))->nhats6
table(nhats6$sleepdrug_6)

nhats7%>%
  mutate(sleepdrug_7 = as.numeric(case_when(hc7sleepmed ==1 ~ "1",
                                            hc7sleepmed ==2  ~ "2",
                                            hc7sleepmed ==3  ~ "3",
                                            hc7sleepmed ==4  ~ "4",
                                            hc7sleepmed >=5  ~ "5",
                                            hc7sleepmed <=-1~ "NA")))->nhats7
table(nhats7$sleepdrug_7)

nhats8%>%
  mutate(sleepdrug_8 = as.numeric(case_when(hc8sleepmed ==1 ~ "1",
                                            hc8sleepmed ==2  ~ "2",
                                            hc8sleepmed ==3  ~ "3",
                                            hc8sleepmed ==4  ~ "4",
                                            hc8sleepmed >=5  ~ "5",
                                            hc8sleepmed <=-1~ "NA")))->nhats8
table(nhats8$sleepdrug_8)

nhats9%>%
  mutate(sleepdrug_9 = as.numeric(case_when(hc9sleepmed ==1 ~ "1",
                                            hc9sleepmed ==2  ~ "2",
                                            hc9sleepmed ==3  ~ "3",
                                            hc9sleepmed ==4  ~ "4",
                                            hc9sleepmed >=5  ~ "5",
                                            hc9sleepmed <=-1~ "NA")))->nhats9
table(nhats9$sleepdrug_9)

nhats10%>%
  mutate(sleepdrug_10 = as.numeric(case_when(hc10sleepmed ==1 ~ "1",
                                             hc10sleepmed ==2  ~ "2",
                                             hc10sleepmed ==3  ~ "3",
                                             hc10sleepmed ==4  ~ "4",
                                             hc10sleepmed >=5  ~ "5",
                                             hc10sleepmed <=-1~ "NA")))->nhats10
table(nhats10$sleepdrug_10)

nhats11%>%
  mutate(sleepdrug_11 = as.numeric(case_when(hc11sleepmed ==1 ~ "1",
                                             hc11sleepmed ==2  ~ "2",
                                             hc11sleepmed ==3  ~ "3",
                                             hc11sleepmed ==4  ~ "4",
                                             hc11sleepmed >=5  ~ "5",
                                             hc11sleepmed <=-1~ "NA")))->nhats11
table(nhats11$sleepdrug_11)

nhats12%>%
  mutate(sleepdrug_12 = as.numeric(case_when(hc12sleepmed ==1 ~ "1",
                                             hc12sleepmed ==2  ~ "2",
                                             hc12sleepmed ==3  ~ "3",
                                             hc12sleepmed ==4  ~ "4",
                                             hc12sleepmed >=5  ~ "5",
                                             hc12sleepmed <=-1~ "NA")))->nhats12
table(nhats12$sleepdrug_12)

####BALANCE####

#Side by Side_ 1complete 10s; 2 no completa 10s; 3 no puede

nhats1%>%
  mutate(side1 = as.numeric(case_when(ba1sxsresult == 1 ~ "1",
                                      ba1sxsresult == 2 ~ "2",
                                      ba1sxsresult == 3 ~ "3",
                                      ba1sxsresult >= -1 ~ "NA")))->nhats1

nhats2%>%
  mutate(side2 = as.numeric(case_when(ba2sxsresult == 1 ~ "1",
                                      ba2sxsresult == 2 ~ "2",
                                      ba2sxsresult == 3 ~ "3",
                                      ba2sxsresult >= -1 ~ "NA")))->nhats2

nhats3%>%
  mutate(side3 = as.numeric(case_when(ba3sxsresult == 1 ~ "1",
                                      ba3sxsresult == 2 ~ "2",
                                      ba3sxsresult == 3 ~ "3",
                                      ba3sxsresult >= -1 ~ "NA")))->nhats3

nhats4%>%
  mutate(side4 = as.numeric(case_when(ba4sxsresult == 1 ~ "1",
                                      ba4sxsresult == 2 ~ "2",
                                      ba4sxsresult == 3 ~ "3",
                                      ba4sxsresult >= -1 ~ "NA")))->nhats4

nhats5%>%
  mutate(side5 = as.numeric(case_when(ba5sxsresult == 1 ~ "1",
                                      ba5sxsresult == 2 ~ "2",
                                      ba5sxsresult == 3 ~ "3",
                                      ba5sxsresult >= -1 ~ "NA")))->nhats5

nhats6%>%
  mutate(side6 = as.numeric(case_when(ba6sxsresult == 1 ~ "1",
                                      ba6sxsresult == 2 ~ "2",
                                      ba6sxsresult == 3 ~ "3",
                                      ba6sxsresult >= -1 ~ "NA")))->nhats6

nhats7%>%
  mutate(side7 = as.numeric(case_when(ba7sxsresult == 1 ~ "1",
                                      ba7sxsresult == 2 ~ "2",
                                      ba7sxsresult == 3 ~ "3",
                                      ba7sxsresult >= -1 ~ "NA")))->nhats7

nhats8%>%
  mutate(side8 = as.numeric(case_when(ba8sxsresult == 1 ~ "1",
                                      ba8sxsresult == 2 ~ "2",
                                      ba8sxsresult == 3 ~ "3",
                                      ba8sxsresult >= -1 ~ "NA")))->nhats8

nhats9%>%
  mutate(side9 = as.numeric(case_when(ba9sxsresult == 1 ~ "1",
                                      ba9sxsresult == 2 ~ "2",
                                      ba9sxsresult == 3 ~ "3",
                                      ba9sxsresult >= -1 ~ "NA")))->nhats9

nhats11%>%
  mutate(side11 = as.numeric(case_when(ba11sxsresult == 1 ~ "1",
                                       ba11sxsresult == 2 ~ "2",
                                       ba11sxsresult == 3 ~ "3",
                                       ba11sxsresult >= -1 ~ "NA")))->nhats11

nhats12%>%
  mutate(side12 = as.numeric(case_when(ba12sxsresult == 1 ~ "1",
                                       ba12sxsresult == 2 ~ "2",
                                       ba12sxsresult == 3 ~ "3",
                                       ba12sxsresult >= -1 ~ "NA")))->nhats12

#Semitandem_ 1complete 10s; 2 no completa 10s; 3 no puede

nhats1%>%
  mutate(semitandem1 = as.numeric(case_when(ba1stdmreslt == 1 ~ "1",
                                            ba1stdmreslt == 2 ~ "2",
                                            ba1stdmreslt == 3 ~ "3",
                                            ba1stdmreslt >= -1 ~ "NA")))->nhats1

nhats2%>%
  mutate(semitandem2 = as.numeric(case_when(ba2stdmreslt == 1 ~ "1",
                                            ba2stdmreslt == 2 ~ "2",
                                            ba2stdmreslt == 3 ~ "3",
                                            ba2stdmreslt >= -1 ~ "NA")))->nhats2

nhats3%>%
  mutate(semitandem3 = as.numeric(case_when(ba3stdmreslt == 1 ~ "1",
                                            ba3stdmreslt == 2 ~ "2",
                                            ba3stdmreslt == 3 ~ "3",
                                            ba3stdmreslt >= -1 ~ "NA")))->nhats3

nhats4%>%
  mutate(semitandem4 = as.numeric(case_when(ba4stdmreslt == 1 ~ "1",
                                            ba4stdmreslt == 2 ~ "2",
                                            ba4stdmreslt == 3 ~ "3",
                                            ba4stdmreslt >= -1 ~ "NA")))->nhats4

nhats5%>%
  mutate(semitandem5 = as.numeric(case_when(ba5stdmreslt == 1 ~ "1",
                                            ba5stdmreslt == 2 ~ "2",
                                            ba5stdmreslt == 3 ~ "3",
                                            ba5stdmreslt >= -1 ~ "NA")))->nhats5

nhats6%>%
  mutate(semitandem6 = as.numeric(case_when(ba6stdmreslt == 1 ~ "1",
                                            ba6stdmreslt == 2 ~ "2",
                                            ba6stdmreslt == 3 ~ "3",
                                            ba6stdmreslt >= -1 ~ "NA")))->nhats6

nhats7%>%
  mutate(semitandem7 = as.numeric(case_when(ba7stdmreslt == 1 ~ "1",
                                            ba7stdmreslt == 2 ~ "2",
                                            ba7stdmreslt == 3 ~ "3",
                                            ba7stdmreslt >= -1 ~ "NA")))->nhats7

nhats8%>%
  mutate(semitandem8 = as.numeric(case_when(ba8stdmreslt == 1 ~ "1",
                                            ba8stdmreslt == 2 ~ "2",
                                            ba8stdmreslt == 3 ~ "3",
                                            ba8stdmreslt >= -1 ~ "NA")))->nhats8

nhats9%>%
  mutate(semitandem9 = as.numeric(case_when(ba9stdmreslt == 1 ~ "1",
                                            ba9stdmreslt == 2 ~ "2",
                                            ba9stdmreslt == 3 ~ "3",
                                            ba9stdmreslt >= -1 ~ "NA")))->nhats9


nhats11%>%
  mutate(semitandem11 = as.numeric(case_when(ba11stdmreslt == 1 ~ "1",
                                             ba11stdmreslt == 2 ~ "2",
                                             ba11stdmreslt == 3 ~ "3",
                                             ba11stdmreslt >= -1 ~ "NA")))->nhats11
nhats12%>%
  mutate(semitandem12 = as.numeric(case_when(ba12stdmreslt == 1 ~ "1",
                                             ba12stdmreslt == 2 ~ "2",
                                             ba12stdmreslt == 3 ~ "3",
                                             ba12stdmreslt >= -1 ~ "NA")))->nhats12

#Tandem_ 1complete 10s; 2 no completa 10s; 3 no puede

nhats1%>%
  mutate(tandem1 = as.numeric(case_when(ba1ftdmreslt == 1 ~ "1",
                                        ba1ftdmreslt == 2 ~ "2",
                                        ba1ftdmreslt == 3 ~ "3",
                                        ba1ftdmreslt >= -1 ~ "NA")))->nhats1

nhats2%>%
  mutate(tandem2 = as.numeric(case_when(ba2ftdmreslt == 1 ~ "1",
                                        ba2ftdmreslt == 2 ~ "2",
                                        ba2ftdmreslt == 3 ~ "3",
                                        ba2ftdmreslt >= -1 ~ "NA")))->nhats2
nhats3%>%
  mutate(tandem3 = as.numeric(case_when(ba3ftdmreslt == 1 ~ "1",
                                        ba3ftdmreslt == 2 ~ "2",
                                        ba3ftdmreslt == 3 ~ "3",
                                        ba3ftdmreslt >= -1 ~ "NA")))->nhats3
nhats4%>%
  mutate(tandem4 = as.numeric(case_when(ba4ftdmreslt == 1 ~ "1",
                                        ba4ftdmreslt == 2 ~ "2",
                                        ba4ftdmreslt == 3 ~ "3",
                                        ba4ftdmreslt >= -1 ~ "NA")))->nhats4
nhats5%>%
  mutate(tandem5 = as.numeric(case_when(ba5ftdmreslt == 1 ~ "1",
                                        ba5ftdmreslt == 2 ~ "2",
                                        ba5ftdmreslt == 3 ~ "3",
                                        ba5ftdmreslt >= -1 ~ "NA")))->nhats5
nhats6%>%
  mutate(tandem6 = as.numeric(case_when(ba6ftdmreslt == 1 ~ "1",
                                        ba6ftdmreslt == 2 ~ "2",
                                        ba6ftdmreslt == 3 ~ "3",
                                        ba6ftdmreslt >= -1 ~ "NA")))->nhats6
nhats7%>%
  mutate(tandem7 = as.numeric(case_when(ba7ftdmreslt == 1 ~ "1",
                                        ba7ftdmreslt == 2 ~ "2",
                                        ba7ftdmreslt == 3 ~ "3",
                                        ba7ftdmreslt >= -1 ~ "NA")))->nhats7
nhats8%>%
  mutate(tandem8 = as.numeric(case_when(ba8ftdmreslt == 1 ~ "1",
                                        ba8ftdmreslt == 2 ~ "2",
                                        ba8ftdmreslt == 3 ~ "3",
                                        ba8ftdmreslt >= -1 ~ "NA")))->nhats8
nhats9%>%
  mutate(tandem9 = as.numeric(case_when(ba9ftdmreslt == 1 ~ "1",
                                        ba9ftdmreslt == 2 ~ "2",
                                        ba9ftdmreslt == 3 ~ "3",
                                        ba9ftdmreslt >= -1 ~ "NA")))->nhats9

nhats11%>%
  mutate(tandem11 = as.numeric(case_when(ba11ftdmreslt == 1 ~ "1",
                                         ba11ftdmreslt == 2 ~ "2",
                                         ba11ftdmreslt == 3 ~ "3",
                                         ba11ftdmreslt >= -1 ~ "NA")))->nhats11

nhats12%>%
  mutate(tandem12 = as.numeric(case_when(ba12ftdmreslt == 1 ~ "1",
                                         ba12ftdmreslt == 2 ~ "2",
                                         ba12ftdmreslt == 3 ~ "3",
                                         ba12ftdmreslt >= -1 ~ "NA")))->nhats12

#One leg eyes open_ 1complete 10s; 2 no completa 10s; 3 no puede

nhats1%>%
  mutate(eyes_open1 = as.numeric(case_when(ba11leoreslt == 1 ~ "1",
                                           ba11leoreslt == 2 ~ "2",
                                           ba11leoreslt == 3 ~ "3",
                                           ba11leoreslt >= -1 ~ "NA")))->nhats1

nhats2%>%
  mutate(eyes_open2 = as.numeric(case_when(ba21leoreslt == 1 ~ "1",
                                           ba21leoreslt == 2 ~ "2",
                                           ba21leoreslt == 3 ~ "3",
                                           ba21leoreslt >= -1 ~ "NA")))->nhats2

nhats3%>%
  mutate(eyes_open3 = as.numeric(case_when(ba31leoreslt == 1 ~ "1",
                                           ba31leoreslt == 2 ~ "2",
                                           ba31leoreslt == 3 ~ "3",
                                           ba31leoreslt >= -1 ~ "NA")))->nhats3

nhats4%>%
  mutate(eyes_open4 = as.numeric(case_when(ba41leoreslt == 1 ~ "1",
                                           ba41leoreslt == 2 ~ "2",
                                           ba41leoreslt == 3 ~ "3",
                                           ba41leoreslt >= -1 ~ "NA")))->nhats4

nhats5%>%
  mutate(eyes_open5 = as.numeric(case_when(ba51leoreslt == 1 ~ "1",
                                           ba51leoreslt == 2 ~ "2",
                                           ba51leoreslt == 3 ~ "3",
                                           ba51leoreslt >= -1 ~ "NA")))->nhats5

nhats6%>%
  mutate(eyes_open6 = as.numeric(case_when(ba61leoreslt == 1 ~ "1",
                                           ba61leoreslt == 2 ~ "2",
                                           ba61leoreslt == 3 ~ "3",
                                           ba61leoreslt >= -1 ~ "NA")))->nhats6

nhats7%>%
  mutate(eyes_open7 = as.numeric(case_when(ba71leoreslt == 1 ~ "1",
                                           ba71leoreslt == 2 ~ "2",
                                           ba71leoreslt == 3 ~ "3",
                                           ba71leoreslt >= -1 ~ "NA")))->nhats7

nhats8%>%
  mutate(eyes_open8 = as.numeric(case_when(ba81leoreslt == 1 ~ "1",
                                           ba81leoreslt == 2 ~ "2",
                                           ba81leoreslt == 3 ~ "3",
                                           ba81leoreslt >= -1 ~ "NA")))->nhats8

nhats9%>%
  mutate(eyes_open9 = as.numeric(case_when(ba91leoreslt == 1 ~ "1",
                                           ba91leoreslt == 2 ~ "2",
                                           ba91leoreslt == 3 ~ "3",
                                           ba91leoreslt >= -1 ~ "NA")))->nhats9

nhats11%>%
  mutate(eyes_open11 = as.numeric(case_when(ba111leoreslt == 1 ~ "1",
                                            ba111leoreslt == 2 ~ "2",
                                            ba111leoreslt == 3 ~ "3",
                                            ba111leoreslt >= -1 ~ "NA")))->nhats11

nhats12%>%
  mutate(eyes_open12 = as.numeric(case_when(ba121leoreslt == 1 ~ "1",
                                            ba121leoreslt == 2 ~ "2",
                                            ba121leoreslt == 3 ~ "3",
                                            ba121leoreslt >= -1 ~ "NA")))->nhats12
table(nhats12$ba)
#One leg eyes closed_ 1complete 10s; 2 no completa 10s; 3 no puede

nhats1%>%
  mutate(eyes_closed1 = as.numeric(case_when(ba11lecreslt == 1 ~ "1",
                                             ba11lecreslt == 2 ~ "2",
                                             ba11lecreslt == 3 ~ "3",
                                             ba11lecreslt >= -1 ~ "NA")))->nhats1

nhats2%>%
  mutate(eyes_closed2 = as.numeric(case_when(ba21lecreslt == 1 ~ "1",
                                             ba21lecreslt == 2 ~ "2",
                                             ba21lecreslt == 3 ~ "3",
                                             ba21lecreslt >= -1 ~ "NA")))->nhats2

nhats3%>%
  mutate(eyes_closed3 = as.numeric(case_when(ba31lecreslt == 1 ~ "1",
                                             ba31lecreslt == 2 ~ "2",
                                             ba31lecreslt == 3 ~ "3",
                                             ba31lecreslt >= -1 ~ "NA")))->nhats3

nhats4%>%
  mutate(eyes_closed4 = as.numeric(case_when(ba41lecreslt == 1 ~ "1",
                                             ba41lecreslt == 2 ~ "2",
                                             ba41lecreslt == 3 ~ "3",
                                             ba41lecreslt >= -1 ~ "NA")))->nhats4

nhats5%>%
  mutate(eyes_closed5 = as.numeric(case_when(ba51lecreslt == 1 ~ "1",
                                             ba51lecreslt == 2 ~ "2",
                                             ba51lecreslt == 3 ~ "3",
                                             ba51lecreslt >= -1 ~ "NA")))->nhats5

nhats6%>%
  mutate(eyes_closed6 = as.numeric(case_when(ba61lecreslt == 1 ~ "1",
                                             ba61lecreslt == 2 ~ "2",
                                             ba61lecreslt == 3 ~ "3",
                                             ba61lecreslt >= -1 ~ "NA")))->nhats6

nhats7%>%
  mutate(eyes_closed7 = as.numeric(case_when(ba71lecreslt == 1 ~ "1",
                                             ba71lecreslt == 2 ~ "2",
                                             ba71lecreslt == 3 ~ "3",
                                             ba71lecreslt >= -1 ~ "NA")))->nhats7

nhats8%>%
  mutate(eyes_closed8 = as.numeric(case_when(ba81lecreslt == 1 ~ "1",
                                             ba81lecreslt == 2 ~ "2",
                                             ba81lecreslt == 3 ~ "3",
                                             ba81lecreslt >= -1 ~ "NA")))->nhats8

nhats9%>%
  mutate(eyes_closed9 = as.numeric(case_when(ba91lecreslt == 1 ~ "1",
                                             ba91lecreslt == 2 ~ "2",
                                             ba91lecreslt == 3 ~ "3",
                                             ba91lecreslt >= -1 ~ "NA")))->nhats9

nhats11%>%
  mutate(eyes_closed11 = as.numeric(case_when(ba111lecreslt == 1 ~ "1",
                                              ba111lecreslt == 2 ~ "2",
                                              ba111lecreslt == 3 ~ "3",
                                              ba111lecreslt >= -1 ~ "NA")))->nhats11
nhats12%>%
  mutate(eyes_closed12 = as.numeric(case_when(ba121lecreslt == 1 ~ "1",
                                              ba121lecreslt == 2 ~ "2",
                                              ba121lecreslt == 3 ~ "3",
                                              ba121lecreslt >= -1 ~ "NA")))->nhats12
####Air Flow_no hay en r10 ni r11####

#mejor valor de airflow

nhats1%>%
  mutate(air1 = pmax(pk1pkarfl1rd, pk1pkarfl2rd)) -> nhats1

nhats2%>%
  mutate(air2 = pmax(pk2pkarfl1rd, pk2pkarfl2rd)) -> nhats2

nhats3%>%
  mutate(air3 = pmax(pk3pkarfl1rd, pk3pkarfl2rd)) -> nhats3

nhats4%>%
  mutate(air4 = pmax(pk4pkarfl1rd, pk4pkarfl2rd)) -> nhats4

nhats5%>%
  mutate(air5 = pmax(pk5pkarfl1rd, pk5pkarfl2rd)) -> nhats5

nhats6%>%
  mutate(air6 = pmax(pk6pkarfl1rd, pk6pkarfl2rd)) -> nhats6

nhats7%>%
  mutate(air7 = pmax(pk7pkarfl1rd, pk7pkarfl2rd)) -> nhats7

nhats8%>%
  mutate(air8 = pmax(pk8pkarfl1rd, pk8pkarfl2rd)) -> nhats8

nhats9%>%
  mutate(air9 = pmax(pk9pkarfl1rd, pk9pkarfl2rd)) -> nhats9

nhats10[,"air10"] <- NA
nhats11[,"air11"] <- NA

nhats12%>%
  mutate(air12 = pmax(pk12pkarfl1rd, pk12pkarfl2rd)) -> nhats12

####FALL####

nhats1%>%
  mutate(fall1 = as.numeric(case_when(hc1faleninyr ==1 ~ "1",
                                      hc1faleninyr ==2  ~ "2",
                                      hc1faleninyr <=-1~ "NA")))->nhats1

nhats2%>%
  mutate(fall2 = as.numeric(case_when(hc2faleninyr ==1 ~ "1",
                                      hc2faleninyr ==2  ~ "2",
                                      hc2faleninyr <=-1~ "NA")))->nhats2
nhats3%>%
  mutate(fall3 = as.numeric(case_when(hc3faleninyr ==1 ~ "1",
                                      hc3faleninyr ==2  ~ "2",
                                      hc3faleninyr <=-1~ "NA")))->nhats3
nhats4%>%
  mutate(fall4 = as.numeric(case_when(hc4faleninyr ==1 ~ "1",
                                      hc4faleninyr ==2  ~ "2",
                                      hc4faleninyr <=-1~ "NA")))->nhats4
nhats5%>%
  mutate(fall5 = as.numeric(case_when(hc5faleninyr ==1 ~ "1",
                                      hc5faleninyr ==2  ~ "2",
                                      hc5faleninyr <=-1~ "NA")))->nhats5
nhats6%>%
  mutate(fall6 = as.numeric(case_when(hc6faleninyr ==1 ~ "1",
                                      hc6faleninyr ==2  ~ "2",
                                      hc6faleninyr <=-1~ "NA")))->nhats6
nhats7%>%
  mutate(fall7 = as.numeric(case_when(hc7faleninyr ==1 ~ "1",
                                      hc7faleninyr ==2  ~ "2",
                                      hc7faleninyr <=-1~ "NA")))->nhats7
nhats8%>%
  mutate(fall8 = as.numeric(case_when(hc8faleninyr ==1 ~ "1",
                                      hc8faleninyr ==2  ~ "2",
                                      hc8faleninyr <=-1~ "NA")))->nhats8
nhats9%>%
  mutate(fall9 = as.numeric(case_when(hc9faleninyr ==1 ~ "1",
                                      hc9faleninyr ==2  ~ "2",
                                      hc9faleninyr <=-1~ "NA")))->nhats9
nhats10%>%
  mutate(fall10 = as.numeric(case_when(hc10faleninyr ==1 ~ "1",
                                       hc10faleninyr ==2  ~ "2",
                                       hc10faleninyr <=-1~ "NA")))->nhats10
nhats11%>%
  mutate(fall11 = as.numeric(case_when(hc11faleninyr ==1 ~ "1",
                                       hc11faleninyr ==2  ~ "2",
                                       hc11faleninyr <=-1~ "NA")))->nhats11

nhats12%>%
  mutate(fall12 = as.numeric(case_when(hc12faleninyr ==1 ~ "1",
                                       hc12faleninyr ==2  ~ "2",
                                       hc12faleninyr <=-1~ "NA")))->nhats12
####MULTIFALL####

nhats1%>%
  mutate(multifall1 = as.numeric(case_when(hc1multifall  ==1 ~ "1",
                                           hc1multifall  ==2  ~ "2",
                                           hc1multifall  <=-1~ "NA")))->nhats1

nhats2%>%
  mutate(multifall2 = as.numeric(case_when(hc2multifall  ==1 ~ "1",
                                           hc2multifall  ==2  ~ "2",
                                           hc2multifall  <=-1~ "NA")))->nhats2
nhats3%>%
  mutate(multifall3 = as.numeric(case_when(hc3multifall  ==1 ~ "1",
                                           hc3multifall  ==2  ~ "2",
                                           hc3multifall  <=-1~ "NA")))->nhats3
nhats4%>%
  mutate(multifall4 = as.numeric(case_when(hc4multifall  ==1 ~ "1",
                                           hc4multifall  ==2  ~ "2",
                                           hc4multifall  <=-1~ "NA")))->nhats4
nhats5%>%
  mutate(multifall5 = as.numeric(case_when(hc5multifall  ==1 ~ "1",
                                           hc5multifall  ==2  ~ "2",
                                           hc5multifall  <=-1~ "NA")))->nhats5
nhats6%>%
  mutate(multifall6 = as.numeric(case_when(hc6multifall  ==1 ~ "1",
                                           hc6multifall  ==2  ~ "2",
                                           hc6multifall  <=-1~ "NA")))->nhats6
nhats7%>%
  mutate(multifall7 = as.numeric(case_when(hc7multifall  ==1 ~ "1",
                                           hc7multifall  ==2  ~ "2",
                                           hc7multifall  <=-1~ "NA")))->nhats7
nhats8%>%
  mutate(multifall8 = as.numeric(case_when(hc8multifall  ==1 ~ "1",
                                           hc8multifall  ==2  ~ "2",
                                           hc8multifall  <=-1~ "NA")))->nhats8
nhats9%>%
  mutate(multifall9 = as.numeric(case_when(hc9multifall  ==1 ~ "1",
                                           hc9multifall  ==2  ~ "2",
                                           hc9multifall  <=-1~ "NA")))->nhats9
nhats10%>%
  mutate(multifall10 = as.numeric(case_when(hc10multifall  ==1 ~ "1",
                                            hc10multifall  ==2  ~ "2",
                                            hc10multifall  <=-1~ "NA")))->nhats10
nhats11%>%
  mutate(multifall11 = as.numeric(case_when(hc11multifall  ==1 ~ "1",
                                            hc11multifall  ==2  ~ "2",
                                            hc11multifall  <=-1~ "NA")))->nhats11

nhats12%>%
  mutate(multifall12 = as.numeric(case_when(hc12multifall  ==1 ~ "1",
                                            hc12multifall  ==2  ~ "2",
                                            hc12multifall  <=-1~ "NA")))->nhats12

####FEARFALL####

nhats1%>%
  mutate(fearfall1 = as.numeric(case_when(hc1worryfall ==1 ~ "1",
                                          hc1worryfall ==2  ~ "2",
                                          hc1worryfall <=-1~ "NA")))->nhats1

nhats2%>%
  mutate(fearfall2 = as.numeric(case_when(hc2worryfall ==1 ~ "1",
                                          hc2worryfall ==2  ~ "2",
                                          hc2worryfall <=-1~ "NA")))->nhats2
nhats3%>%
  mutate(fearfall3 = as.numeric(case_when(hc3worryfall ==1 ~ "1",
                                          hc3worryfall ==2  ~ "2",
                                          hc3worryfall <=-1~ "NA")))->nhats3
nhats4%>%
  mutate(fearfall4 = as.numeric(case_when(hc4worryfall ==1 ~ "1",
                                          hc4worryfall ==2  ~ "2",
                                          hc4worryfall <=-1~ "NA")))->nhats4
nhats5%>%
  mutate(fearfall5 = as.numeric(case_when(hc5worryfall ==1 ~ "1",
                                          hc5worryfall ==2  ~ "2",
                                          hc5worryfall <=-1~ "NA")))->nhats5
nhats6%>%
  mutate(fearfall6 = as.numeric(case_when(hc6faleninyr ==1 ~ "1",
                                          hc6worryfall ==2  ~ "2",
                                          hc6worryfall <=-1~ "NA")))->nhats6
nhats7%>%
  mutate(fearfall7 = as.numeric(case_when(hc7worryfall ==1 ~ "1",
                                          hc7worryfall ==2  ~ "2",
                                          hc7worryfall <=-1~ "NA")))->nhats7
nhats8%>%
  mutate(fearfall8 = as.numeric(case_when(hc8worryfall ==1 ~ "1",
                                          hc8worryfall ==2  ~ "2",
                                          hc8worryfall <=-1~ "NA")))->nhats8
nhats9%>%
  mutate(fearfall9 = as.numeric(case_when(hc9worryfall ==1 ~ "1",
                                          hc9worryfall ==2  ~ "2",
                                          hc9worryfall <=-1~ "NA")))->nhats9
nhats10%>%
  mutate(fearfall10 = as.numeric(case_when(hc10worryfall ==1 ~ "1",
                                           hc10worryfall ==2  ~ "2",
                                           hc10worryfall <=-1~ "NA")))->nhats10
nhats11%>%
  mutate(fearfall11 = as.numeric(case_when(hc11worryfall ==1 ~ "1",
                                           hc11worryfall ==2  ~ "2",
                                           hc11worryfall <=-1~ "NA")))->nhats11
nhats12%>%
  mutate(fearfall12 = as.numeric(case_when(hc12worryfall ==1 ~ "1",
                                           hc12worryfall ==2  ~ "2",
                                           hc12worryfall <=-1~ "NA")))->nhats12


####5STS test####

#removing -1 for 5STS-seconds

nhats1$ch1chstndsec<-ifelse(nhats1$ch1chstndsec<=-1, NA, nhats1$ch1chstndsec) 
nhats2$ch2chstndsec<-ifelse(nhats2$ch2chstndsec<=-1, NA, nhats2$ch2chstndsec) 
nhats3$ch3chstndsec<-ifelse(nhats3$ch3chstndsec<=-1, NA, nhats3$ch3chstndsec) 
nhats4$ch4chstndsec<-ifelse(nhats4$ch4chstndsec<=-1, NA, nhats4$ch4chstndsec) 
nhats5$ch5chstndsec<-ifelse(nhats5$ch5chstndsec<=-1, NA, nhats5$ch5chstndsec) 
nhats6$ch6chstndsec<-ifelse(nhats6$ch6chstndsec<=-1, NA, nhats6$ch6chstndsec) 
nhats7$ch7chstndsec<-ifelse(nhats7$ch7chstndsec<=-1, NA, nhats7$ch7chstndsec) 
nhats8$ch8chstndsec<-ifelse(nhats8$ch8chstndsec<=-1, NA, nhats8$ch8chstndsec) 
nhats9$ch9chstndsec<-ifelse(nhats9$ch9chstndsec<=-1, NA, nhats9$ch9chstndsec) 
nhats11$ch11chstndsec<-ifelse(nhats11$ch11chstndsec<=-1, NA, nhats11$ch11chstndsec) 
nhats12$ch12chstndsec<-ifelse(nhats12$ch12chstndsec<=-1, NA, nhats12$ch12chstndsec) 

#removing -1 for 5STS-centesima
nhats1$ch1chstdhndr<-ifelse(nhats1$ch1chstdhndr<=-1, NA, nhats1$ch1chstdhndr) 
nhats2$ch2chstdhndr<-ifelse(nhats2$ch2chstdhndr<=-1, NA, nhats2$ch2chstdhndr) 
nhats3$ch3chstdhndr<-ifelse(nhats3$ch3chstdhndr<=-1, NA, nhats3$ch3chstdhndr) 
nhats4$ch4chstdhndr<-ifelse(nhats4$ch4chstdhndr<=-1, NA, nhats4$ch4chstdhndr) 
nhats5$ch5chstdhndr<-ifelse(nhats5$ch5chstdhndr<=-1, NA, nhats5$ch5chstdhndr) 
nhats6$ch6chstdhndr<-ifelse(nhats6$ch6chstdhndr<=-1, NA, nhats6$ch6chstdhndr) 
nhats7$ch7chstdhndr<-ifelse(nhats7$ch7chstdhndr<=-1, NA, nhats7$ch7chstdhndr) 
nhats8$ch8chstdhndr<-ifelse(nhats8$ch8chstdhndr<=-1, NA, nhats8$ch8chstdhndr) 
nhats9$ch9chstdhndr<-ifelse(nhats9$ch9chstdhndr<=-1, NA, nhats9$ch9chstdhndr) 
nhats11$ch11chstdhndr<-ifelse(nhats11$ch11chstdhndr<=-1, NA, nhats11$ch11chstdhndr) 
nhats12$ch12chstdhndr<-ifelse(nhats12$ch12chstdhndr<=-1, NA, nhats12$ch12chstdhndr) 

#calculate 5sts total secs
nhats1%>%
  mutate(sts1 = as.numeric(ch1chstndsec + ch1chstdhndr / 100 )) -> nhats1

nhats2%>%
  mutate(sts2 = as.numeric(ch2chstndsec + ch2chstdhndr / 100 )) -> nhats2

nhats3%>%
  mutate(sts3 = as.numeric(ch3chstndsec + ch3chstdhndr / 100 )) -> nhats3

nhats4%>%
  mutate(sts4 = as.numeric(ch4chstndsec + ch4chstdhndr / 100 )) -> nhats4

nhats5%>%
  mutate(sts5 = as.numeric(ch5chstndsec + ch5chstdhndr / 100 )) -> nhats5

nhats6%>%
  mutate(sts6 = as.numeric(ch6chstndsec + ch6chstdhndr / 100 )) -> nhats6

nhats7%>%
  mutate(sts7 = as.numeric(ch7chstndsec + ch7chstdhndr / 100 )) -> nhats7

nhats8%>%
  mutate(sts8 = as.numeric(ch8chstndsec + ch8chstdhndr / 100 )) -> nhats8

nhats9%>%
  mutate(sts9 = as.numeric(ch9chstndsec + ch9chstdhndr / 100 )) -> nhats9

nhats11%>%
  mutate(sts11 = as.numeric(ch11chstndsec + ch11chstdhndr / 100 )) -> nhats11

nhats12%>%
  mutate(sts12 = as.numeric(ch12chstndsec + ch12chstdhndr / 100 )) -> nhats12



# Reemplazar los valores en sts1 por NA según la condición en ch1chstddone o si ha tardado menos de 5sec


nhats1 <- nhats1 %>% 
  mutate(sts1 = ifelse(ch1chstndsec < 5 | ch1chstddone %in% c(1, 2, 3, 4) | ch1chstntdn1 ==1 | ch1chstntdn2 ==1 | ch1chstntdn3 ==1 | ch1chstntdn4 ==1 | ch1chstntdn5 ==1 | ch1chstntdn9 ==1, NA, sts1))

nhats2 <- nhats2 %>% 
  mutate(sts2 = ifelse(ch2chstndsec < 5 | ch2chstddone %in% c(1, 2, 3, 4) | ch2chstntdn1 ==1 | ch2chstntdn2 ==1 | ch2chstntdn3 ==1 | ch2chstntdn4 ==1 | ch2chstntdn5 ==1 | ch2chstntdn9 ==1, NA, sts2))

nhats3 <- nhats3 %>% 
  mutate(sts3 = ifelse(ch3chstndsec < 5 | ch3chstddone %in% c(1, 2, 3, 4) | ch3chstntdn1 ==1 | ch3chstntdn2 ==1 | ch3chstntdn3 ==1 | ch3chstntdn4 ==1 | ch3chstntdn5 ==1 | ch3chstntdn9 ==1, NA, sts3))

nhats4 <- nhats4 %>% 
  mutate(sts4 = ifelse(ch4chstndsec < 5 | ch4chstddone %in% c(1, 2, 3, 4) | ch4chstntdn1 ==1 | ch4chstntdn2 ==1 | ch4chstntdn3 ==1 | ch4chstntdn4 ==1 | ch4chstntdn5 ==1 | ch4chstntdn9 ==1, NA, sts4))

nhats5 <- nhats5 %>% 
  mutate(sts5 = ifelse(ch5chstndsec < 5 | ch5chstddone %in% c(1, 2, 3, 4) | ch5chstntdn1 ==1 | ch5chstntdn2 ==1 | ch5chstntdn3 ==1 | ch5chstntdn4 ==1 | ch5chstntdn5 ==1 | ch5chstntdn9 ==1, NA, sts5))

nhats6 <- nhats6 %>% 
  mutate(sts6 = ifelse(ch6chstndsec < 5 | ch6chstddone %in% c(1, 2, 3, 4) | ch6chstntdn1 ==1 | ch6chstntdn2 ==1 | ch6chstntdn3 ==1 | ch6chstntdn4 ==1 | ch6chstntdn5 ==1 | ch6chstntdn9 ==1, NA, sts6))

nhats7 <- nhats7 %>% 
  mutate(sts7 = ifelse(ch7chstndsec < 5 | ch7chstddone %in% c(1, 2, 3, 4) | ch7chstntdn1 ==1 | ch7chstntdn2 ==1 | ch7chstntdn3 ==1 | ch7chstntdn4 ==1 | ch7chstntdn5 ==1 | ch7chstntdn9 ==1, NA, sts7))

nhats8 <- nhats8 %>% 
  mutate(sts8 = ifelse(ch8chstndsec < 5 | ch8chstddone %in% c(1, 2, 3, 4) | ch8chstntdn1 ==1 | ch8chstntdn2 ==1 | ch8chstntdn3 ==1 | ch8chstntdn4 ==1 | ch8chstntdn5 ==1 | ch8chstntdn9 ==1, NA, sts8))

nhats9 <- nhats9 %>% 
  mutate(sts9 = ifelse(ch9chstndsec < 5 | ch9chstddone %in% c(1, 2, 3, 4) | ch9chstntdn1 ==1 | ch9chstntdn2 ==1 | ch9chstntdn3 ==1 | ch9chstntdn4 ==1 | ch9chstntdn5 ==1 | ch9chstntdn9 ==1, NA, sts9))

nhats11 <- nhats11 %>% 
  mutate(sts11 = ifelse(ch11chstndsec < 5 | ch11chstddone %in% c(1, 2, 3, 4) | ch11chstntdn1 ==1 | ch11chstntdn2 ==1 | ch11chstntdn3 ==1 | ch11chstntdn4 ==1 | ch11chstntdn5 ==1 | ch11chstntdn9 ==1, NA, sts11))

nhats12 <- nhats12 %>% 
  mutate(sts12 = ifelse(ch12chstndsec < 5 | ch12chstddone %in% c(1, 2, 3, 4) | ch12chstntdn1 ==1 | ch12chstntdn2 ==1 | ch12chstntdn3 ==1 | ch12chstntdn4 ==1 | ch12chstntdn5 ==1 | ch12chstntdn9 ==1, NA, sts12))


####smoking status####

nhats1%>%
  mutate(smoking_st1 = as.numeric(case_when(smk_before1 ==1 & smk_now1 ==0 ~ "2",#this is former smoker
                                            smk_now1 ==1  ~ "3",#this is current smoker
                                            smk_before1 ==0  ~ "1")))->nhats1  #this (0) is never smoker  

nhats2%>%
  mutate(smoking_st2 = as.numeric(case_when(smk_before2 ==1 & smk_now2 ==0 ~ "2",#this is former smoker
                                            smk_now2 ==1  ~ "3",#this is current smoker
                                            smk_before2 ==0  ~ "1")))->nhats2  #this (0) is never smoker  

nhats3%>%
  mutate(smoking_st3 = as.numeric(case_when(smk_before3 ==1 & smk_now3 ==0 ~ "2",#this is former smoker
                                            smk_now3 ==1  ~ "3",#this is current smoker
                                            smk_before3 ==0  ~ "1")))->nhats3  #this (0) is never smoker  

nhats4%>%
  mutate(smoking_st4 = as.numeric(case_when(smk_before4 ==1 & smk_now4 ==0 ~ "2",#this is former smoker
                                            smk_now4 ==1  ~ "3",#this is current smoker
                                            smk_before4 ==0  ~ "1")))->nhats4  #this (0) is never smoker  

nhats5%>%
  mutate(smoking_st5 = as.numeric(case_when(smk_before5 ==1 & smk_now5 ==0 ~ "2",#this is former smoker
                                            smk_now5 ==1  ~ "3",#this is current smoker
                                            smk_before5 ==0  ~ "1")))->nhats5  #this (0) is never smoker  

nhats6%>%
  mutate(smoking_st6 = as.numeric(case_when(smk_before6 ==1 & smk_now6 ==0 ~ "2",#this is former smoker
                                            smk_now6 ==1  ~ "3",#this is current smoker
                                            smk_before6 ==0  ~ "1")))->nhats6  #this (0) is never smoker  

nhats7%>%
  mutate(smoking_st7 = as.numeric(case_when(smk_before7 ==1 & smk_now7 ==0 ~ "2",#this is former smoker
                                            smk_now7 ==1  ~ "3",#this is current smoker
                                            smk_before7 ==0  ~ "1")))->nhats7  #this (0) is never smoker  

nhats8%>%
  mutate(smoking_st8 = as.numeric(case_when(smk_before8 ==1 & smk_now8 ==0 ~ "2",#this is former smoker
                                            smk_now8 ==1  ~ "3",#this is current smoker
                                            smk_before8 ==0  ~ "1")))->nhats8  #this (0) is never smoker  

nhats9%>%
  mutate(smoking_st9 = as.numeric(case_when(smk_before9 ==1 & smk_now9 ==0 ~ "2",#this is former smoker
                                            smk_now9 ==1  ~ "3",#this is current smoker
                                            smk_before9 ==0  ~ "1")))->nhats9  #this (0) is never smoker  

nhats10%>%
  mutate(smoking_st10 = as.numeric(case_when(smk_before10 ==1 & smk_now10 ==0 ~ "2",#this is former smoker
                                             smk_now10 ==1  ~ "3",#this is current smoker
                                             smk_before10 ==0  ~ "1")))->nhats10  #this (0) is never smoker  

nhats11%>%
  mutate(smoking_st11 = as.numeric(case_when(smk_before11 ==1 & smk_now11 ==0 ~ "2",#this is former smoker
                                             smk_now11 ==1  ~ "3",#this is current smoker
                                             smk_before11 ==0  ~ "1")))->nhats11  #this (0) is never smoker  

nhats12%>%
  mutate(smoking_st12 = as.numeric(case_when(smk_before12 ==1 & smk_now12 ==0 ~ "2",#this is former smoker
                                             smk_now12 ==1  ~ "3",#this is current smoker
                                             smk_before12 ==0  ~ "1")))->nhats12  #this (0) is never smoker  

####sppb_rec####
nhats1%>%
  mutate(sppb_rec1= as.factor(case_when(sppb1<10~"1",
                                        sppb1>=10 ~"0")))->nhats1

nhats2%>%
  mutate(sppb_rec2= as.factor(case_when(sppb2<10~"1",
                                        sppb2>=10 ~"0")))->nhats2

nhats3%>%
  mutate(sppb_rec3= as.factor(case_when(sppb3<10~"1",
                                        sppb3>=10 ~"0")))->nhats3

nhats4%>%
  mutate(sppb_rec4= as.factor(case_when(sppb4<10~"1",
                                        sppb4>=10 ~"0")))->nhats4

nhats5%>%
  mutate(sppb_rec5= as.factor(case_when(sppb5<10~"1",
                                        sppb5>=10 ~"0")))->nhats5

nhats6%>%
  mutate(sppb_rec6= as.factor(case_when(sppb6<10~"1",
                                        sppb6>=10 ~"0")))->nhats6

nhats7%>%
  mutate(sppb_rec7= as.factor(case_when(sppb7<10~"1",
                                        sppb7>=10 ~"0")))->nhats7

nhats8%>%
  mutate(sppb_rec8= as.factor(case_when(sppb8<10~"1",
                                        sppb8>=10 ~"0")))->nhats8

nhats9%>%
  mutate(sppb_rec9= as.factor(case_when(sppb9<10~"1",
                                        sppb9>=10 ~"0")))->nhats9

nhats11%>%
  mutate(sppb_rec11= as.factor(case_when(sppb11<10~"1",
                                         sppb11>=10 ~"0")))->nhats11

nhats12%>%
  mutate(sppb_rec12= as.factor(case_when(sppb12<10~"1",
                                         sppb12>=10 ~"0")))->nhats12

####eth####
nhats1%>%
  mutate(eth = as.numeric(case_when(rl1yourrace1 ==1  ~ "1",
                                    rl1yourrace1 ==2  ~ "0",
                                    rl1yourrace1 <=-1 ~ "NA")))->nhats1
nhats2[,"eth"] <- NA
nhats3[,"eth"] <- NA
nhats4[,"eth"] <- NA
nhats5%>%
  mutate(eth = as.numeric(case_when(rl5yourrace1 ==1  ~ "1",
                                    rl5yourrace1 ==2  ~ "0",
                                    rl5yourrace1 <=-1 ~ "NA")))->nhats5
nhats6[,"eth"] <- NA
nhats7[,"eth"] <- NA
nhats8[,"eth"] <- NA
nhats9[,"eth"] <- NA
nhats10[,"eth"] <- NA
nhats11[,"eth"] <- NA
nhats12%>%
  mutate(eth = as.numeric(case_when(rl12dyourrace1 ==1  ~ "1",
                                    rl12dyourrace1 ==2  ~ "0",
                                    rl12dyourrace1 <=-1 ~ "NA")))->nhats12

####DEMENTIA####
#####R1####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia1 <- read_sas("/Users/danielgallardogomez/Downloads/SP_1")

#2. ENTER WHICH ROUND?
demencia1 <- demencia1 %>% 
  mutate(rnd = 1) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia1 <- demencia1 %>% 
  rename_all(~stringr::str_replace(.,"^r1","")) %>%
  rename_all(~stringr::str_replace(.,"^hc1","")) %>% 
  rename_all(~stringr::str_replace(.,"^is1","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp1","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg1",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia1 <- demencia1 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia1 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df1<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))


#####R2####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia2 <- read_sas("/Users/danielgallardogomez/Downloads/SP_2")

#2. ENTER WHICH ROUND?
demencia2 <- demencia2 %>% 
  mutate(rnd = 2) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia2 <- demencia2 %>% 
  rename_all(~stringr::str_replace(.,"^r2","")) %>%
  rename_all(~stringr::str_replace(.,"^hc2","")) %>% 
  rename_all(~stringr::str_replace(.,"^is2","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp2","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg2",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia2 <- demencia2 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia2 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df2<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))


#####R3####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia3 <- read_sas("/Users/danielgallardogomez/Downloads/SP_3")

#2. ENTER WHICH ROUND?
demencia3 <- demencia3 %>% 
  mutate(rnd = 3) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia3 <- demencia3 %>% 
  rename_all(~stringr::str_replace(.,"^r3","")) %>%
  rename_all(~stringr::str_replace(.,"^hc3","")) %>% 
  rename_all(~stringr::str_replace(.,"^is3","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp3","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg3",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia3 <- demencia3 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia3 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df3<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R4####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia4 <- read_sas("/Users/danielgallardogomez/Downloads/SP_4")

#2. ENTER WHICH ROUND?
demencia4 <- demencia4 %>% 
  mutate(rnd = 4) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia4 <- demencia4 %>% 
  rename_all(~stringr::str_replace(.,"^r4","")) %>%
  rename_all(~stringr::str_replace(.,"^hc4","")) %>% 
  rename_all(~stringr::str_replace(.,"^is4","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp4","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg4",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia4 <- demencia4 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia4 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df4<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R5####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia5 <- read_sas("/Users/danielgallardogomez/Downloads/SP_5")

#2. ENTER WHICH ROUND?
demencia5 <- demencia5 %>% 
  mutate(rnd = 5) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia5 <- demencia5 %>% 
  rename_all(~stringr::str_replace(.,"^r5","")) %>%
  rename_all(~stringr::str_replace(.,"^hc5","")) %>% 
  rename_all(~stringr::str_replace(.,"^is5","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp5","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg5",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia5 <- demencia5 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia5 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df5<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R6####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia6 <- read_sas("/Users/danielgallardogomez/Downloads/SP_6")

#2. ENTER WHICH ROUND?
demencia6 <- demencia6 %>% 
  mutate(rnd = 6) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia6 <- demencia6 %>% 
  rename_all(~stringr::str_replace(.,"^r6","")) %>%
  rename_all(~stringr::str_replace(.,"^hc6","")) %>% 
  rename_all(~stringr::str_replace(.,"^is6","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp6","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg6",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia6 <- demencia6 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia6 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df6<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R7####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia7 <- read_sas("/Users/danielgallardogomez/Downloads/SP_7")

#2. ENTER WHICH ROUND?
demencia7 <- demencia7 %>% 
  mutate(rnd = 7) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia7 <- demencia7 %>% 
  rename_all(~stringr::str_replace(.,"^r7","")) %>%
  rename_all(~stringr::str_replace(.,"^hc7","")) %>% 
  rename_all(~stringr::str_replace(.,"^is7","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp7","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg7",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia7 <- demencia7 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia7 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df7<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R8####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia8 <- read_sas("/Users/danielgallardogomez/Downloads/SP_8")

#2. ENTER WHICH ROUND?
demencia8 <- demencia8 %>% 
  mutate(rnd = 8) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia8 <- demencia8 %>% 
  rename_all(~stringr::str_replace(.,"^r8","")) %>%
  rename_all(~stringr::str_replace(.,"^hc8","")) %>% 
  rename_all(~stringr::str_replace(.,"^is8","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp8","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg8",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia8 <- demencia8 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia8 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df8<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R9####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia9 <- read_sas("/Users/danielgallardogomez/Downloads/SP_9")

#2. ENTER WHICH ROUND?
demencia9 <- demencia9 %>% 
  mutate(rnd = 9) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia9 <- demencia9 %>% 
  rename_all(~stringr::str_replace(.,"^r9","")) %>%
  rename_all(~stringr::str_replace(.,"^hc9","")) %>% 
  rename_all(~stringr::str_replace(.,"^is9","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp9","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg9",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia9 <- demencia9 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia9 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df9<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R10####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia10 <- read_sas("/Users/danielgallardogomez/Downloads/SP_10")

#2. ENTER WHICH ROUND?
demencia10 <- demencia10 %>% 
  mutate(rnd = 10) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia10 <- demencia10 %>% 
  rename_all(~stringr::str_replace(.,"^r10","")) %>%
  rename_all(~stringr::str_replace(.,"^hc10","")) %>% 
  rename_all(~stringr::str_replace(.,"^is10","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp10","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg10",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia10 <- demencia10 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia10 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df10<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R11####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia11 <- read_sas("/Users/danielgallardogomez/Downloads/SP_11")

#2. ENTER WHICH ROUND?
demencia11 <- demencia11 %>% 
  mutate(rnd = 11) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia11 <- demencia11 %>% 
  rename_all(~stringr::str_replace(.,"^r11","")) %>%
  rename_all(~stringr::str_replace(.,"^hc11","")) %>% 
  rename_all(~stringr::str_replace(.,"^is11","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp11","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg11",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1
demencia11 <- demencia11 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))

#SUBSET NEEDED VARIABLES
df<-demencia11 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)

#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df11<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))

#####R12####

#1. ENTER DESIRED SP FILE IN THE PARENTHESES
demencia12 <- read_sas("/Users/danielgallardogomez/Downloads/SP_12")

#2. ENTER WHICH ROUND?
demencia12 <- demencia12 %>% 
  mutate(rnd = 12) 

#3. EDIT ROUND NUMBER INSIDE THE QUOTES 
#(THIS REMOVES THE PREFIXES ON NEEDED VARIABLES ) 
demencia12 <- demencia12 %>% 
  rename_all(~stringr::str_replace(.,"^r12","")) %>%
  rename_all(~stringr::str_replace(.,"^hc12","")) %>% 
  rename_all(~stringr::str_replace(.,"^is12","")) %>% 
  rename_all(~stringr::str_replace(.,"^cp12","")) %>% 
  rename_all(~stringr::str_replace(.,"^cg12",""))


#ADD R1DAD8DEM AND SET TO -1 FOR ROUND 1 BECAUSE THERE IS NO PRIOR DIAGNOSIS IN R1_ variable dad8dem en r12 es cp12dad8dem
demencia12 <- demencia12 %>% 
  mutate(dad8dem = ifelse(rnd == 1, -1, dad8dem))


#SUBSET NEEDED VARIABLES
df<-demencia12 %>% 
  dplyr::select(spid, rnd, dresid, resptype, disescn9, chgthink1, chgthink2, chgthink3, chgthink4, chgthink5, chgthink6, chgthink7, chgthink8, dad8dem,
                speaktosp, todaydat1, todaydat2, todaydat3, todaydat4, todaydat5, presidna1, presidna3, vpname1, vpname3, quesremem, dclkdraw, atdrwclck, 
                dwrdimmrc, dwrdlstnm, dwrddlyrc)


#FIX A ROUND 2 CODING ERROR#
df <- df %>%
  mutate(dwrdimmrc = ifelse(dwrdimmrc==10 & dwrddlyrc==-3 & rnd==2, -3, dwrdimmrc))

#CREATE SELECTED ROUND DEMENTIA CLASSIFICATION VARIABLE 
df <- df %>%
  mutate(demclas  =  ifelse(dresid==3 | dresid==5 | dresid==7, -9, #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                            ifelse((dresid==4 & rnd==1) | dresid==6 | dresid==8, -1,                #SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED)
                                   ifelse((disescn9==1 | disescn9==7) &           #CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY*
                                            (resptype==1 | resptype==2), 1, NA))))

#CODE AD8_SCORE*
#INITIALIZE COUNTS TO NOT APPLICABLE*
#ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS 
for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("chgthink", i, sep = "")]]==2 & df$resptype==2 & is.na(df$demclas), 0, #PROXY REPORTS NO CHANGE
                                                         ifelse((df[[paste("chgthink", i, sep = "")]]==1 | df[[paste("chgthink", i, sep = "")]] == 3) & df$resptype==2 & is.na(df$demclas), 1, #PROXY REPORTS A CHANGE OR ALZ/DEMENTIA*
                                                                ifelse(df$resptype==2 & is.na(df$demclas), NA, -1))))    #SET TO NA IF IN RES CARE AND demclass=., OTHERWISE AD8 ITEM IS SET TO NOT APPLICABLE                                                                                                                        
}

#INITIALIZE COUNTS TO NOT APPLICABLE*
for(i in 1:8){
  df[[paste("ad8miss_", i, sep = "")]]  <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]), 1,
                                                             ifelse((df[[paste("ad8_", i, sep = "")]]==0 | df[[paste("ad8_", i, sep = "")]]==1) & df$resptype==2 & is.na(df$demclas), 0, -1)))
}

for(i in 1:8){
  df[[paste("ad8_", i, sep = "")]] <- as.numeric(ifelse(is.na(df[[paste("ad8_", i, sep = "")]]) & is.na(df$demclas) & df$resptype==2, 0, df[[paste("ad8_", i, sep = "")]]))
}

#COUNT AD8 ITEMS
#ROUNDS 2+
df <- df %>%
  mutate(ad8_score = ifelse(resptype==2 & is.na(demclas), (ad8_1 + ad8_2 + ad8_3 + ad8_4 + ad8_5 + ad8_6 + ad8_7 + ad8_8), -1)) %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 
  mutate(ad8_score = ifelse(dad8dem==1 & resptype==2 & is.na(demclas), 8, ad8_score))  %>% 
  #SET PREVIOUS ROUND DEMENTIA DIAGNOSIS BASED ON AD8 TO AD8_SCORE=8 FOR ROUNDS 4-9
  mutate(ad8_score = ifelse(resptype==2 & dad8dem==-1 & chgthink1==-1 & (rnd>=4 & rnd<=9), 8, ad8_score)) 

#COUNT MISSING AD8 ITEMS
df <- df %>% 
  mutate(ad8_miss = ifelse(resptype==2 & is.na(demclas),(ad8miss_1+ad8miss_2+ad8miss_3+ad8miss_4+ad8miss_5+ad8miss_6+ad8miss_7+ad8miss_8), -1))

#CODE AD8 DEMENTIA CLASS 
#IF SCORE>=2 THEN MEETS AD8 CRITERIA
#IF SCORE IS 0 OR 1 THEN DOES NOT MEET AD8 CRITERIA
df <- df %>% 
  mutate(ad8_dem = ifelse(ad8_score>=2, 1,
                          ifelse(ad8_score==0 | ad8_score==1 | ad8_miss==8, 2, NA)))

#UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS
df <- df %>% 
  #PROBABLE DEMENTIA BASED ON AD8 SCORE  
  mutate(demclas = ifelse(ad8_dem==1 & is.na(demclas), 1, 
                          #NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS*
                          ifelse(ad8_dem==2 & speaktosp==2 & is.na(demclas), 3, demclas)))


####CODE DATE ITEMS AND COUNT 
#CODE ONLY YES/NO RESPONSES: MISSING/NA CODES -1, -9 LEFT MISSING*
#2: NO/DK OR -7: REFUSED RECODED TO : NO/DK/RF*
#****ADD NOTES HERE ABOUT WHAT IS HAPPENING IN ROUNDS 1-3, 5+ VS. ROUND 4 
#*
for(i in 1:5){
  df[[paste("date_item", i, sep = "")]]  <- as.numeric(ifelse(df[[paste("todaydat", i, sep = "")]]==1, 1,
                                                              ifelse(df[[paste("todaydat", i, sep = "")]]==2 | df[[paste("todaydat", i, sep = "")]]== -7, 0, NA)))
}

#COUNT CORRECT DATE ITEMS
df <- df %>% 
  mutate(date_item4 = ifelse(rnd==4, date_item5, date_item4)) %>% 
  mutate(date_sum = date_item1 + date_item2 + date_item3 + date_item4) %>% 
  
  #PROXY SAYS CAN'T SPEAK TO SP
  mutate(date_sum = ifelse(speaktosp==2 & is.na(date_sum),-2,  
                           #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER*
                           ifelse((is.na(date_item1) | is.na(date_item2) | is.na(date_item3) | is.na(date_item4)) & speaktosp==1,-3, date_sum))) %>% 
  
  #MISSING IF PROXY SAYS CAN'T SPEAK TO SP*  
  mutate(date_sumr = ifelse(date_sum == -2 , NA, 
                            #0 IF SP UNABLE TO ANSWER*
                            ifelse(date_sum == -3 , 0, date_sum)))


#PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT#
##CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING *
##2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF*
df <- df %>% 
  mutate(preslast = ifelse(presidna1 == 1, 1,
                           ifelse(presidna1 == 2 | presidna1 == -7, 0, NA))) %>% 
  mutate(presfirst = ifelse(presidna3 == 1, 1,
                            ifelse(presidna3 == 2 | presidna3 == -7, 0, NA))) %>% 
  mutate(vplast = ifelse(vpname1 == 1, 1,
                         ifelse(vpname1 == 2 | vpname1 == -7, 0, NA))) %>% 
  mutate(vpfirst = ifelse(vpname3 == 1, 1,
                          ifelse(vpname3 == 2 | vpname3 == -7, 0, NA))) %>% 
  
  #COUNT CORRECT PRESIDENT/VP NAME ITEMS*
  mutate(presvp = preslast + presfirst + vplast + vpfirst) %>% 
  #PROXY SAYS CAN'T SPEAK TO SP 
  mutate(presvp = ifelse(speaktosp == 2 & is.na(presvp), -2, 
                         #PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER                           
                         ifelse((is.na(preslast) | is.na(presfirst) | is.na(vplast) | is.na(vpfirst)) & speaktosp==1 & is.na(presvp),-3, presvp))) %>% 
  
  #MISSING IF PROXY SAYS CAN’T SPEAK TO SP*
  mutate(presvpr =  ifelse(presvp == -2 , NA, 
                           ifelse(presvp == -3 , 0, presvp))) %>% 
  
  #ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT/VP NAMING* 
  mutate(date_prvp = date_sumr + presvpr)


#EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE#
#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUND 10 ONLY)* 
df <- df %>% 
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd==10, -2,
                           ifelse(speaktosp==1 & (quesremem==2 | quesremem==-7 | quesremem==-8) & dclkdraw==-9 & rnd==10, -3,
                                  ifelse(atdrwclck==2 & dclkdraw==-9 & rnd==10, -4,
                                         ifelse(atdrwclck==97 & dclkdraw==-9 & rnd==10, -7, dclkdraw)))))

#RECODE DCLKDRAW TO ALIGN WITH MISSING VALUES IN PREVIOUS ROUNDS (ROUNDS 11 AND FORWARD ONLY)* 
df<-df  %>%
  mutate(dclkdraw = ifelse(speaktosp == 2 & dclkdraw == -9 & rnd>=11, -2, 
                           ifelse(speaktosp == 1 & (quesremem == 2 | quesremem == -7 | quesremem == -8) & dclkdraw == -9, -3 & rnd>=11, dclkdraw))) 
df<-df  %>%
  mutate(clock_scorer = ifelse(dclkdraw == -3 | dclkdraw == -4 | dclkdraw == -7, 0,
                               #IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK*
                               #IF PROXY SAID CAN ASK SP*
                               ifelse(dclkdraw == -9 & speaktosp == 1, 2, 
                                      #IF SELF-RESPONDENT*       
                                      ifelse(dclkdraw == -9 & speaktosp == -1, 3, 
                                             ifelse(dclkdraw == -2 | dclkdraw == -9, NA, dclkdraw)))))


#MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL 
df <- df %>%
  mutate(irecall  =  ifelse(dwrdimmrc == -2 | dwrdimmrc == -1, NA,
                            ifelse(dwrdimmrc == -7 | dwrdimmrc == -3, 0, dwrdimmrc))) %>% 
  mutate(irecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, irecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(drecall  =  ifelse(dwrddlyrc == -2 | dwrddlyrc == -1, NA,
                            ifelse(dwrddlyrc == -7 | dwrddlyrc == -3, 0, dwrddlyrc))) %>% 
  mutate(drecall = ifelse(rnd==5 & dwrddlyrc==-9, NA, drecall)) %>%  #round 5 only: set cases with missing word list and not previously assigned to missing
  
  mutate(wordrecall0_20 = irecall+drecall)


#CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE 

df<-df %>% 
  mutate(clock65 = ifelse(clock_scorer == 0 | clock_scorer==1, 1, 
                          ifelse(clock_scorer > 1 & clock_scorer<6, 0, NA)))

df<-df %>%  
  mutate(word65 = ifelse(wordrecall0_20 >= 0 & wordrecall0_20 <=3, 1, 
                         ifelse(wordrecall0_20 > 3 & wordrecall0_20 <=20, 0, NA)))

df<-df %>%  
  mutate(datena65 = ifelse(date_prvp >= 0 & date_prvp <=3, 1, 
                           ifelse(date_prvp > 3 & date_prvp <= 8, 0, NA)))

#  *CREATE COGNITIVE DOMAIN SCORE*
df<-df %>% 
  mutate(domain65 = clock65+word65+datena65)

#*SET CASES WITH MISSING WORD LIST AND NOT PREVIOUSLY ASSIGNED TO MISSING (ROUND 5 ONLY)
df<-df %>%   
  mutate(demclas = ifelse(rnd==5 & dwrdlstnm==-9 & is.na(demclas), -9, demclas))

#UPDATE COGNITIVE CLASSIFICATION*
df12<-df %>% 
  #PROBABLE DEMENTIA
  mutate(demclas = ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & (domain65==2 | domain65==3), 1,
                          #POSSIBLE DEMENTIA
                          ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==1, 2,
                                 #NO DEMENITA                    
                                 ifelse(is.na(demclas) & (speaktosp == 1 | speaktosp == -1) & domain65==0, 3, demclas))))



####get datasets with all variables of interest####meter smoking_st y sppb_rec en lugar de los 2 smoke


nhats1<-left_join(nhats1,df1, by = "spid")
nhats2<-left_join(nhats2,df2, by = "spid")
nhats3<-left_join(nhats3,df3, by = "spid")
nhats4<-left_join(nhats4,df4, by = "spid")
nhats5<-left_join(nhats5,df5, by = "spid")
nhats6<-left_join(nhats6,df6, by = "spid")
nhats7<-left_join(nhats7,df7, by = "spid")
nhats8<-left_join(nhats8,df8, by = "spid")
nhats9<-left_join(nhats9,df9, by = "spid")
nhats10<-left_join(nhats10,df10 , by = "spid")
nhats11<-left_join(nhats11,df11, by = "spid")
nhats12<-left_join(nhats12,df12, by = "spid")


####Example get datasets with all variables of interest####

# add column age_died manually
nhats1$age_died <- NA

nhats1%>%
  dplyr::select(spid, sppb1, demclas, dementia1, age_died, hc1dementage, sociso_index, wc1,dep1_index_rec,ax1_index_rec, chr_cond1, smk_before1, smk_now1,pain, exercise, income, sex, dis_issues, hear_issues, emp,res, hosp_stay_n1, frailty1, frailty_category1, r1dintvwrage, mort_status, handgrip1, walk1, side1, semitandem1, tandem1, eyes_open1, eyes_closed1, sts1, air1, eth)%>%
  transmute(id = spid,
            sppb = sppb1,
            sisindex = sociso_index,
            wc =wc1,
            demdiag = dementia1,
            age_died = age_died,
            dep = dep1_index_rec,
            anx = ax1_index_rec,
            chr_cond = chr_cond1,
            smk_before = smk_before1,
            smk_now = smk_now1,
            pain = pain,
            exercise = exercise,
            income = income,
            sex= sex,
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res=res,
            hosp_stay=hosp_stay_n1,
            frailty=frailty1,
            frailty_category=frailty_category1,
            age=r1dintvwrage,
            mort_status = mort_status,
            handgrip = handgrip1,
            walk = walk1,
            side = side1,
            semitandem = semitandem1,
            tandem = tandem1,
            eyes_open = eyes_open1,
            eyes_closed = eyes_closed1,
            sts = sts1,
            air = air1,
            demclas = demclas,
            dementia_age = hc1dementage,
            eth = eth)->nhats1

nhats2%>%
  dplyr::select(spid, sppb2,demclas, dementia2, r2ddeathage, r2ddeathage, hc2dementage,sociso_index, wc2,dep2_index_rec,ax2_index_rec, chr_cond2, smk_before2,smk_now2,pain, exercise, income ,sex, dis_issues, hear_issues, emp,res, hosp_stay_n2, frailty2, frailty_category2, r2dintvwrage, mort_status, handgrip2, walk2, side2, semitandem2, tandem2, eyes_open2, eyes_closed2, sts2, air2, eth)%>%
  transmute(id = spid,
            sppb = sppb2,
            age_died = r2ddeathage,
            sisindex = sociso_index,
            wc =wc2,
            demdiag = dementia2,
            dep = dep2_index_rec,
            anx = ax2_index_rec,
            chr_cond = chr_cond2,
            smk_before = smk_before2,
            smk_now = smk_now2,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n2,
            frailty=frailty_category2,
            age=r2dintvwrage,
            frailty_category=frailty_category2,
            mort_status = mort_status,
            handgrip = handgrip2,
            walk = walk2,
            side = side2,
            semitandem = semitandem2,
            tandem = tandem2,
            eyes_open = eyes_open2,
            eyes_closed = eyes_closed2,
            sts = sts2,
            air = air2,
            demclas = demclas,
            dementia_age = hc2dementage,
            eth = eth)->nhats2


nhats3%>%
  dplyr::select(spid, sppb3,demclas, dementia3, hc3dementage,sociso_index, r3ddeathage,  wc3,dep3_index_rec,ax3_index_rec, chr_cond3, smk_before3,smk_now3,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n3, frailty3, r3dintvwrage, frailty_category3, mort_status, handgrip3, walk3, side3, semitandem3, tandem3, eyes_open3, eyes_closed3, sts3, air3, eth)%>%
  transmute(id = spid,
            sppb = sppb3,
            sisindex = sociso_index,
            wc =wc3,
            demdiag = dementia3,
            age_died = r3ddeathage,
            dep = dep3_index_rec,
            anx = ax3_index_rec,
            chr_cond = chr_cond3,
            smk_before = smk_before3,
            smk_now = smk_now3,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n3,
            frailty=frailty3,
            age=r3dintvwrage,
            frailty_category=frailty_category3,
            mort_status = mort_status,
            handgrip = handgrip3,
            walk = walk3,
            side = side3,
            semitandem = semitandem3,
            tandem = tandem3,
            eyes_open = eyes_open3,
            eyes_closed = eyes_closed3,
            sts = sts3,
            air = air3,
            demclas = demclas,
            dementia_age = hc3dementage,
            eth = eth)->nhats3

nhats4%>%
  dplyr::select(spid, sppb4, dementia4, demclas,hc4dementage, r4ddeathage, sociso_index, wc4,dep4_index_rec,ax4_index_rec, chr_cond4, smk_before4,smk_now4,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n4, frailty4, r4dintvwrage, frailty_category4, mort_status, handgrip4, walk4, side4, semitandem4, tandem4, eyes_open4, eyes_closed4, sts4, air4, eth)%>%
  transmute(id = spid,
            sppb = sppb4,
            age_died = r4ddeathage,
            sisindex = sociso_index,
            wc =wc4,
            demdiag = dementia4,
            dep = dep4_index_rec,
            anx = ax4_index_rec,
            chr_cond = chr_cond4,
            smk_before = smk_before4,
            smk_now = smk_now4,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n4,
            frailty=frailty4,
            age=r4dintvwrage,
            frailty_category=frailty_category4,
            mort_status = mort_status,
            handgrip = handgrip4,
            walk = walk4,
            side = side4,
            semitandem = semitandem4,
            tandem = tandem4,
            eyes_open = eyes_open4,
            eyes_closed = eyes_closed4,
            sts = sts4,
            air = air4,
            demclas = demclas,
            dementia_age = hc4dementage,
            eth = eth)->nhats4

nhats5%>%
  dplyr::select(spid, sppb5,demclas,dementia5, hc5dementage, r5ddeathage, sociso_index, wc5,dep5_index_rec,ax5_index_rec, chr_cond5, smk_before5,smk_now5,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n5, frailty5, r5dintvwrage, frailty_category5, mort_status, handgrip5, walk5, side5, semitandem5, tandem5, eyes_open5, eyes_closed5, sts5, air5, eth)%>%
  transmute(id = spid,
            sppb = sppb5,
            sisindex = sociso_index,
            wc =wc5,
            demdiag = dementia5,
            age_died = r5ddeathage,
            dep = dep5_index_rec,
            anx = ax5_index_rec,
            chr_cond = chr_cond5,
            smk_before = smk_before5,
            smk_now = smk_now5,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n5,
            frailty=frailty5,
            age=r5dintvwrage,
            frailty_category=frailty_category5,
            mort_status = mort_status,
            handgrip = handgrip5,
            walk = walk5,
            side = side5,
            semitandem = semitandem5,
            tandem = tandem5,
            eyes_open = eyes_open5,
            eyes_closed = eyes_closed5,
            sts = sts5,
            air = air5,
            demclas = demclas,
            dementia_age = hc5dementage,
            eth = eth)->nhats5

nhats6%>%
  dplyr::select(spid, sppb6,demclas,dementia6, hc6dementage, r6ddeathage, sociso_index, wc6,dep6_index_rec,ax6_index_rec, chr_cond6, smk_before6,smk_now6,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n6, frailty6, r6dintvwrage, frailty_category6, mort_status, handgrip6, walk6, side6, semitandem6, tandem6, eyes_open6, eyes_closed6, sts6, air6, eth)%>%
  transmute(id = spid,
            sppb = sppb6,
            sisindex = sociso_index,
            wc =wc6,
            demdiag = dementia6,
            age_died = r6ddeathage,
            dep = dep6_index_rec,
            anx = ax6_index_rec,
            chr_cond = chr_cond6,
            smk_before = smk_before6,
            smk_now = smk_now6,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n6,
            frailty=frailty6,
            age=r6dintvwrage,
            frailty_category=frailty_category6,
            mort_status = mort_status,
            handgrip = handgrip6,
            walk = walk6,
            side = side6,
            semitandem = semitandem6,
            tandem = tandem6,
            eyes_open = eyes_open6,
            eyes_closed = eyes_closed6,
            sts = sts6,
            air = air6,
            demclas = demclas,
            dementia_age = hc6dementage,
            eth = eth)->nhats6

nhats7%>%
  dplyr::select(spid, sppb7,demclas,dementia7, hc7dementage, r7ddeathage, sociso_index, wc7,dep7_index_rec,ax7_index_rec, chr_cond7, smk_before7,smk_now7,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n7, frailty7, r7dintvwrage, frailty_category7, mort_status, handgrip7, walk7, side7, semitandem7, tandem7, eyes_open7, eyes_closed7, sts7, air7, eth)%>%
  transmute(id = spid,
            sppb = sppb7,
            sisindex = sociso_index,
            wc =wc7,
            demdiag = dementia7,
            age_died = r7ddeathage,
            dep = dep7_index_rec,
            anx = ax7_index_rec,
            chr_cond = chr_cond7,
            smk_before = smk_before7,
            smk_now = smk_now7,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n7,
            frailty=frailty7,
            age=r7dintvwrage,
            frailty_category=frailty_category7,
            mort_status = mort_status,
            handgrip = handgrip7,
            walk = walk7,
            side = side7,
            semitandem = semitandem7,
            tandem = tandem7,
            eyes_open = eyes_open7,
            eyes_closed = eyes_closed7,
            sts = sts7,
            air = air7,
            demclas = demclas,
            dementia_age = hc7dementage,
            eth = eth)->nhats7

nhats8%>%
  dplyr::select(spid, sppb8,demclas,dementia8, hc8dementage, r8ddeathage, sociso_index, wc8,dep8_index_rec,ax8_index_rec, chr_cond8, smk_before8,smk_now8,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n8, frailty8, r8dintvwrage, frailty_category8, mort_status, handgrip8, walk8, side8, semitandem8, tandem8, eyes_open8, eyes_closed8, sts8, air8, eth)%>%
  transmute(id = spid,
            sppb = sppb8,
            sisindex = sociso_index,
            wc =wc8,
            demdiag = dementia8,
            age_died = r8ddeathage,
            dep = dep8_index_rec,
            anx = ax8_index_rec,
            chr_cond = chr_cond8,
            smk_before = smk_before8,
            smk_now = smk_now8,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n8,
            frailty=frailty_category8,
            age=r8dintvwrage,
            frailty_category=frailty_category8,
            mort_status = mort_status,
            handgrip = handgrip8,
            walk = walk8,
            side = side8,
            semitandem = semitandem8,
            tandem = tandem8,
            eyes_open = eyes_open8,
            eyes_closed = eyes_closed8,
            sts = sts8,
            air = air8,
            demclas = demclas,
            dementia_age = hc8dementage,
            eth = eth)->nhats8

nhats9%>%
  dplyr::select(spid, sppb9,hc9dementage, r9ddeathage, demclas,dementia9, sociso_index, wc9,dep9_index_rec,ax9_index_rec, chr_cond9, smk_before9,smk_now9,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n9, frailty9, r9dintvwrage, frailty_category9, mort_status, handgrip9, walk9, side9, semitandem9, tandem9, eyes_open9, eyes_closed9, sts9, air9, eth)%>%
  transmute(id = spid,
            sppb = sppb9,
            sisindex = sociso_index,
            wc =wc9,
            demdiag = dementia9,
            age_died = r9ddeathage,
            dep = dep9_index_rec,
            anx = ax9_index_rec,
            chr_cond = chr_cond9,
            smk_before = smk_before9,
            smk_now = smk_now9,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n9,
            frailty=frailty_category9,
            age=r9dintvwrage,
            frailty_category=frailty_category9,
            mort_status = mort_status,
            handgrip = handgrip9,
            walk = walk9,
            side = side9,
            semitandem = semitandem9,
            tandem = tandem9,
            eyes_open = eyes_open9,
            eyes_closed = eyes_closed9,
            sts = sts9,
            air = air9,
            demclas = demclas,
            dementia_age = hc9dementage,
            eth = eth)->nhats9


# add column sppb10 manually
nhats10$sppb10 <- NA

# add column wc10 manually
nhats10$wc10 <- NA

# add column side10 manually
nhats10$side10 <- NA

# add column semitandem10 manually
nhats10$semitandem10 <- NA

# add column tandem10 manually
nhats10$tandem10 <- NA

# add column eyes_open10 manually
nhats10$eyes_open10 <- NA

# add column eyes_closed10 manually
nhats10$eyes_closed10 <- NA

# add column sts10 manually
nhats10$sts10 <- NA

# add column walk10 manually
nhats10$walk10 <- NA


nhats10%>%
  dplyr::select(spid, sppb10,hc10dementage, r10ddeathage, demclas,dementia10, sociso_index, wc10,dep10_index_rec,ax10_index_rec, chr_cond10, smk_before10,smk_now10,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n10, frailty10, r10dintvwrage, frailty_category10, mort_status, handgrip10, walk10, side10, semitandem10, tandem10, eyes_open10, eyes_closed10, sts10, air10, eth)%>%
  transmute(id = spid,
            sppb = sppb10,
            sisindex = sociso_index,
            wc =wc10,
            demdiag = dementia10,
            age_died = r10ddeathage,
            dep = dep10_index_rec,
            anx = ax10_index_rec,
            chr_cond = chr_cond10,
            smk_before = smk_before10,
            smk_now = smk_now10,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n10,
            frailty=frailty_category10,
            age=r10dintvwrage,
            frailty_category=frailty_category10,
            mort_status = mort_status,
            handgrip = handgrip10,
            walk = walk10,
            side = side10,
            semitandem = semitandem10,
            tandem = tandem10,
            eyes_open = eyes_open10,
            eyes_closed = eyes_closed10,
            sts = sts10,
            air = air10,
            demclas = demclas,
            dementia_age = hc10dementage,
            eth = eth)->nhats10

nhats11%>%
  dplyr::select(spid, sppb11,hc11dementage, r11ddeathage,demclas,dementia11, sociso_index, wc11,dep11_index_rec,ax11_index_rec, chr_cond11, smk_before11,smk_now11,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n11, frailty11, r11dintvwrage, frailty_category11, mort_status, handgrip11, walk11, side11, semitandem11, tandem11, eyes_open11, eyes_closed11, sts11, air11, eth)%>%
  transmute(id = spid,
            sppb = sppb11,
            sisindex = sociso_index,
            wc =wc11,
            demdiag = dementia11,
            age_died = r11ddeathage,
            dep = dep11_index_rec,
            anx = ax11_index_rec,
            chr_cond = chr_cond11,
            smk_before = smk_before11,
            smk_now = smk_now11,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n11,
            frailty=frailty_category11,
            age=r11dintvwrage,
            frailty_category=frailty_category11,
            mort_status = mort_status,
            handgrip = handgrip11,
            walk = walk11,
            side = side11,
            semitandem = semitandem11,
            tandem = tandem11,
            eyes_open = eyes_open11,
            eyes_closed = eyes_closed11,
            sts = sts11,
            air = air11,
            demclas = demclas,
            dementia_age = hc11dementage,
            eth = eth)->nhats11

nhats12%>%
  dplyr::select(spid, sppb12,hc12dementage, r12ddeathage,demclas,dementia12, sociso_index, wc12,dep12_index_rec,ax12_index_rec, chr_cond12, smk_before12,smk_now12,pain, exercise, income,sex, dis_issues, hear_issues, emp,res, hosp_stay_n12, frailty12, r12dintvwrage, frailty_category12, mort_status, handgrip12, walk12, side12, semitandem12, tandem12, eyes_open12, eyes_closed12, sts12, air12, eth)%>%
  transmute(id = spid,
            sppb = sppb12,
            sisindex = sociso_index,
            wc =wc12,
            demdiag = dementia12,
            age_died = r12ddeathage,
            dep = dep12_index_rec,
            anx = ax12_index_rec,
            chr_cond = chr_cond12,
            smk_before = smk_before12,
            smk_now = smk_now12,
            pain = pain,
            exercise = exercise,
            income = income,
            sex = sex, 
            hear= hear_issues,
            vision = dis_issues,
            emp = emp,
            res = res,
            hosp_stay=hosp_stay_n12,
            frailty=frailty_category12,
            age=r12dintvwrage,
            frailty_category=frailty_category12,
            mort_status = mort_status,
            handgrip = handgrip12,
            walk = walk12,
            side = side12,
            semitandem = semitandem12,
            tandem = tandem12,
            eyes_open = eyes_open12,
            eyes_closed = eyes_closed12,
            sts = sts12,
            air = air12,
            demclas = demclas,
            dementia_age = hc12dementage,
            eth = eth)->nhats12


#removing -1 for age 

nhats1$age<-ifelse(nhats1$age<=-1, NA, nhats1$age) 
nhats2$age<-ifelse(nhats2$age<=-1, NA, nhats2$age) 
nhats3$age<-ifelse(nhats3$age<=-1, NA, nhats3$age) 
nhats4$age<-ifelse(nhats4$age<=-1, NA, nhats4$age) 
nhats5$age<-ifelse(nhats5$age<=-1, NA, nhats5$age) 
nhats6$age<-ifelse(nhats6$age<=-1, NA, nhats6$age) 
nhats7$age<-ifelse(nhats7$age<=-1, NA, nhats7$age) 
nhats8$age<-ifelse(nhats8$age<=-1, NA, nhats8$age) 
nhats9$age<-ifelse(nhats9$age<=-1, NA, nhats9$age) 
nhats10$age<-ifelse(nhats10$age<=-1, NA, nhats10$age) 
nhats11$age<-ifelse(nhats11$age<=-1, NA, nhats11$age) 
nhats12$age<-ifelse(nhats12$age<=-1, NA, nhats12$age) 


#removing -1 or -9 for demclas 

nhats1$demclas<-ifelse(nhats1$demclas<=-1 , NA, nhats1$demclas) 
nhats2$demclas<-ifelse(nhats2$demclas<=-1, NA, nhats2$demclas) 
nhats3$demclas<-ifelse(nhats3$demclas<=-1, NA, nhats3$demclas) 
nhats4$demclas<-ifelse(nhats4$demclas<=-1, NA, nhats4$demclas) 
nhats5$demclas<-ifelse(nhats5$demclas<=-1, NA, nhats5$demclas) 
nhats6$demclas<-ifelse(nhats6$demclas<=-1, NA, nhats6$demclas) 
nhats7$demclas<-ifelse(nhats7$demclas<=-1, NA, nhats7$demclas) 
nhats8$demclas<-ifelse(nhats8$demclas<=-1, NA, nhats8$demclas) 
nhats9$demclas<-ifelse(nhats9$demclas<=-1, NA, nhats9$demclas) 
nhats10$demclas<-ifelse(nhats10$demclas<=-1, NA, nhats10$demclas) 
nhats11$demclas<-ifelse(nhats11$demclas<=-1, NA, nhats11$demclas) 
nhats12$demclas<-ifelse(nhats12$demclas<=-1, NA, nhats12$demclas) 

# add column wave manually
nhats1$wave <- 1
nhats2$wave <- 2
nhats3$wave <- 3
nhats4$wave <- 4
nhats5$wave <- 5
nhats6$wave <- 6
nhats7$wave <- 7
nhats8$wave <- 8
nhats9$wave <- 9
nhats10$wave <- 10
nhats11$wave <- 11
nhats12$wave <- 12

NHATS_Round_1_SP_extra$r1sp

# add mort status as 0 in demclas
nhats2 <- nhats2 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats3 <- nhats3 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats4 <- nhats4 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats5 <- nhats5 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats6 <- nhats6 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats7 <- nhats7 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats8 <- nhats8 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats9 <- nhats9 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats10 <- nhats10 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats11 <- nhats11 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))
nhats12 <- nhats12 %>% mutate(demclas=ifelse(mort_status==1,0,demclas))


#DATOS PARA FECHA DE ENTREVISTA
tracker1 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_1")
tracker2 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_2")
tracker3 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_3")
tracker4 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_4")
tracker5 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_5")
tracker6 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_6")
tracker7 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_7")
tracker8 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_8")
tracker9 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_9")
tracker10 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_10")
tracker11 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_11")
tracker12 <- read_sas("/Users/danielgallardogomez/Downloads/TRACKER_12")

#crear tiempo que lleva en el estudio
#mes de la entrevista
nhats1$date_mes <- tracker1 %>% filter(spid %in% nhats1$id) %>% .$r1spstatdtmt 
nhats1 <- nhats1 %>%dplyr::filter(date_mes!=-1)

nhats2$date_mes <- tracker2 %>% filter(spid %in% nhats2$id) %>% .$r2spstatdtmt 
nhats2 <- nhats2 %>%dplyr::filter(date_mes!=-1)

nhats3$date_mes <- tracker3 %>% filter(spid %in% nhats3$id) %>% .$r3spstatdtmt 
nhats3 <- nhats3 %>%dplyr::filter(date_mes!=-1)

nhats4$date_mes <- tracker4 %>% filter(spid %in% nhats4$id) %>% .$r4spstatdtmt 
nhats4 <- nhats4 %>%dplyr::filter(date_mes!=-1)

nhats5$date_mes <- tracker5 %>% filter(spid %in% nhats5$id) %>% .$r5spstatdtmt 
nhats5 <- nhats5 %>%dplyr::filter(date_mes!=-1)

nhats6$date_mes <- tracker6 %>% filter(spid %in% nhats6$id) %>% .$r6spstatdtmt 
nhats6 <- nhats6 %>%dplyr::filter(date_mes!=-1)

nhats7$date_mes <- tracker7 %>% filter(spid %in% nhats7$id) %>% .$r7spstatdtmt 
nhats7 <- nhats7 %>%dplyr::filter(date_mes!=-1)

nhats8$date_mes <- tracker8 %>% filter(spid %in% nhats8$id) %>% .$r8spstatdtmt 
nhats8 <- nhats8 %>%dplyr::filter(date_mes!=-1)

nhats9$date_mes <- tracker9 %>% filter(spid %in% nhats9$id) %>% .$r9spstatdtmt 
nhats9 <- nhats9 %>%dplyr::filter(date_mes!=-1)

nhats10$date_mes <- tracker10 %>% filter(spid %in% nhats10$id) %>% .$r10spstatdtmt 
nhats10 <- nhats10 %>%dplyr::filter(date_mes!=-1)

nhats11$date_mes <- tracker11 %>% filter(spid %in% nhats11$id) %>% .$r11spstatdtmt1 
nhats11 <- nhats11 %>%
  mutate(date_mes = ifelse(is.na(date_mes) & id %in% tracker11$spid, r11spstatdtmt2, date_mes))
nhats11 <- nhats11 %>%dplyr::filter(date_mes!=-1)

nhats12$date_mes <- tracker12 %>% filter(spid %in% nhats12$id) %>% .$r12spstatdtmt1 
nhats12 <- nhats12 %>%
  mutate(date_mes = ifelse(is.na(date_mes) & id %in% tracker12$spid, r12spstatdtmt2, date_mes))
nhats12 <- nhats12 %>%dplyr::filter(date_mes!=-1)

#año de la entrevista r2spstatdtyr

nhats1$date_yr <- tracker1 %>% filter(spid %in% nhats1$id) %>% .$r1spstatdtyr 
nhats1 <- nhats1 %>%dplyr::filter(date_yr!=-1)

nhats2$date_yr <- tracker2 %>% filter(spid %in% nhats2$id) %>% .$r2spstatdtyr 
nhats2 <- nhats2 %>%dplyr::filter(date_yr!=-1)

nhats3$date_yr <- tracker3 %>% filter(spid %in% nhats3$id) %>% .$r3spstatdtyr 
nhats3 <- nhats3 %>%dplyr::filter(date_yr!=-1)

nhats4$date_yr <- tracker4 %>% filter(spid %in% nhats4$id) %>% .$r4spstatdtyr 
nhats4 <- nhats4 %>%dplyr::filter(date_yr!=-1)

nhats5$date_yr <- tracker5 %>% filter(spid %in% nhats5$id) %>% .$r5spstatdtyr 
nhats5 <- nhats5 %>%dplyr::filter(date_yr!=-1)

nhats6$date_yr <- tracker6 %>% filter(spid %in% nhats6$id) %>% .$r6spstatdtyr 
nhats6 <- nhats6 %>%dplyr::filter(date_yr!=-1)

nhats7$date_yr <- tracker7 %>% filter(spid %in% nhats7$id) %>% .$r7spstatdtyr 
nhats7 <- nhats7 %>%dplyr::filter(date_yr!=-1)

nhats8$date_yr <- tracker8 %>% filter(spid %in% nhats8$id) %>% .$r8spstatdtyr 
nhats8 <- nhats8 %>%dplyr::filter(date_yr!=-1)

nhats9$date_yr <- tracker9 %>% filter(spid %in% nhats9$id) %>% .$r9spstatdtyr 
nhats9 <- nhats9 %>%dplyr::filter(date_yr!=-1)

nhats10$date_yr <- tracker10 %>% filter(spid %in% nhats10$id) %>% .$r10spstatdtyr 
nhats10 <- nhats10 %>%dplyr::filter(date_yr!=-1)

nhats11$date_yr <- tracker11 %>% filter(spid %in% nhats11$id) %>% .$r11spstatdtyr1 
nhats11 <- nhats11 %>%
  mutate(date_yr = ifelse(is.na(date_yr) & id %in% tracker11$spid, r11spstatdtyr2, date_yr))
nhats11 <- nhats11 %>%dplyr::filter(date_yr!=-1)

nhats12$date_yr <- tracker12 %>% filter(spid %in% nhats12$id) %>% .$r12spstatdtyr1 
nhats12 <- nhats12 %>%
  mutate(date_yr = ifelse(is.na(date_yr) & id %in% tracker12$spid, r12spstatdtyr2, date_yr))
nhats12 <- nhats12 %>%dplyr::filter(date_yr!=-1)

#fecha de la entrevista date_mes y date_yr
nhats1$date <- make_date(year = nhats1$date_yr, month = nhats1$date_mes)
nhats2$date <- make_date(year = nhats2$date_yr, month = nhats2$date_mes)
nhats3$date <- make_date(year = nhats3$date_yr, month = nhats3$date_mes)
nhats4$date <- make_date(year = nhats4$date_yr, month = nhats4$date_mes)
nhats5$date <- make_date(year = nhats5$date_yr, month = nhats5$date_mes)
nhats6$date <- make_date(year = nhats6$date_yr, month = nhats6$date_mes)
nhats7$date <- make_date(year = nhats7$date_yr, month = nhats7$date_mes)
nhats8$date <- make_date(year = nhats8$date_yr, month = nhats8$date_mes)
nhats9$date <- make_date(year = nhats9$date_yr, month = nhats9$date_mes)
nhats10$date <- make_date(year = nhats10$date_yr, month = nhats10$date_mes)
nhats11$date <- make_date(year = nhats11$date_yr, month = nhats11$date_mes)
nhats12$date <- make_date(year = nhats12$date_yr, month = nhats12$date_mes)


#mes de la muerte

# add column died r1 manually
nhats1$date_died_mes <- NA

nhats2$date_died_mes <- NHATS_Round_2_SP_extra %>% filter(spid %in% nhats2$id) %>% .$pd2mthdied 
nhats2$date_died_mes <- ifelse(nhats2$date_died_mes < 0, NA, nhats2$date_died_mes)

nhats3$date_died_mes <- NHATS_Round_3_SP_extra %>% filter(spid %in% nhats3$id) %>% .$pd3mthdied 
nhats3$date_died_mes <- ifelse(nhats3$date_died_mes < 0, NA, nhats3$date_died_mes)

nhats4$date_died_mes <- NHATS_Round_4_SP_extra %>% filter(spid %in% nhats4$id) %>% .$pd4mthdied 
nhats4$date_died_mes <- ifelse(nhats4$date_died_mes < 0, NA, nhats4$date_died_mes)

nhats5$date_died_mes <- NHATS_Round_5_SP_extra %>% filter(spid %in% nhats5$id) %>% .$pd5mthdied 
nhats5$date_died_mes <- ifelse(nhats5$date_died_mes < 0, NA, nhats5$date_died_mes)

nhats6$date_died_mes <- NHATS_Round_6_SP_extra %>% filter(spid %in% nhats6$id) %>% .$pd6mthdied 
nhats6$date_died_mes <- ifelse(nhats6$date_died_mes < 0, NA, nhats6$date_died_mes)

nhats7$date_died_mes <- NHATS_Round_7_SP_extra %>% filter(spid %in% nhats7$id) %>% .$pd7mthdied 
nhats7$date_died_mes <- ifelse(nhats7$date_died_mes < 0, NA, nhats7$date_died_mes)

nhats8$date_died_mes <- NHATS_Round_8_SP_extra %>% filter(spid %in% nhats8$id) %>% .$pd8mthdied 
nhats8$date_died_mes <- ifelse(nhats8$date_died_mes < 0, NA, nhats8$date_died_mes)

nhats9$date_died_mes <- NHATS_Round_9_SP_extra %>% filter(spid %in% nhats9$id) %>% .$pd9mthdied 
nhats9$date_died_mes <- ifelse(nhats9$date_died_mes < 0, NA, nhats9$date_died_mes)

nhats10$date_died_mes <- NHATS_Round_10_SP_extra %>% filter(spid %in% nhats10$id) %>% .$pd10mthdied 
nhats10$date_died_mes <- ifelse(nhats10$date_died_mes < 0, NA, nhats10$date_died_mes)

nhats11$date_died_mes <- NHATS_Round_11_SP_extra %>% filter(spid %in% nhats11$id) %>% .$pd11mthdied 
nhats11$date_died_mes <- ifelse(nhats11$date_died_mes < 0, NA, nhats11$date_died_mes)

nhats12$date_died_mes <- NHATS_Round_12_SP_extra %>% filter(spid %in% nhats12$id) %>% .$pd12mthdied 
nhats12$date_died_mes <- ifelse(nhats12$date_died_mes < 0, NA, nhats12$date_died_mes)


#año de la muerte pd2yrdied  pd2yrdied= en la 2 1 es 2011 y 2 es 2012


# add column died r1 manually
nhats1$date_died_yr <- NA

nhats2$date_died_yr <- NHATS_Round_2_SP_extra %>% filter(spid %in% nhats2$id) %>% .$pd2yrdied 
nhats2$date_died_yr <- case_when(nhats2$date_died_yr ==1~ "2011",
                                 nhats2$date_died_yr ==2~ "2012")

table(nhats8$date_died_yr)

nhats3$date_died_yr <- NHATS_Round_3_SP_extra %>% filter(spid %in% nhats3$id) %>% .$pd3yrdied 
nhats3$date_died_yr <- ifelse(nhats3$date_died_yr < 0, NA, nhats3$date_died_yr)

nhats4$date_died_yr <- NHATS_Round_4_SP_extra %>% filter(spid %in% nhats4$id) %>% .$pd4yrdied 
nhats4$date_died_yr <- ifelse(nhats4$date_died_yr < 0, NA, nhats4$date_died_yr)

nhats5$date_died_yr <- NHATS_Round_5_SP_extra %>% filter(spid %in% nhats5$id) %>% .$pd5yrdied 
nhats5$date_died_yr <- ifelse(nhats5$date_died_yr < 0, NA, nhats5$date_died_yr)

nhats6$date_died_yr <- NHATS_Round_6_SP_extra %>% filter(spid %in% nhats6$id) %>% .$pd6yrdied 
nhats6$date_died_yr <- ifelse(nhats6$date_died_yr < 0, NA, nhats6$date_died_yr)

nhats7$date_died_yr <- NHATS_Round_7_SP_extra %>% filter(spid %in% nhats7$id) %>% .$pd7yrdied 
nhats7$date_died_yr <- ifelse(nhats7$date_died_yr < 0, NA, nhats7$date_died_yr)

nhats8$date_died_yr <- NHATS_Round_8_SP_extra %>% filter(spid %in% nhats8$id) %>% .$pd8yrdied 
nhats8$date_died_yr <- ifelse(nhats8$date_died_yr < 0, NA, nhats8$date_died_yr)

nhats9$date_died_yr <- NHATS_Round_9_SP_extra %>% filter(spid %in% nhats9$id) %>% .$pd9yrdied 
nhats9$date_died_yr <- ifelse(nhats9$date_died_yr < 0, NA, nhats9$date_died_yr)

nhats10$date_died_yr <- NHATS_Round_10_SP_extra %>% filter(spid %in% nhats10$id) %>% .$pd10yrdied 
nhats10$date_died_yr <- ifelse(nhats10$date_died_yr < 0, NA, nhats10$date_died_yr)

nhats11$date_died_yr <- NHATS_Round_11_SP_extra %>% filter(spid %in% nhats11$id) %>% .$pd11yrdied 
nhats11$date_died_yr <- ifelse(nhats11$date_died_yr < 0, NA, nhats11$date_died_yr)

nhats12$date_died_yr <- NHATS_Round_12_SP_extra %>% filter(spid %in% nhats12$id) %>% .$pd12yrdied 
nhats12$date_died_yr <- ifelse(nhats12$date_died_yr < 0, NA, nhats12$date_died_yr)

#fecha de la muerte date_died_mes y date_died_yr

nhats1$date_died <- NA

nhats2$date_died <- make_date(year = nhats2$date_died_yr, month = nhats2$date_died_mes)
nhats3$date_died <- make_date(year = nhats3$date_died_yr, month = nhats3$date_died_mes)
nhats4$date_died <- make_date(year = nhats4$date_died_yr, month = nhats4$date_died_mes)
nhats5$date_died <- make_date(year = nhats5$date_died_yr, month = nhats5$date_died_mes)
nhats6$date_died <- make_date(year = nhats6$date_died_yr, month = nhats6$date_died_mes)
nhats7$date_died <- make_date(year = nhats7$date_died_yr, month = nhats7$date_died_mes)
nhats8$date_died <- make_date(year = nhats8$date_died_yr, month = nhats8$date_died_mes)
nhats9$date_died <- make_date(year = nhats9$date_died_yr, month = nhats9$date_died_mes)
nhats10$date_died <- make_date(year = nhats10$date_died_yr, month = nhats10$date_died_mes)
nhats11$date_died <- make_date(year = nhats11$date_died_yr, month = nhats11$date_died_mes)
nhats12$date_died <- make_date(year = nhats12$date_died_yr, month = nhats12$date_died_mes)


####get the data in long format for plm UNION####
#check here from age
plm_exam<-rbind.data.frame(nhats1, nhats2,nhats3,nhats4, nhats5,
                           nhats6,nhats7,nhats8,nhats9,nhats10, nhats11, nhats12)

library(tidyr)
# data in wide format creation
data.wide <- nhats1 %>% 
  dplyr::rename(wave.1 = wave) %>% 
  pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.1) %>% mutate(wave_1=1) %>%
  full_join(nhats2 %>% dplyr::rename(wave.2 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.2),by="id") %>% mutate(wave_2=2) %>%
  full_join(nhats3 %>% dplyr::rename(wave.3 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.3),by="id") %>% mutate(wave_3=3) %>%
  full_join(nhats4 %>% dplyr::rename(wave.4 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.4),by="id") %>% mutate(wave_4=4) %>%
  full_join(nhats5 %>% dplyr::rename(wave.5 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.5),by="id") %>% mutate(wave_5=5) %>%
  full_join(nhats6 %>% dplyr::rename(wave.6 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.6),by="id") %>% mutate(wave_6=6) %>%
  full_join(nhats7 %>% dplyr::rename(wave.7 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.7),by="id") %>% mutate(wave_7=7) %>%
  full_join(nhats8 %>% dplyr::rename(wave.8 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.8),by="id") %>% mutate(wave_8=8) %>%
  full_join(nhats9 %>% dplyr::rename(wave.9 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.9),by="id") %>% mutate(wave_9=9) %>%
  full_join(nhats10 %>% dplyr::rename(wave.10 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.10),by="id") %>% mutate(wave_10=10) %>%
  full_join(nhats11 %>% dplyr::rename(wave.11 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.11),by="id") %>% mutate(wave_11=11) %>%
  full_join(nhats12 %>% dplyr::rename(wave.12 = wave) %>% pivot_wider(id_cols = id, values_from = c(demclas,date, date_died), names_from = wave.12),by="id") %>% mutate(wave_12=12) %>%
  .[, !duplicated(colnames(.))]


# ADENDA: after visualise wrong observations with a clinical diagnosis of dementia that is not demclas == 1 (i.e., probable dementia), we are going to fix that in the code
## observations
plm_exam %>%
  dplyr::select(id, demclas, wave, demdiag) %>%
  arrange(id) %>%
  filter(demclas != 1 & demdiag == 1)

## modifying the wrong observations in the wave 11 
plm_exam <- plm_exam %>%
  mutate(demclas = ifelse(wave == 11 & demdiag == 1, 1, demclas))

#VARIABLE AÑOS EN EL NHATS
data_wide <- data.wide %>%
  mutate(t_21 = as.numeric(difftime(as.Date(date_2), as.Date(date_1), units = "weeks")/ 52.1429), # average weeks of a year
         t_31 = as.numeric(difftime(as.Date(date_3), as.Date(date_1), units = "weeks")/ 52.1429),
         t_41 = as.numeric(difftime(as.Date(date_4), as.Date(date_1), units = "weeks")/ 52.1429),
         t_51 = as.numeric(difftime(as.Date(date_5), as.Date(date_1), units = "weeks")/ 52.1429),
         t_61 = as.numeric(difftime(as.Date(date_6), as.Date(date_1), units = "weeks")/ 52.1429),
         t_71 = as.numeric(difftime(as.Date(date_7), as.Date(date_1), units = "weeks")/ 52.1429),
         t_81 = as.numeric(difftime(as.Date(date_8), as.Date(date_1), units = "weeks")/ 52.1429),
         t_91 = as.numeric(difftime(as.Date(date_9), as.Date(date_1), units = "weeks")/ 52.1429),
         t_101 = as.numeric(difftime(as.Date(date_10), as.Date(date_1), units = "weeks")/ 52.1429),
         t_111 = as.numeric(difftime(as.Date(date_11), as.Date(date_1), units = "weeks")/ 52.1429),
         t_121 = as.numeric(difftime(as.Date(date_12), as.Date(date_1), units = "weeks")/ 52.1429)) %>% 
  mutate(t_21 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_2), as.Date(date_1), units = "weeks")/ 52.1429), t_21),
         t_31 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_3), as.Date(date_1), units = "weeks")/ 52.1429), t_31),
         t_41 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_4), as.Date(date_1), units = "weeks")/ 52.1429), t_41),
         t_51 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_5), as.Date(date_1), units = "weeks")/ 52.1429), t_51),
         t_61 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_6), as.Date(date_1), units = "weeks")/ 52.1429), t_61),
         t_71 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_7), as.Date(date_1), units = "weeks")/ 52.1429), t_71),
         t_81 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_8), as.Date(date_1), units = "weeks")/ 52.1429), t_81),
         t_91 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_9), as.Date(date_1), units = "weeks")/ 52.1429), t_91),
         t_101 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_10), as.Date(date_1), units = "weeks")/ 52.1429), t_101),
         t_111 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_11), as.Date(date_1), units = "weeks")/ 52.1429), t_111),
         t_121 = ifelse(demclas_2 == 0, as.numeric(difftime(as.Date(date_died_12), as.Date(date_1), units = "weeks")/ 52.1429), t_121))


### RECODING ASSUMING THAT WHEN A MORE-ADVANCED STATE IS REACHED, THE NEXT ONES WILL BE CODED AS THE SAME, OR MORE ADVANCED
### NOT ALLOWING BACKWARDS TRANSITIONS
data_wide <- data_wide %>%
  mutate(demclas_2 = ifelse(demclas_2 > demclas_1, demclas_1, demclas_2),
         demclas_3 = ifelse(demclas_3 > demclas_2, demclas_2, demclas_3),
         demclas_4 = ifelse(demclas_4 > demclas_3, demclas_3, demclas_4),
         demclas_5 = ifelse(demclas_5 > demclas_4, demclas_4, demclas_5),
         demclas_6 = ifelse(demclas_6 > demclas_5, demclas_5, demclas_6),
         demclas_7 = ifelse(demclas_7 > demclas_6, demclas_6, demclas_7),
         demclas_8 = ifelse(demclas_8 > demclas_7, demclas_7, demclas_8),
         demclas_9 = ifelse(demclas_9 > demclas_8, demclas_8, demclas_9),
         demclas_10 = ifelse(demclas_10 > demclas_9, demclas_9, demclas_10),
         demclas_11 = ifelse(demclas_11 > demclas_10, demclas_10, demclas_11),
         demclas_12 = ifelse(demclas_12 > demclas_11, demclas_11, demclas_12))

# add transitions variables to plm_exam by wave
# wave 1
long_1 <- data.frame(t_0 = rep(0, nrow(nhats1)),
                     demclas_1 = plm_exam %>% 
                       filter(wave == 1) %>% 
                       left_join(data_wide, by = "id") %>%
                       dplyr::select(demclas_1))

# wave 2
long_2 <- plm_exam %>%
  filter(wave == 2) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_21,
                demclas_2)

# wave 3
long_3 <- plm_exam %>%
  filter(wave == 3) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_31,
                demclas_3)

# wave 4
long_4 <- plm_exam %>%
  filter(wave == 4) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_41,
                demclas_4)

# wave 5
long_5 <- plm_exam %>%
  filter(wave == 5) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_51,
                demclas_5)

# wave 6
long_6 <- plm_exam %>%
  filter(wave == 6) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_61,
                demclas_6)

# wave 7
long_7 <- plm_exam %>%
  filter(wave == 7) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_71,
                demclas_7)

# wave 8
long_8 <- plm_exam %>%
  filter(wave == 8) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_81,
                demclas_8)

# wave 9
long_9 <- plm_exam %>%
  filter(wave == 9) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_91,
                demclas_9)

# wave 10
long_10 <- plm_exam %>%
  filter(wave == 10) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_101,
                demclas_10)

# wave 11
long_11 <- plm_exam %>%
  filter(wave == 11) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_111,
                demclas_11)

# wave 12
long_12 <- plm_exam %>%
  filter(wave == 12) %>% 
  left_join(data_wide, by = "id") %>%
  dplyr::select(t_121,
                demclas_12)


# final variable transition (years)
plm_exam$transition <- c(long_1$t_0, long_2$t_21, long_3$t_31, long_4$t_41, long_5$t_51, long_6$t_61,
                         long_7$t_71, long_8$t_81, long_9$t_91, long_10$t_101, long_11$t_111, long_12$t_121)

### RECODING ASSUMING THAT WHEN A MORE-ADVANCED STATE IS REACHED, THE NEXT ONES WILL BE CODED AS THE SAME, OR MORE ADVANCED
### NOT ALLOWING BACKWARDS TRANSITIONS
plm_exam$demclas <- c(long_1$demclas_1, long_2$demclas_2, long_3$demclas_3, long_4$demclas_4, long_5$demclas_5, long_6$demclas_6,
                      long_7$demclas_7, long_8$demclas_8, long_9$demclas_9, long_10$demclas_10, long_11$demclas_11, long_12$demclas_12)

# substitute age by age_died in case of death
plm_exam <- plm_exam %>% 
  mutate(age = ifelse(demclas == 0, age_died, age)) %>%
  mutate(age = ifelse(age < 0, NA, age))


# Exploratory Data Analysis (health status across SPPB scores)
plm_exam %>%
  mutate(demclas = case_when( # transform health status in "Healthy" or "Dementia"
    demclas == 3 ~ 0,
    demclas == 2 ~ 1, 
    demclas == 1 ~ 1,
    demclas == 0 ~ 4
  )) %>%
  mutate(demclas = factor(demclas)) %>%
  filter(demclas != 4) %>%
  ggplot(aes(x = sppb, y = demclas)) +
  geom_jitter(alpha = 0.2) +
  labs(x = "SPPB score",
        y = "") +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  scale_y_discrete(labels = c("Healthy", "Dementia\n(possible or\nprobable)")) +
  theme_light()


####AMELIA####  

# This part of the code is just in case data imputation would be needed

# m <- 25
# a.out <- amelia(x = plm_exam, m=m, cs = "id", ts = "wave",  ords =c("sppb_rec", "sisindex", "sppb", "dep", "anx", "smoking_st", "exercise", "pain", "income", "eth", "sex", "education", "hear", "vision", "emp", "res", "sleep30", "sleepback", "sleepdrug", "chr_cond" ,  "hosp", "hosp_stay"))
# summary(a.out)
# plot(a.out)
# summary(a.out$imputations[[20]]$sppb)# similar to original data. 
# summary(plm_exam$sppb)# similar distribution to imputed 



### Multi-state modelling 

# A bit of data wrangling

plm_exam <- readRDS(file = "dataset.Rda")

## Change direction of health states
plm_exam <- plm_exam %>%
  mutate(demclas = case_when(
    demclas == 3 ~ 1, # healthy
    demclas == 2 ~ 2, # possible dementia
    demclas == 1 ~ 3, # probable dementia
    demclas == 0 ~ 4  # death
  )) %>%
  mutate(demclas = as.integer(demclas)) # as integer following msm package requirements

## Ordering the IDs and selecting the variables
plm_exam <- plm_exam[order(plm_exam$id), ] %>%
  dplyr::select(id, age, demclas, transition, sppb, sisindex, wc, dep, anx, chr_cond, smk_before, smk_now, 
                pain, exercise, income, sex, hear, vision, emp, res, hosp_stay, frailty, eth, wave) %>% # TBC
  as.data.frame()


### CASE 1: 3-STATE MODEL (DISEASE-FREE -> DISEASE -> DEATH)

# Data handling to transform our state variable into 3 states
plm_exam_3_states <- plm_exam %>%
  mutate(demclas = case_when(
    demclas == 1 ~ 1, # healthy
    demclas %in% c(2, 3) ~ 2, # dementia
    demclas == 4 ~ 3 # death
  ))

# Frequency table
## A useful way to summarise multi-state data is a frequency table of pairs of consecutive states
statetable.msm(state = demclas, subject = id, data = plm_exam_3_states)
# 1410 dementia-free deaths, and 2016 deaths from dementia state

# Specifying a model
## A model is ruled by a transition intensity matrix *Q*
## In our example, there are three possible states through which older adults can move
## Disease-free, dementia, and death
## Firstly, we assume that the patient can advance or recover from dementia
Q_1 <- rbind(c(0, 0.1, 0.01),
             c(0, 0, 0.1),
             c(0, 0, 0))

# Model: Maximum likelihood estimation
model.msm <- msm(demclas ~ transition, subject = id, data = plm_exam_3_states,
                 qmatrix = Q_1, 
                 gen.inits = TRUE,
                 deathexact = 3, # we exactly know the day which someone dies
                 control = list(fnscale = 10, # this ensures that optimisation is done on a normalised scale (years), helping to avoid numerical computation problems
                                trace = 1,
                                REPORT = 1)) # trace and REPORT arguments are used to see the progress of the optimisation process 

# Mean sojourn time
## The mean sojourn time in a state *r* is the expected length of one period spent in that state
options(digits = 3)
sojourn.msm(model.msm)

# Total length of stay in states
## For someone in state *r* now, over the next t = 5 years they are expected to spend a total of *X* years in state 1, *X* years in state 2,...
totlos.msm(model.msm, t = 5)

# Predicting the prevalence of states over time
## Expected prevalence of state *s* at time *t* defined by the transition probabilities from state *r* to state *s*, averaged by the proportion who start in each state *r*
## Model fit can be checked by comparing the expected prevalence with the observed proportion in the data occupying each state at each time
plot.prevalence.msm(model.msm)


# Does our model appear to fit the data?
## If death times are known, we could check the fit of a msm model by comparing estimated survival probabilities with Kaplan-Meier estimates
plot.survfit.msm(model.msm, from = 1, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS without dementia", 
                 col = "red",
                 col.surv = "black")

plot.survfit.msm(model.msm, from = 2, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS with dementia", 
                 col = "red",
                 col.surv = "black")


#######################################
############ NEW ANALYSES #############
#######################################

## saving in an external file the model
saveRDS(model.msm, file = "model_missclassif.Rda")
saveRDS(plm_exam_elect, file = "plm_exam_elect.Rda")
saveRDS(model.elect.1, file = "model_elect_1.Rda")
saveRDS(plm_exam, file = "plm_exam_clean.Rda")

## restoring the file
plm_exam <- readRDS(file = "dataset.Rda")
model.msm <- readRDS(file = "model_missclassif.Rda")
plm_exam_elect <- readRDS(file = "plm_exam_elect.Rda")
model_elect_1 <- readRDS(file = "model_elect_1.Rda")
plm_exam_clean <- readRDS(file = "plm_exam_clean.Rda")


# Data wrangling
## Create the sppb10 variable (less than 10 in the SPPB is considered a poor physical function)
plm_exam_clean <- plm_exam_clean %>%
  mutate(sppb10 = ifelse(sppb >= 10, 1, 0))

## Create income variable (less than 20000$/year is considered low income)
plm_exam_clean <- plm_exam_clean %>%
  mutate(income2 = ifelse(income <= 2, 0, 1))


### CASE 2: 4-STATE MODEL (DEMENTIA-FREE -> POSSIBLE DEMENTIA -> PROBABLE DEMENTIA -> DEATH)
ids <- plm_exam %>% filter(wave == 1) %>% left_join(plm_exam %>% filter(wave == 12), by = "id") %>% .$id

# Frequency table
## A useful way to summarise multi-state data is a frequency table of pairs of consecutive states
statetable.msm(state = demclas, subject = id, data = plm_exam)
# 1397 dementia-free deaths, 447 deaths from state 2, and 1485 deaths from state 3


# Specifying a model
## A model is ruled by a transition intensity matrix *Q*
## In our example, there are four possible states through which older adults can move
## Dementia-free, possible dementia, probable dementia, and death
## Firstly, we assume that the patient can advance or recover from consecutive states while alive, and die from any state
Q <- rbind(c(0, 0.1, 0.1, 0.1),
           c(0, 0, 0.1, 0.1),
           c(0, 0, 0, 0.1),
           c(0, 0, 0, 0))

# ematrix for missclassified models
ematrix <- rbind(c(-0.11, 0.1, 0.01, 0),
                 c(0.1, -0.2, 0.1, 0),
                 c(0.1, 0.1, -0.2, 0),
                 c(0, 0, 0, 0))

# another possible option
Q <- rbind(c(-0.3, 0.1, 0.1, 0.1),
           c(0, -0.2, 0.1, 0.1),
           c(0, 0, -0.1, 0.1),
           c(0, 0, 0, 0))


## Model: Maximum likelihood estimation
model.msm <- msm(demclas ~ transition, 
                   subject = id, 
                   data = plm_exam_clean,
                 qmatrix = Q,
                 deathexact = 4, 
                 control = list(fnscale = 10000, 
                                trace = 1,
                                REPORT = 1),
                 center = FALSE,
                 death = TRUE,
                 gen.inits = TRUE) 

model.msm.cov <- msm(demclas ~ transition, 
                 subject = id, 
                 data = plm_exam_clean,
                 covariates = ~ sppb10 + exercise,
                 qmatrix = Q,
                 deathexact = 4,
                 center = FALSE,
                 death = TRUE,
                 gen.inits = TRUE,
                 control = list(fnscale = 10000,
                                trace = 1,
                                REPORT = 1)) 

model.msm.cov.2 <- msm(demclas ~ transition, 
                     subject = id, 
                     data = plm_exam_clean,
                     covariates = ~ sppb10 + exercise + income + sex,
                     qmatrix = Q,
                     deathexact = 4,
                     center = FALSE,
                     death = TRUE,
                     control = list(fnscale = 10000,
                                    trace = 1,
                                    REPORT = 1),
                     gen.inits = TRUE) 

model.msm.cov.3 <- msm(demclas ~ transition, 
                       subject = id, 
                       data = plm_exam_clean,
                       covariates = ~ sppb10 + exercise + income2 + sex +
                         dep + smk_before,
                       qmatrix = Q,
                       deathexact = 4,
                       center = FALSE,
                       death = TRUE,
                       control = list(fnscale = 10000,
                                      trace = 1,
                                      REPORT = 1,
                                      maxit = 500),
                       gen.inits = TRUE) 


# Covariate selection
AIC(model.msm, model.msm.cov, model.msm.cov.2, model.msm.cov.3)


## problematic IDs with two different states at the same time point
prob.ids <- plm_exam %>%
  filter(demclas == 4 & transition == 0) %>% .$id

### PREDICTION FROM MULTI-STATE MODELS
# Multi-state models describe a process evolving over time
# Given that fitted model, we usually want to predict what will happen to a person who follows that model

# Mean sojourn time
options(digits = 3)
sojourn.msm(model.msm)

### *Interpretation*
# an average older adult would spend sojourn.msm(model.msm.1)[1, 1] years in state 1
# sojourn.msm(model.msm.1)[2, 1] years in state 2
# sojourn.msm(model.msm.1)[3, 1] years in state 3

### plot!
# Plotting fitted survival probability curves from every transient state to the final
tiff("surv.tiff", height = 6, width = 8, res = 300, units = "in")
plot(model.msm, from = 1:3, to = 4)
dev.off()

# Transition probability matrix
pmatrix.msm(model.msm, ci = "normal", B = 100, t = 10)

# this shows that study participants in state 1 at time zero have (approx.) a 60.47% probability of still being in State 1 after years, 7.53% probability of being in state 2, 10.53% probability of being state 3, and 21.48% of being in state 4

# For instance, we can calculate transition probabilities over time, say, at 1 to 5 years
matrix <- stacked.data.msm(model = model.msm.1, tstart = 0, tforward = 5, tseqn = 6)
subset(matrix, matrix$from == "State 1")

### *Interpretation*
# this returns a tidy dataset with all transition probs from and to every state over tseq = 6 equally-spaced time intervals between time zero and time five
# we can see that the probability of still being in state 1, starting from state 1, is (approx) 86% after one year, 78% after two years, 71% after three years, 66% after four years, and 61% after five years

library(msm.stacked)

### plot!
# Stacked probabilities plots
tiff("probs.tiff", height = 6, width = 8, res = 300, units = "in")
stacked.plot.msm(model = model.msm, tstart = 0, tforward = 10) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "To:") +
  scale_x_continuous(breaks = seq(0, 10, 2))
dev.off()

# Total length of stay in states
totlos.msm(model.msm, t = 10)

### *Interpretation*
# For someone in state 1 now, over the next t = 5 years they are expected to spend a total of 3.8 years in state 1, 0.38 years in state 2, 0.32 years in state 3, and 0.50 years in state 4


# Expected first passage time
efpt.msm(model.msm, tostate = 4)

### *Interpretation*
# The expectation of the time until someone in state 1 next enters a state 4 (the "passage" time or "hitting" time) is 15.56 years
# The expectation of the time until someone in state 2 enters a state 4 is 12.43 years
# The expectation of the time until someone in state 3 enters a state 4 is 6.74 years


### plot!
# Predicting the prevalence of states over time
plot.prevalence.msm(model.msm)


#### customised plot!
prev <- prevalence.msm(model.msm)

# reshape observed prevalence
do1 <-as_tibble(row.names(prev$Observed)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
do2 <-as_tibble(prev$Observed) %>% mutate(type = "observed")
do <- cbind(do1,do2) %>% dplyr::select(-Total)
do_l <- do %>% gather(state, number, -time, -type)
library(tidyr)

# reshape expected prevalence
de1 <-as_tibble(row.names(prev$Expected)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
de2 <-as_tibble(prev$Expected) %>% mutate(type = "expected")
de <- cbind(de1,de2) %>% dplyr::select(-Total) 
de_l <- de %>% gather(state, number, -time, -type) 

# bind into a single data frame
prev_l <-rbind(do_l,de_l) %>% mutate(type = factor(type),
                                     state = factor(state),
                                     time = round(time,3))

# plot for comparison
prev_gp <- prev_l %>% group_by(state)
prev_l %>% ggplot() +
  geom_line(aes(time, number, color = type)) +
  labs(x = "Years",
       y = "Number of older adults at risk",
       col = "Data") + 
  facet_wrap(~state) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  theme_light()


### plot!
# Does our model appear to fit the data?
## If death times are known, we could check the fit of a msm model by comparing estimated survival probabilities with Kaplan-Meier estimates
par(mfrow = c(1,1))
tiff("surv_1_covmodel.tiff", height = 6, width = 6, res = 300, units = "in")
plot.survfit.msm(model.msm.cov.3, from = 1, 
                 ylab = "Survival probability", 
                 xlab = "Time after entering in NHATS (dementia-free)", 
                 col = "red",
                 col.surv = "black")
dev.off()
tiff("surv_2_covmodel.tiff", height = 6, width = 6, res = 300, units = "in")
plot.survfit.msm(model.msm.cov.3, from = 2, 
                 ylab = "Survival probability", 
                 xlab = "Time after entering in NHATS (possible dementia)", 
                 col = "red",
                 col.surv = "black")
dev.off()
tiff("surv_3_covmodel.tiff", height = 6, width = 6, res = 300, units = "in")
plot.survfit.msm(model.msm.cov.3, from = 3, 
                 ylab = "Survival probability", 
                 xlab = "Time after entering in NHATS (probable dementia)", 
                 col = "red",
                 col.surv = "black")
dev.off()
### *Interpretation*
# this slightly overestimates the probability of death, because the people whose state we know at later times are more likely to be the people who have died
# we don't know the state at later times for people whose outcomes were censored at the end of the study when they were still alive
# we could correct for this bias if we impose an "end of study" time for every person, and exclude people from the denominator after this time, even people who we know to have died


### MULTI-STATE MODELS WITH COVARIATES

# First, we create a variable indicating if older adults meet the SPPB criteria for good physical performance in males (>8) and females (>7)
plm_exam <- plm_exam %>%
  mutate(sppb_binary = case_when(
    sex == 1 & sppb > 8 ~ 1,
    sex == 1 & sppb <= 8 ~ 0,
    sex == 2 & sppb > 7 ~ 1, 
    sex == 2 & sppb <= 7 ~ 0
  ),
  sppb_10 = ifelse(sppb < 10, 0, 1))

# Next, we must select which transitions should be affected by which covariates
# In the next model we assumed that we have sufficient data on all transitions and that physical performance may affect all possible transitions
# We could determine even if identical effects of same covariate happen on different transitions
# For parsimony sake, we assume that the effect of having a good or bad physical performance is the same for all transitions
### Other possibility: set covariates to a list of linear model formulae to specify which intensities get which covariates
### Model example:
# diff_intensities_model <- msm(demclas ~ transition,
                              # subject = id,
                              # data = plm_exam,
                              # qmatrix = Q,
                              # covariates = list("1-2" = ~ sppb + smoke_now,
                                                # "2-3" = ~ sppb))

### Exploratory Analysis of the potential covariates
tiff("dep.tiff", height = 7, width = 7, units = "in", res = 300)
plm_exam %>%
  filter(!is.na(dep)) %>%
  ggplot(aes(dep, demclas)) +
  geom_jitter(alpha = 0.05) +
  geom_text(data = plm_exam %>%
              group_by(dep, demclas) %>%
              summarize(n = n(),
                        total = nrow(plm_exam),
                        prop = n / total,
                        n_prop = scales::label_percent(accuracy = 0.1)(prop)),
            aes(label = n), vjust = -7) +
  labs(y = "States",
       x = "Depression diagnosis") +
  scale_x_discrete(breaks = c(0, 1),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("Dementia-free",
                                "Possible dementia",
                                "Probable dementia",
                                "Death")) +
  theme_minimal()
dev.off()
library(tidybayes)

tiff("age.tiff", width = 7, height = 6, res = 300, units = "in")
plm_exam %>%
  mutate(demclas = factor(demclas)) %>%
  filter(!is.na(demclas)) %>%
  ggplot(aes(x = age, y = demclas)) +
  tidybayes::stat_gradientinterval() +
  stat_dotsinterval(slab_colour = "gold") +
  labs(x = "Age",
       y = "States") +
  scale_x_continuous(breaks = seq(65, 110, 5)) + 
  scale_y_discrete(breaks = c(1, 2, 3, 4),
                     labels = c("Dementia-free",
                                "Possible dementia",
                                "Probable dementia",
                                "Death")) +
  theme_minimal()
dev.off()  


# Fitting the model with our new covariate
model.msm.cov <- msm(demclas ~ transition, 
                     subject = id, 
                     data = plm_exam_f,
                     covariates = ~ sex + eth + income,
                     qmatrix = Q, 
                     deathexact = 4, 
                     control = list(fnscale = 4000, 
                                    trace = 1,
                                    REPORT = 1,
                                    reltol = 1e-16))

model.msm.cov.1 <-  msm(demclas ~ transition, 
                        subject = id, 
                        data = plm_exam_f,
                        covariates = ~ sex + income + chr_cond + sppb_10,
                        qmatrix = Q, 
                        deathexact = 4, 
                        control = list(fnscale = 4000, 
                                       trace = 1,
                                       REPORT = 1,
                                       reltol = 1e-16))

model.msm.cov.2 <- msm(demclas ~ transition, 
                     subject = id, 
                     data = plm_exam,
                     covariates = ~ sppb_10 + anx + dep,
                     qmatrix = Q, 
                     deathexact = 4, 
                     control = list(fnscale = 10000, 
                                    trace = 1,
                                    REPORT = 1))

model.msm.cov.3 <- msm(demclas ~ transition, 
                       subject = id, 
                       data = plm_exam,
                       covariates = ~ sppb_10 + anx + dep + exercise,
                       qmatrix = Q, 
                       deathexact = 4, 
                       control = list(fnscale = 10000, 
                                      trace = 1,
                                      REPORT = 1))

# transform variables in factors
plm_exam_f <- plm_exam %>%
  mutate(income = factor(income),
         sex = factor(sex), # 1 = males, 2 = females
         eth = factor(eth))


model.msm.cov.4 <- msm(demclas ~ transition, 
                       subject = id, 
                       data = plm_exam_f,
                       covariates = ~ sppb_10 + anx + dep + exercise + sex,
                       qmatrix = Q, 
                       deathexact = 4, 
                       control = list(fnscale = 10000, 
                                      trace = 1,
                                      REPORT = 1))

logliks <- c(logLik(model.msm.cov), logLik(model.msm.cov.1))
AICs <- AIC(model.msm.cov, model.msm.cov.1)
cbind(logliks, AICs)

# Output
print(model.msm.cov.1, digits = 2)

### *Interpretation*
# Hazard ratios for sppb represent how the instantaneous risk of making a particular transition is modified by the covariate
# In our model, someone in state 1, with a good physical performance (males >8 and females >7 in the SPPB), is a as.data.frame(hazard.msm(model.msm.cov))[1, 1]% lower risk of progression to state 2 (dementia), at any time, compared to someone with poor physical performance
# Shockingly, someone in state 1, with a good physical performance, is a as.data.frame(hazard.msm(model.msm.cov))[3, 1]% lower risk of progression to state 4 (death), at any time, compared to someone with poor physical performance

p.male.low.no.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                               sppb_10 = 0, 
                                                               income = 0, 
                                                               chr_cond = 0),
                     ci = "normal", B = 100)

p.male.low.yes.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 0, 
                                                                             income = 0, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.male.high.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 0, 
                                                                             income = 1, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.male.black.high.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 0, 
                                                                             income = 1, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.male.white.low.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 1, 
                                                                             income = 0, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.male.white.low.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 1, 
                                                                             income = 0, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.male.white.high.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 1, 
                                                                             income = 1, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.male.white.high.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                             eth = 1, 
                                                                             income = 1, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.female.black.low.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 0, 
                                                                             income = 0, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.female.black.low.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 0, 
                                                                             income = 0, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.female.black.high.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 0, 
                                                                             income = 1, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.female.black.high.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 0, 
                                                                             income = 1, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.female.white.low.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 1, 
                                                                             income = 0, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.female.white.low.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 1, 
                                                                             income = 0, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)
p.female.white.high.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 1, 
                                                                             income = 1, 
                                                                             chr_cond = 0),
                                   ci = "normal", B = 100)
p.female.white.high.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                             eth = 1, 
                                                                             income = 1, 
                                                                             chr_cond = 1),
                                   ci = "normal", B = 100)

p.male.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                   sppb_10 = 0),
                         ci = "normal", B = 100)

p.male.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 1,
                                                                   sppb_10 = 1),
                         ci = "normal", B = 100)

p.female.no <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                   sppb_10 = 0),
                         ci = "normal", B = 100)

p.female.yes <- pmatrix.msm(model.msm.cov.1, t = 5, covariates = list(sex = 2,
                                                                   sppb_10 = 1),
                         ci = "normal", B = 100)
### combine matrix
p_14 <- rbind(p.male.black.low.no[1, 4],
              p.female.black.low.no[1, 4],
              p.male.white.low.no[1, 4],
              p.female.white.low.no[1, 4],
              p.male.black.high.no[1, 4],
              p.female.black.high.no[1, 4],
              p.male.white.high.no[1, 4],
              p.female.white.high.no[1, 4],
              p.male.black.low.yes[1, 4],
              p.female.black.low.yes[1, 4],
              p.male.white.low.yes[1, 4],
              p.female.white.low.yes[1, 4],
              p.male.black.high.yes[1, 4],
              p.female.black.high.yes[1, 4],
              p.male.white.high.yes[1, 4],
              p.female.white.high.yes[1, 4])

p_14 <- rbind(p.male.no[1, 4],
              p.male.yes[1, 4],
              p.female.no[1, 4],
              p.female.yes[1, 4])

cbind(expand.grid(sex = c("Male", "Female"), sppb_10 = c("Poor", "Good")), p_14)


### plot!
library(msm.stacked)
# plotting when older adults are below healthy cutoff points on SPPB

tiff("probs_smk.tiff", height = 6, width = 8, res = 300, units = "in")
(stacked.plot.msm(model = model.elect2, 
                                tstart = 0,
                                exclude = "State 4",
                                tforward = 20, 
                 covariates = list(sex = 1,
                                   income2 = 1,
                                   exercise1 = 1,
                                   sppb101 = 0,
                                   smk_before1 = 0,
                                   age = -15)) +
  scale_fill_brewer(palette = "Greys") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(fill = "To:",
       title = "Non-smoker",
       x = "Time (years)"))/
  (stacked.plot.msm(model = model.elect2, 
                   tstart = 0,
                   exclude = "State 4",
                   tforward = 20, 
                   covariates = list(sex = 1,
                                     income2 = 1,
                                     exercise1 = 1,
                                     sppb101 = 0,
                                     smk_before1 = 1,
                                     age = -15)) +
  scale_fill_brewer(palette = "Greys") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "To:",
       title = "Regular smoker",
       x = "Time (years)"))
dev.off()


tiff("probs_smk_col.tiff", height = 6, width = 8, res = 300, units = "in")
(stacked.plot.msm(model = model.elect2, 
                  tstart = 0,
                  exclude = "State 4",
                  tforward = 20, 
                  covariates = list(sex = 1,
                                    income2 = 1,
                                    exercise1 = 1,
                                    sppb101 = 0,
                                    smk_before1 = 0,
                                    age = -15)) +
    scale_fill_viridis_d(option = "plasma") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(fill = "To:",
         title = "Non-smoker",
         x = "Time (years)"))/
  (stacked.plot.msm(model = model.elect2, 
                    tstart = 0,
                    exclude = "State 4",
                    tforward = 20, 
                    covariates = list(sex = 1,
                                      income2 = 1,
                                      exercise1 = 1,
                                      sppb101 = 0,
                                      smk_before1 = 1,
                                      age = -15)) +
     scale_fill_viridis_d(option = "plasma") +
     theme_minimal() +
     theme(legend.position = "bottom") +
     labs(fill = "To:",
          title = "Regular smoker",
          x = "Time (years)"))
dev.off()

tiff("probs_women.tiff", height = 6, width = 8, res = 300, units = "in")
(stacked.plot.msm(model = model.elect2, tstart = 0, tforward = 10, 
                 covariates = list(sppb101 = 0,
                                   sex2 = 2), exclude = "State 4") +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "To:",
       title = "Older women with poor physical performance") +
  scale_x_continuous(breaks = seq(0, 10, 2))) / 
  (stacked.plot.msm(model = model.elect2, tstart = 0, tforward = 10, 
                    covariates = list(sppb101 = 0,
                                      sex2 = 2,
                                      age = 0), exclude = "State 4") +
     scale_fill_viridis_d(option = "plasma") +
     theme_minimal() +
     theme(legend.position = "bottom") +
     labs(fill = "To:",
          title = "Older women with good physical performance") +
     scale_x_continuous(breaks = seq(0, 10, 2)))
dev.off()


# plotting when older adults are above healthy cutoff points on SPPB
good_pp_plot <- stacked.plot.msm(model = model.msm.cov.3, tstart = 0, tforward = 5, 
                 covariates = list(smk_before = 1)) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "To:",
       title = "Good physical functioning")

# combined plot
tiff("cov_sppb.tiff", height = 6, width = 10, res = 300, units = "in")
ggarrange(bad_pp_plot, good_pp_plot, 
          common.legend = TRUE,
          legend = "bottom")
dev.off()

totlos.msm(model.msm.cov.1, tot = 20, covariates = list(sppb_10 = 1))

# Transition probability matrix by covariate classes
list("Good physical performance" = pmatrix.msm(model.msm.cov, covariates = list(sppb_binary = 1)),
     "Poor physical performance" = pmatrix.msm(model.msm.cov, covariates = list(sppb_binary = 0)))

### *Interpretation*
# A good physical performance is a protective factor (lower transition probabilities for sppb_binary = 1)


# Total length of stay in states
# Older adults with poor physical performance
totlos.msm(model.msm.cov.3, tot = 10, covariates = list(sppb_10 = 0))

# Older adults with good physical performance
totlos.msm(model.msm.cov.3, tot = 10, covariates = list(sppb_10 = 1))

### *Interpretation*
# For older adults in state 1 now with good physical performance, over the next tot = 10 years they are expected to spend a total of totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 1))[1] years in state 1, totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 1))[2] years in state 2, totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 1))[3] years in state 3, and totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 1))[4] years in state 4
# In contrast, older adults in state 1 with poor physical performance, over the next 10 years they are expected to spend a total of totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 0))[1] years in state 1, totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 0))[2] years in state 2, totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 0))[3] years in state 3, and totlos.msm(model.msm.cov, tot = 10, covariates = list(sppb_binary = 0))[4] years in state 4

# Expected first passage time
efpt.msm(model.msm.cov, tostate = 4, covariates = list(sppb_binary = 0))

### *Interpretation*
# Predictions for older adults with poor physical performance
# The expectation of the time until someone in state 1 next enters a state 4 (the "passage" time or "hitting" time) is 11.75 years
# The expectation of the time until someone in state 2 enters a state 4 is 8.57 years
# The expectation of the time until someone in state 3 enters a state 4 is 4.76 years

# The expectations for older adults with good physical performance should be taken into consideration
# For example, the expectation of the time until someone in state 1 next enters a state 4 is 47 years


### plot!
# Predicting the prevalence of states over time
# Let's compare the observe and expected prevalences
# Note: the subset argument is used to restrict the observed prevalences to those observed in the indicated set of patient IDs

# subsets: poor and good physical performance
males_poor <- unique(prueba2$id[prueba2$sex == 1 &
                                 prueba2$sppb10 == 0])
good_data <- unique(prueba2$id[prueba2$income2==1 & 
                                 prueba2$smk_before ==1 &
                                 prueba2$sex == 0 &
                                 prueba2$sppb10 == 1 &
                                 prueba2$exercise==1])

### plot!
# poor physical performance
tiff("prev_poor.tiff", height = 6, width = 8, res = 300, units = "in")
plot.prevalence.msm(model.elect2, covariates = list(sex = 1,
                                                    sppb10 = 0), subset = males_poor)
dev.off()
# good physical performance
tiff("prev_good.tiff", height = 6, width = 8, res = 300, units = "in")
plot.prevalence.msm(model.msm.cov.1, covariates = list(income = 1), subset = good_data)
dev.off()

  
### *Interpretation*
# This is awesome. When older adults conserve a good physical performance, the prevalence of dementia is so low, and we can observe a sharp drop at 10 years time point in state 1 associated with the increase of occupancy of state 4
# That indicates that the majority of older adults die in a dementia-free state due to another cause
# So, we could draw the conclusion that having a good physical performance is a protective factor against dementia


### plot!
# Does our model appear to fit the data?
## If death times are known, we could check the fit of a msm model by comparing estimated survival probabilities with Kaplan-Meier estimates
par(mfrow = c(1,2))

# Note: the empirical curve is plotted for the full population

# From state 1
## Poor physical performance
plot.survfit.msm(model.elect2, from = 1, 
                 ylab = "Survival probability", 
                 xlab = "Time after entering in NHATS (healthy)",
                 main = "Poor physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sex = 0, 
                                   sppb10 = 0))

## Good physical performance
plot.survfit.msm(model.msm.cov, from = 3, 
                 ylab = "Survival probability", 
                 xlab = "Time after entering in NHATS (healthy)",
                 main = "Good physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sex = 2))


# From state 2
## Poor physical performance
plot.survfit.msm(model.msm.cov.3, from = 2, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS (possible dementia)",
                 main = "Poor physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sppb_10 = 0))

## Good physical performance
plot.survfit.msm(model.msm.cov.3, from = 2, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS (possible dementia)",
                 main = "Good physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sppb_10 = 1))


# From state 3
## Poor physical performance
plot.survfit.msm(model.msm.cov, from = 3, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS (probable dementia)",
                 main = "Poor physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sppb_binary = 0))

## Good physical performance
plot.survfit.msm(model.msm.cov, from = 3, 
                 ylab = "Survival probability", 
                 xlab = "Time after enter in NHATS (probable dementia)",
                 main = "Good physical performance",
                 col = "red",
                 col.surv = "black",
                 covariates = list(sppb_binary = 1))



# ESTIMATION OF LIFE EXPECTANCIES USING CONTINUOUS-TIME MULTI-STATE SURVIVAL MODELS

# Life expectancy in a specified state is defined as the expected remaining number of years in that state and is conditional on current age

# Create the mean age variable for centering age
mean.age <- mean(plm_exam_clean$age, na.rm = TRUE)

# Dataset creation following the package prerequisites
plm_exam_elect <- plm_exam_clean %>%
  dplyr::mutate(age = ifelse(demclas == 4, age + 1, age)) %>%
  dplyr::rename(state = demclas,
                years = age) %>%
  dplyr::mutate(age = years - mean.age)

### order by id and years
plm_exam_elect <- plm_exam_elect[order(plm_exam_elect$id, plm_exam_elect$years), ]

### creating the age variable centered
plm_exam_elect <- plm_exam_elect %>%
  rename(time_transition = age) %>%
  mutate(age = years - mean(years, na.rm = TRUE))

### fix the ages adjusting the first collected age and adding the transition time
dataset <- plm_exam_elect %>% filter(!duplicated(id)) %>%
  dplyr::select(id, years)

### New variable creation: newyear
prueba <- plm_exam_elect %>%
  full_join(dataset %>% dplyr::mutate(newyear = years), by = "id") %>%
  dplyr::mutate(newyear = newyear + age)

### Create the new dataset
prueba <- prueba %>%
  dplyr::select(id, state, dep, smk_before, exercise, income, sex, 
                wave, sppb, newyear) %>%
  dplyr::rename(age = newyear) %>%
  dplyr::mutate(age = age - mean.age)

### Order the IDs and ages
prueba <- prueba[order(prueba$id, prueba$age), ]


###### LOAD *prueba*
prueba <- readRDS(file = "prueba_clean.Rda")

### Remove different states in the same age
prueba2 <- prueba %>% 
  dplyr::group_by(id) %>%
  filter(!duplicated(age)) %>% 
  dplyr::ungroup()

prueba2 <- prueba2 %>%
  dplyr::mutate(sex = as.character(sex),
                sppb = as.character(sppb),
                exercise = as.character(exercise),
                smk_before = as.character(smk_before))


# Fitting the model into msm
model.elect2 <- msm(state ~ age, 
                   subject = id, 
                   data = prueba2,
                   covariates = ~ age + sppb10 +
                     sex,
                   qmatrix = Q, 
                   deathexact = 4, 
                   control = list(fnscale = 10000,
                                  maxit = 1000,
                                  trace = 1,
                                  REPORT = 1,
                                  reltol = 1e-8),
                   center = FALSE,
                   death = TRUE)

model.elect.age <- msm(state ~ age, 
                   subject = id, 
                   data = prueba2,
                   covariates = ~ age,
                   qmatrix = Q, 
                   deathexact = 4, 
                   control = list(fnscale = 10000,
                                  maxit = 1000,
                                  trace = 1,
                                  REPORT = 1,
                                  reltol = 1e-16),
                   center = FALSE,
                   death = TRUE)

# Model fit
AIC(model.elect2, model.elect)

# save the model
saveRDS(model.elect2, file = "new_model.Rda")
model.elect2 <- readRDS(file = "new_model.Rda")

# Defining the data used for the distribution of the living states conditional on a specified age (time interval since baseline)
# This distribution is needed to compute marginal life expectancies
sddata <- prueba2[prueba2$state %in% c(1, 2, 3),]


# It is up the user how to choose this data
# Here we specify age = 0 (older adults with the mean age at baseline ~ mean(plm_exam %>% filter(wave == 1) %>% .$age, na.rm = TRUE)) and sppb_binary = 0, poor physical performance

### Defining the parameters for the LEs
age <- 75 - mean.age
age.max <- 110 - mean.age
sex <- 0
sppb10 <- 1
smk <- 0
exercise <- 1

# LE estimates
LEs <- elect(model.elect2,
             b.covariates = list(age = age,
                                 sppb10 = 0,
                                 sex = 1),
             statedistdata = sddata,
             h = 0.5,
             time.scale.msm = "years",
             age.max = age.max,
             S = 100,
             setseed = 2012024)
summary.elect(LEs)

LEs1 <- elect(model.elect2,
             b.covariates = list(age = age,
                                 sppb101 = 1,
                                 sex = 1),
             statedistdata = sddata,
             h = 0.5,
             time.scale.msm = "years",
             age.max = age.max,
             S = 100,
             setseed = 2012024)
summary.elect(LEs1)
plot.elect(LEs1)


### mean and 95% CI from the differences between predicted values of the 500 simulations
library(tidybayes)
mean_qi(LEs$sim[, 1] - LEs1$sim[, 1])
mean_qi(data.1$`LEs$sim[, 13]` - data.2$`LEs1$sim[, 13]`[1:430])


# LE estimates for a range of age
mod_summaries <- list()

for (i in seq(-14, 22, 0.5)) {
  mod_summaries[[i+15]] <- elect(model.elect.age, 
                                 b.covariates = list(age = i),
                                 statedistdata = sddata, 
                                 time.scale.msm = "years",
                                 h = 0.5, 
                                 age.max = 32, 
                                 S = 50,
                                 setseed = 2012024)
}

### organise data for plot
median_qi(mod_summaries[[1]]$sim[, 13]) %>% 
  rbind(mean_qi(mod_summaries[[2]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[3]]$sim[, 13])) %>% 
  rbind(mean_qi(mod_summaries[[4]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[5]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[6]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[7]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[8]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[9]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[10]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[11]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[12]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[13]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[14]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[15]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[16]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[17]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[18]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[19]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[20]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[21]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[22]]$sim[, 13])) %>%
  cbind(age = seq(-14 + mean.age, -3.5 + mean.age, 0.5)) %>%
  ggplot(aes(x = age, y = y)) +
  geom_pointinterval(aes(ymin = ymin,
                         ymax = ymax))



# Life expectancy estimates for older adults with poor physical performance
mod_summaries <- list()

for (i in seq(-15.55227, 24.44773, 1)) {
  mod_summaries[[i+16.55227]] <- elect(model.elect2, 
                      b.covariates = list(age = i,
                                          sppb10 = 0,
                                          sex = 1),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = age.max, 
                      S = 100,
                      setseed = 2012024)
}

# Life expectancy estimates for older adults with poor physical performance
mod_summaries_poor <- list()

for (i in seq(-15.55227, 24.44773, 1)) {
  mod_summaries_poor[[i+16.55227]] <- elect(model.elect2, 
                                 b.covariates = list(age = i,
                                                     sppb10 = 0,
                                                     sex = 2),
                                 statedistdata = sddata, 
                                 time.scale.msm = "years",
                                 h = 0.5, 
                                 age.max = age.max, 
                                 S = 100,
                                 setseed = 2012024)
}

mod_sum_males_poor <- mod_summaries
mod_sum_females_poor <- mod_summaries_poor
saveRDS(mod_sum_males_poor, file = "LE_males_poor.Rda")
saveRDS(mod_sum_females_poor, file = "LE_females_poor.Rda")

mod_summaries <- readRDS("LE_males_poor.Rda")
mod_summaries_poor <- readRDS("LE_females_poor.Rda")

length(unique(prueba2$id))
prueba2 %>% filter(sex == 1) %>% nrow()
prueba2 %>% filter(state == 1) %>% filter(!duplicated(id)) %>% nrow()
prueba2 %>% filter(sex == 2) %>% filter(!duplicated(id)) %>% nrow()

statetable.msm(demclas, subject = id, plm_exam)
summary.elect(mod_sum_males_poor[[21]])

mod_sum_females_good[[1]]$sim[, 10] # marginal dementia-free LE
mod_sum_females_good[[1]]$sim[, 11] # marginal MCI LE
mod_sum_females_good[[1]]$sim[, 12] # marginal dementia LE
mod_sum_females_good[[1]]$sim[, 13] # total LE


# Data frame with dementia-free estimates
free.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
           pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
           year = rep(seq(65, 105), each = 400),
           type = "Dementia-free",
           value = c(mod_sum_females_good[[1]]$sim[, 10],
                         mod_sum_females_poor[[1]]$sim[, 10],
                         mod_sum_males_good[[1]]$sim[, 10],
                         mod_sum_males_poor[[1]]$sim[, 10],
                     mod_sum_females_good[[2]]$sim[, 10],
                     mod_sum_females_poor[[2]]$sim[, 10],
                     mod_sum_males_good[[2]]$sim[, 10],
                     mod_sum_males_poor[[2]]$sim[, 10],
                     mod_sum_females_good[[3]]$sim[, 10],
                     mod_sum_females_poor[[3]]$sim[, 10],
                     mod_sum_males_good[[3]]$sim[, 10],
                     mod_sum_males_poor[[3]]$sim[, 10],
                     mod_sum_females_good[[4]]$sim[, 10],
                     mod_sum_females_poor[[4]]$sim[, 10],
                     mod_sum_males_good[[4]]$sim[, 10],
                     mod_sum_males_poor[[4]]$sim[, 10],
                     mod_sum_females_good[[5]]$sim[, 10],
                     mod_sum_females_poor[[5]]$sim[, 10],
                     mod_sum_males_good[[5]]$sim[, 10],
                     mod_sum_males_poor[[5]]$sim[, 10],
                     mod_sum_females_good[[6]]$sim[, 10],
                     mod_sum_females_poor[[6]]$sim[, 10],
                     mod_sum_males_good[[6]]$sim[, 10],
                     mod_sum_males_poor[[6]]$sim[, 10],
                     mod_sum_females_good[[7]]$sim[, 10],
                     mod_sum_females_poor[[7]]$sim[, 10],
                     mod_sum_males_good[[7]]$sim[, 10],
                     mod_sum_males_poor[[7]]$sim[, 10],
                     mod_sum_females_good[[8]]$sim[, 10],
                     mod_sum_females_poor[[8]]$sim[, 10],
                     mod_sum_males_good[[8]]$sim[, 10],
                     mod_sum_males_poor[[8]]$sim[, 10],
                     mod_sum_females_good[[9]]$sim[, 10],
                     mod_sum_females_poor[[9]]$sim[, 10],
                     mod_sum_males_good[[9]]$sim[, 10],
                     mod_sum_males_poor[[9]]$sim[, 10],
                     mod_sum_females_good[[10]]$sim[, 10],
                     mod_sum_females_poor[[10]]$sim[, 10],
                     mod_sum_males_good[[10]]$sim[, 10],
                     mod_sum_males_poor[[10]]$sim[, 10],
                     mod_sum_females_good[[11]]$sim[, 10],
                     mod_sum_females_poor[[11]]$sim[, 10],
                     mod_sum_males_good[[11]]$sim[, 10],
                     mod_sum_males_poor[[11]]$sim[, 10],
                     mod_sum_females_good[[12]]$sim[, 10],
                     mod_sum_females_poor[[12]]$sim[, 10],
                     mod_sum_males_good[[12]]$sim[, 10],
                     mod_sum_males_poor[[12]]$sim[, 10],
                     mod_sum_females_good[[13]]$sim[, 10],
                     mod_sum_females_poor[[13]]$sim[, 10],
                     mod_sum_males_good[[13]]$sim[, 10],
                     mod_sum_males_poor[[13]]$sim[, 10],
                     mod_sum_females_good[[14]]$sim[, 10],
                     mod_sum_females_poor[[14]]$sim[, 10],
                     mod_sum_males_good[[14]]$sim[, 10],
                     mod_sum_males_poor[[14]]$sim[, 10],
                     mod_sum_females_good[[15]]$sim[, 10],
                     mod_sum_females_poor[[15]]$sim[, 10],
                     mod_sum_males_good[[15]]$sim[, 10],
                     mod_sum_males_poor[[15]]$sim[, 10],
                     mod_sum_females_good[[16]]$sim[, 10],
                     mod_sum_females_poor[[16]]$sim[, 10],
                     mod_sum_males_good[[16]]$sim[, 10],
                     mod_sum_males_poor[[16]]$sim[, 10],
                     mod_sum_females_good[[17]]$sim[, 10],
                     mod_sum_females_poor[[17]]$sim[, 10],
                     mod_sum_males_good[[17]]$sim[, 10],
                     mod_sum_males_poor[[17]]$sim[, 10],
                     mod_sum_females_good[[18]]$sim[, 10],
                     mod_sum_females_poor[[18]]$sim[, 10],
                     mod_sum_males_good[[18]]$sim[, 10],
                     mod_sum_males_poor[[18]]$sim[, 10],
                     mod_sum_females_good[[19]]$sim[, 10],
                     mod_sum_females_poor[[19]]$sim[, 10],
                     mod_sum_males_good[[19]]$sim[, 10],
                     mod_sum_males_poor[[19]]$sim[, 10],
                     mod_sum_females_good[[20]]$sim[, 10],
                     mod_sum_females_poor[[20]]$sim[, 10],
                     mod_sum_males_good[[20]]$sim[, 10],
                     mod_sum_males_poor[[20]]$sim[, 10],
                     mod_sum_females_good[[21]]$sim[, 10],
                     mod_sum_females_poor[[21]]$sim[, 10],
                     mod_sum_males_good[[21]]$sim[, 10],
                     mod_sum_males_poor[[21]]$sim[, 10],
                     mod_sum_females_good[[22]]$sim[, 10],
                     mod_sum_females_poor[[22]]$sim[, 10],
                     mod_sum_males_good[[22]]$sim[, 10],
                     mod_sum_males_poor[[22]]$sim[, 10],
                     mod_sum_females_good[[23]]$sim[, 10],
                     mod_sum_females_poor[[23]]$sim[, 10],
                     mod_sum_males_good[[23]]$sim[, 10],
                     mod_sum_males_poor[[23]]$sim[, 10],
                     mod_sum_females_good[[24]]$sim[, 10],
                     mod_sum_females_poor[[24]]$sim[, 10],
                     mod_sum_males_good[[24]]$sim[, 10],
                     mod_sum_males_poor[[24]]$sim[, 10],
                     mod_sum_females_good[[25]]$sim[, 10],
                     mod_sum_females_poor[[25]]$sim[, 10],
                     mod_sum_males_good[[25]]$sim[, 10],
                     mod_sum_males_poor[[25]]$sim[, 10],
                     mod_sum_females_good[[26]]$sim[, 10],
                     mod_sum_females_poor[[26]]$sim[, 10],
                     mod_sum_males_good[[26]]$sim[, 10],
                     mod_sum_males_poor[[26]]$sim[, 10],
                     mod_sum_females_good[[27]]$sim[, 10],
                     mod_sum_females_poor[[27]]$sim[, 10],
                     mod_sum_males_good[[27]]$sim[, 10],
                     mod_sum_males_poor[[27]]$sim[, 10],
                     mod_sum_females_good[[28]]$sim[, 10],
                     mod_sum_females_poor[[28]]$sim[, 10],
                     mod_sum_males_good[[28]]$sim[, 10],
                     mod_sum_males_poor[[28]]$sim[, 10],
                     mod_sum_females_good[[29]]$sim[, 10],
                     mod_sum_females_poor[[29]]$sim[, 10],
                     mod_sum_males_good[[29]]$sim[, 10],
                     mod_sum_males_poor[[29]]$sim[, 10],
                     mod_sum_females_good[[30]]$sim[, 10],
                     mod_sum_females_poor[[30]]$sim[, 10],
                     mod_sum_males_good[[30]]$sim[, 10],
                     mod_sum_males_poor[[30]]$sim[, 10],
                     mod_sum_females_good[[31]]$sim[, 10],
                     mod_sum_females_poor[[31]]$sim[, 10],
                     mod_sum_males_good[[31]]$sim[, 10],
                     mod_sum_males_poor[[31]]$sim[, 10],
                     mod_sum_females_good[[32]]$sim[, 10],
                     mod_sum_females_poor[[32]]$sim[, 10],
                     mod_sum_males_good[[32]]$sim[, 10],
                     mod_sum_males_poor[[32]]$sim[, 10],
                     mod_sum_females_good[[33]]$sim[, 10],
                     mod_sum_females_poor[[33]]$sim[, 10],
                     mod_sum_males_good[[33]]$sim[, 10],
                     mod_sum_males_poor[[33]]$sim[, 10],
                     mod_sum_females_good[[34]]$sim[, 10],
                     mod_sum_females_poor[[34]]$sim[, 10],
                     mod_sum_males_good[[34]]$sim[, 10],
                     mod_sum_males_poor[[34]]$sim[, 10],
                     mod_sum_females_good[[35]]$sim[, 10],
                     mod_sum_females_poor[[35]]$sim[, 10],
                     mod_sum_males_good[[35]]$sim[, 10],
                     mod_sum_males_poor[[35]]$sim[, 10],
                     mod_sum_females_good[[36]]$sim[, 10],
                     mod_sum_females_poor[[36]]$sim[, 10],
                     mod_sum_males_good[[36]]$sim[, 10],
                     mod_sum_males_poor[[36]]$sim[, 10],
                     mod_sum_females_good[[37]]$sim[, 10],
                     mod_sum_females_poor[[37]]$sim[, 10],
                     mod_sum_males_good[[37]]$sim[, 10],
                     mod_sum_males_poor[[37]]$sim[, 10],
                     mod_sum_females_good[[38]]$sim[, 10],
                     mod_sum_females_poor[[38]]$sim[, 10],
                     mod_sum_males_good[[38]]$sim[, 10],
                     mod_sum_males_poor[[38]]$sim[, 10],
                     mod_sum_females_good[[39]]$sim[, 10],
                     mod_sum_females_poor[[39]]$sim[, 10],
                     mod_sum_males_good[[39]]$sim[, 10],
                     mod_sum_males_poor[[39]]$sim[, 10],
                     mod_sum_females_good[[40]]$sim[, 10],
                     mod_sum_females_poor[[40]]$sim[, 10],
                     mod_sum_males_good[[40]]$sim[, 10],
                     mod_sum_males_poor[[40]]$sim[, 10],
                     mod_sum_females_good[[41]]$sim[, 10],
                     mod_sum_females_poor[[41]]$sim[, 10],
                     mod_sum_males_good[[41]]$sim[, 10],
                     mod_sum_males_poor[[41]]$sim[, 10]))


# Data frame with MCI estimates
mci.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                        pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                        year = rep(seq(65, 105), each = 400),
                        type = "MCI",
                        value = c(mod_sum_females_good[[1]]$sim[, 11],
                                  mod_sum_females_poor[[1]]$sim[, 11],
                                  mod_sum_males_good[[1]]$sim[, 11],
                                  mod_sum_males_poor[[1]]$sim[, 11],
                                  mod_sum_females_good[[2]]$sim[, 11],
                                  mod_sum_females_poor[[2]]$sim[, 11],
                                  mod_sum_males_good[[2]]$sim[, 11],
                                  mod_sum_males_poor[[2]]$sim[, 11],
                                  mod_sum_females_good[[3]]$sim[, 11],
                                  mod_sum_females_poor[[3]]$sim[, 11],
                                  mod_sum_males_good[[3]]$sim[, 11],
                                  mod_sum_males_poor[[3]]$sim[, 11],
                                  mod_sum_females_good[[4]]$sim[, 11],
                                  mod_sum_females_poor[[4]]$sim[, 11],
                                  mod_sum_males_good[[4]]$sim[, 11],
                                  mod_sum_males_poor[[4]]$sim[, 11],
                                  mod_sum_females_good[[5]]$sim[, 11],
                                  mod_sum_females_poor[[5]]$sim[, 11],
                                  mod_sum_males_good[[5]]$sim[, 11],
                                  mod_sum_males_poor[[5]]$sim[, 11],
                                  mod_sum_females_good[[6]]$sim[, 11],
                                  mod_sum_females_poor[[6]]$sim[, 11],
                                  mod_sum_males_good[[6]]$sim[, 11],
                                  mod_sum_males_poor[[6]]$sim[, 11],
                                  mod_sum_females_good[[7]]$sim[, 11],
                                  mod_sum_females_poor[[7]]$sim[, 11],
                                  mod_sum_males_good[[7]]$sim[, 11],
                                  mod_sum_males_poor[[7]]$sim[, 11],
                                  mod_sum_females_good[[8]]$sim[, 11],
                                  mod_sum_females_poor[[8]]$sim[, 11],
                                  mod_sum_males_good[[8]]$sim[, 11],
                                  mod_sum_males_poor[[8]]$sim[, 11],
                                  mod_sum_females_good[[9]]$sim[, 11],
                                  mod_sum_females_poor[[9]]$sim[, 11],
                                  mod_sum_males_good[[9]]$sim[, 11],
                                  mod_sum_males_poor[[9]]$sim[, 11],
                                  mod_sum_females_good[[10]]$sim[, 11],
                                  mod_sum_females_poor[[10]]$sim[, 11],
                                  mod_sum_males_good[[10]]$sim[, 11],
                                  mod_sum_males_poor[[10]]$sim[, 11],
                                  mod_sum_females_good[[11]]$sim[, 11],
                                  mod_sum_females_poor[[11]]$sim[, 11],
                                  mod_sum_males_good[[11]]$sim[, 11],
                                  mod_sum_males_poor[[11]]$sim[, 11],
                                  mod_sum_females_good[[12]]$sim[, 11],
                                  mod_sum_females_poor[[12]]$sim[, 11],
                                  mod_sum_males_good[[12]]$sim[, 11],
                                  mod_sum_males_poor[[12]]$sim[, 11],
                                  mod_sum_females_good[[13]]$sim[, 11],
                                  mod_sum_females_poor[[13]]$sim[, 11],
                                  mod_sum_males_good[[13]]$sim[, 11],
                                  mod_sum_males_poor[[13]]$sim[, 11],
                                  mod_sum_females_good[[14]]$sim[, 11],
                                  mod_sum_females_poor[[14]]$sim[, 11],
                                  mod_sum_males_good[[14]]$sim[, 11],
                                  mod_sum_males_poor[[14]]$sim[, 11],
                                  mod_sum_females_good[[15]]$sim[, 11],
                                  mod_sum_females_poor[[15]]$sim[, 11],
                                  mod_sum_males_good[[15]]$sim[, 11],
                                  mod_sum_males_poor[[15]]$sim[, 11],
                                  mod_sum_females_good[[16]]$sim[, 11],
                                  mod_sum_females_poor[[16]]$sim[, 11],
                                  mod_sum_males_good[[16]]$sim[, 11],
                                  mod_sum_males_poor[[16]]$sim[, 11],
                                  mod_sum_females_good[[17]]$sim[, 11],
                                  mod_sum_females_poor[[17]]$sim[, 11],
                                  mod_sum_males_good[[17]]$sim[, 11],
                                  mod_sum_males_poor[[17]]$sim[, 11],
                                  mod_sum_females_good[[18]]$sim[, 11],
                                  mod_sum_females_poor[[18]]$sim[, 11],
                                  mod_sum_males_good[[18]]$sim[, 11],
                                  mod_sum_males_poor[[18]]$sim[, 11],
                                  mod_sum_females_good[[19]]$sim[, 11],
                                  mod_sum_females_poor[[19]]$sim[, 11],
                                  mod_sum_males_good[[19]]$sim[, 11],
                                  mod_sum_males_poor[[19]]$sim[, 11],
                                  mod_sum_females_good[[20]]$sim[, 11],
                                  mod_sum_females_poor[[20]]$sim[, 11],
                                  mod_sum_males_good[[20]]$sim[, 11],
                                  mod_sum_males_poor[[20]]$sim[, 11],
                                  mod_sum_females_good[[21]]$sim[, 11],
                                  mod_sum_females_poor[[21]]$sim[, 11],
                                  mod_sum_males_good[[21]]$sim[, 11],
                                  mod_sum_males_poor[[21]]$sim[, 11],
                                  mod_sum_females_good[[22]]$sim[, 11],
                                  mod_sum_females_poor[[22]]$sim[, 11],
                                  mod_sum_males_good[[22]]$sim[, 11],
                                  mod_sum_males_poor[[22]]$sim[, 11],
                                  mod_sum_females_good[[23]]$sim[, 11],
                                  mod_sum_females_poor[[23]]$sim[, 11],
                                  mod_sum_males_good[[23]]$sim[, 11],
                                  mod_sum_males_poor[[23]]$sim[, 11],
                                  mod_sum_females_good[[24]]$sim[, 11],
                                  mod_sum_females_poor[[24]]$sim[, 11],
                                  mod_sum_males_good[[24]]$sim[, 11],
                                  mod_sum_males_poor[[24]]$sim[, 11],
                                  mod_sum_females_good[[25]]$sim[, 11],
                                  mod_sum_females_poor[[25]]$sim[, 11],
                                  mod_sum_males_good[[25]]$sim[, 11],
                                  mod_sum_males_poor[[25]]$sim[, 11],
                                  mod_sum_females_good[[26]]$sim[, 11],
                                  mod_sum_females_poor[[26]]$sim[, 11],
                                  mod_sum_males_good[[26]]$sim[, 11],
                                  mod_sum_males_poor[[26]]$sim[, 11],
                                  mod_sum_females_good[[27]]$sim[, 11],
                                  mod_sum_females_poor[[27]]$sim[, 11],
                                  mod_sum_males_good[[27]]$sim[, 11],
                                  mod_sum_males_poor[[27]]$sim[, 11],
                                  mod_sum_females_good[[28]]$sim[, 11],
                                  mod_sum_females_poor[[28]]$sim[, 11],
                                  mod_sum_males_good[[28]]$sim[, 11],
                                  mod_sum_males_poor[[28]]$sim[, 11],
                                  mod_sum_females_good[[29]]$sim[, 11],
                                  mod_sum_females_poor[[29]]$sim[, 11],
                                  mod_sum_males_good[[29]]$sim[, 11],
                                  mod_sum_males_poor[[29]]$sim[, 11],
                                  mod_sum_females_good[[30]]$sim[, 11],
                                  mod_sum_females_poor[[30]]$sim[, 11],
                                  mod_sum_males_good[[30]]$sim[, 11],
                                  mod_sum_males_poor[[30]]$sim[, 11],
                                  mod_sum_females_good[[31]]$sim[, 11],
                                  mod_sum_females_poor[[31]]$sim[, 11],
                                  mod_sum_males_good[[31]]$sim[, 11],
                                  mod_sum_males_poor[[31]]$sim[, 11],
                                  mod_sum_females_good[[32]]$sim[, 11],
                                  mod_sum_females_poor[[32]]$sim[, 11],
                                  mod_sum_males_good[[32]]$sim[, 11],
                                  mod_sum_males_poor[[32]]$sim[, 11],
                                  mod_sum_females_good[[33]]$sim[, 11],
                                  mod_sum_females_poor[[33]]$sim[, 11],
                                  mod_sum_males_good[[33]]$sim[, 11],
                                  mod_sum_males_poor[[33]]$sim[, 11],
                                  mod_sum_females_good[[34]]$sim[, 11],
                                  mod_sum_females_poor[[34]]$sim[, 11],
                                  mod_sum_males_good[[34]]$sim[, 11],
                                  mod_sum_males_poor[[34]]$sim[, 11],
                                  mod_sum_females_good[[35]]$sim[, 11],
                                  mod_sum_females_poor[[35]]$sim[, 11],
                                  mod_sum_males_good[[35]]$sim[, 11],
                                  mod_sum_males_poor[[35]]$sim[, 11],
                                  mod_sum_females_good[[36]]$sim[, 11],
                                  mod_sum_females_poor[[36]]$sim[, 11],
                                  mod_sum_males_good[[36]]$sim[, 11],
                                  mod_sum_males_poor[[36]]$sim[, 11],
                                  mod_sum_females_good[[37]]$sim[, 11],
                                  mod_sum_females_poor[[37]]$sim[, 11],
                                  mod_sum_males_good[[37]]$sim[, 11],
                                  mod_sum_males_poor[[37]]$sim[, 11],
                                  mod_sum_females_good[[38]]$sim[, 11],
                                  mod_sum_females_poor[[38]]$sim[, 11],
                                  mod_sum_males_good[[38]]$sim[, 11],
                                  mod_sum_males_poor[[38]]$sim[, 11],
                                  mod_sum_females_good[[39]]$sim[, 11],
                                  mod_sum_females_poor[[39]]$sim[, 11],
                                  mod_sum_males_good[[39]]$sim[, 11],
                                  mod_sum_males_poor[[39]]$sim[, 11],
                                  mod_sum_females_good[[40]]$sim[, 11],
                                  mod_sum_females_poor[[40]]$sim[, 11],
                                  mod_sum_males_good[[40]]$sim[, 11],
                                  mod_sum_males_poor[[40]]$sim[, 11],
                                  mod_sum_females_good[[41]]$sim[, 11],
                                  mod_sum_females_poor[[41]]$sim[, 11],
                                  mod_sum_males_good[[41]]$sim[, 11],
                                  mod_sum_males_poor[[41]]$sim[, 11]))

# Data frame with dementia estimates
dem.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                       pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                       year = rep(seq(65, 105), each = 400),
                       type = "Dementia",
                       value = c(mod_sum_females_good[[1]]$sim[, 12],
                                 mod_sum_females_poor[[1]]$sim[, 12],
                                 mod_sum_males_good[[1]]$sim[, 12],
                                 mod_sum_males_poor[[1]]$sim[, 12],
                                 mod_sum_females_good[[2]]$sim[, 12],
                                 mod_sum_females_poor[[2]]$sim[, 12],
                                 mod_sum_males_good[[2]]$sim[, 12],
                                 mod_sum_males_poor[[2]]$sim[, 12],
                                 mod_sum_females_good[[3]]$sim[, 12],
                                 mod_sum_females_poor[[3]]$sim[, 12],
                                 mod_sum_males_good[[3]]$sim[, 12],
                                 mod_sum_males_poor[[3]]$sim[, 12],
                                 mod_sum_females_good[[4]]$sim[, 12],
                                 mod_sum_females_poor[[4]]$sim[, 12],
                                 mod_sum_males_good[[4]]$sim[, 12],
                                 mod_sum_males_poor[[4]]$sim[, 12],
                                 mod_sum_females_good[[5]]$sim[, 12],
                                 mod_sum_females_poor[[5]]$sim[, 12],
                                 mod_sum_males_good[[5]]$sim[, 12],
                                 mod_sum_males_poor[[5]]$sim[, 12],
                                 mod_sum_females_good[[6]]$sim[, 12],
                                 mod_sum_females_poor[[6]]$sim[, 12],
                                 mod_sum_males_good[[6]]$sim[, 12],
                                 mod_sum_males_poor[[6]]$sim[, 12],
                                 mod_sum_females_good[[7]]$sim[, 12],
                                 mod_sum_females_poor[[7]]$sim[, 12],
                                 mod_sum_males_good[[7]]$sim[, 12],
                                 mod_sum_males_poor[[7]]$sim[, 12],
                                 mod_sum_females_good[[8]]$sim[, 12],
                                 mod_sum_females_poor[[8]]$sim[, 12],
                                 mod_sum_males_good[[8]]$sim[, 12],
                                 mod_sum_males_poor[[8]]$sim[, 12],
                                 mod_sum_females_good[[9]]$sim[, 12],
                                 mod_sum_females_poor[[9]]$sim[, 12],
                                 mod_sum_males_good[[9]]$sim[, 12],
                                 mod_sum_males_poor[[9]]$sim[, 12],
                                 mod_sum_females_good[[10]]$sim[, 12],
                                 mod_sum_females_poor[[10]]$sim[, 12],
                                 mod_sum_males_good[[10]]$sim[, 12],
                                 mod_sum_males_poor[[10]]$sim[, 12],
                                 mod_sum_females_good[[11]]$sim[, 12],
                                 mod_sum_females_poor[[11]]$sim[, 12],
                                 mod_sum_males_good[[11]]$sim[, 12],
                                 mod_sum_males_poor[[11]]$sim[, 12],
                                 mod_sum_females_good[[12]]$sim[, 12],
                                 mod_sum_females_poor[[12]]$sim[, 12],
                                 mod_sum_males_good[[12]]$sim[, 12],
                                 mod_sum_males_poor[[12]]$sim[, 12],
                                 mod_sum_females_good[[13]]$sim[, 12],
                                 mod_sum_females_poor[[13]]$sim[, 12],
                                 mod_sum_males_good[[13]]$sim[, 12],
                                 mod_sum_males_poor[[13]]$sim[, 12],
                                 mod_sum_females_good[[14]]$sim[, 12],
                                 mod_sum_females_poor[[14]]$sim[, 12],
                                 mod_sum_males_good[[14]]$sim[, 12],
                                 mod_sum_males_poor[[14]]$sim[, 12],
                                 mod_sum_females_good[[15]]$sim[, 12],
                                 mod_sum_females_poor[[15]]$sim[, 12],
                                 mod_sum_males_good[[15]]$sim[, 12],
                                 mod_sum_males_poor[[15]]$sim[, 12],
                                 mod_sum_females_good[[16]]$sim[, 12],
                                 mod_sum_females_poor[[16]]$sim[, 12],
                                 mod_sum_males_good[[16]]$sim[, 12],
                                 mod_sum_males_poor[[16]]$sim[, 12],
                                 mod_sum_females_good[[17]]$sim[, 12],
                                 mod_sum_females_poor[[17]]$sim[, 12],
                                 mod_sum_males_good[[17]]$sim[, 12],
                                 mod_sum_males_poor[[17]]$sim[, 12],
                                 mod_sum_females_good[[18]]$sim[, 12],
                                 mod_sum_females_poor[[18]]$sim[, 12],
                                 mod_sum_males_good[[18]]$sim[, 12],
                                 mod_sum_males_poor[[18]]$sim[, 12],
                                 mod_sum_females_good[[19]]$sim[, 12],
                                 mod_sum_females_poor[[19]]$sim[, 12],
                                 mod_sum_males_good[[19]]$sim[, 12],
                                 mod_sum_males_poor[[19]]$sim[, 12],
                                 mod_sum_females_good[[20]]$sim[, 12],
                                 mod_sum_females_poor[[20]]$sim[, 12],
                                 mod_sum_males_good[[20]]$sim[, 12],
                                 mod_sum_males_poor[[20]]$sim[, 12],
                                 mod_sum_females_good[[21]]$sim[, 12],
                                 mod_sum_females_poor[[21]]$sim[, 12],
                                 mod_sum_males_good[[21]]$sim[, 12],
                                 mod_sum_males_poor[[21]]$sim[, 12],
                                 mod_sum_females_good[[22]]$sim[, 12],
                                 mod_sum_females_poor[[22]]$sim[, 12],
                                 mod_sum_males_good[[22]]$sim[, 12],
                                 mod_sum_males_poor[[22]]$sim[, 12],
                                 mod_sum_females_good[[23]]$sim[, 12],
                                 mod_sum_females_poor[[23]]$sim[, 12],
                                 mod_sum_males_good[[23]]$sim[, 12],
                                 mod_sum_males_poor[[23]]$sim[, 12],
                                 mod_sum_females_good[[24]]$sim[, 12],
                                 mod_sum_females_poor[[24]]$sim[, 12],
                                 mod_sum_males_good[[24]]$sim[, 12],
                                 mod_sum_males_poor[[24]]$sim[, 12],
                                 mod_sum_females_good[[25]]$sim[, 12],
                                 mod_sum_females_poor[[25]]$sim[, 12],
                                 mod_sum_males_good[[25]]$sim[, 12],
                                 mod_sum_males_poor[[25]]$sim[, 12],
                                 mod_sum_females_good[[26]]$sim[, 12],
                                 mod_sum_females_poor[[26]]$sim[, 12],
                                 mod_sum_males_good[[26]]$sim[, 12],
                                 mod_sum_males_poor[[26]]$sim[, 12],
                                 mod_sum_females_good[[27]]$sim[, 12],
                                 mod_sum_females_poor[[27]]$sim[, 12],
                                 mod_sum_males_good[[27]]$sim[, 12],
                                 mod_sum_males_poor[[27]]$sim[, 12],
                                 mod_sum_females_good[[28]]$sim[, 12],
                                 mod_sum_females_poor[[28]]$sim[, 12],
                                 mod_sum_males_good[[28]]$sim[, 12],
                                 mod_sum_males_poor[[28]]$sim[, 12],
                                 mod_sum_females_good[[29]]$sim[, 12],
                                 mod_sum_females_poor[[29]]$sim[, 12],
                                 mod_sum_males_good[[29]]$sim[, 12],
                                 mod_sum_males_poor[[29]]$sim[, 12],
                                 mod_sum_females_good[[30]]$sim[, 12],
                                 mod_sum_females_poor[[30]]$sim[, 12],
                                 mod_sum_males_good[[30]]$sim[, 12],
                                 mod_sum_males_poor[[30]]$sim[, 12],
                                 mod_sum_females_good[[31]]$sim[, 12],
                                 mod_sum_females_poor[[31]]$sim[, 12],
                                 mod_sum_males_good[[31]]$sim[, 12],
                                 mod_sum_males_poor[[31]]$sim[, 12],
                                 mod_sum_females_good[[32]]$sim[, 12],
                                 mod_sum_females_poor[[32]]$sim[, 12],
                                 mod_sum_males_good[[32]]$sim[, 12],
                                 mod_sum_males_poor[[32]]$sim[, 12],
                                 mod_sum_females_good[[33]]$sim[, 12],
                                 mod_sum_females_poor[[33]]$sim[, 12],
                                 mod_sum_males_good[[33]]$sim[, 12],
                                 mod_sum_males_poor[[33]]$sim[, 12],
                                 mod_sum_females_good[[34]]$sim[, 12],
                                 mod_sum_females_poor[[34]]$sim[, 12],
                                 mod_sum_males_good[[34]]$sim[, 12],
                                 mod_sum_males_poor[[34]]$sim[, 12],
                                 mod_sum_females_good[[35]]$sim[, 12],
                                 mod_sum_females_poor[[35]]$sim[, 12],
                                 mod_sum_males_good[[35]]$sim[, 12],
                                 mod_sum_males_poor[[35]]$sim[, 12],
                                 mod_sum_females_good[[36]]$sim[, 12],
                                 mod_sum_females_poor[[36]]$sim[, 12],
                                 mod_sum_males_good[[36]]$sim[, 12],
                                 mod_sum_males_poor[[36]]$sim[, 12],
                                 mod_sum_females_good[[37]]$sim[, 12],
                                 mod_sum_females_poor[[37]]$sim[, 12],
                                 mod_sum_males_good[[37]]$sim[, 12],
                                 mod_sum_males_poor[[37]]$sim[, 12],
                                 mod_sum_females_good[[38]]$sim[, 12],
                                 mod_sum_females_poor[[38]]$sim[, 12],
                                 mod_sum_males_good[[38]]$sim[, 12],
                                 mod_sum_males_poor[[38]]$sim[, 12],
                                 mod_sum_females_good[[39]]$sim[, 12],
                                 mod_sum_females_poor[[39]]$sim[, 12],
                                 mod_sum_males_good[[39]]$sim[, 12],
                                 mod_sum_males_poor[[39]]$sim[, 12],
                                 mod_sum_females_good[[40]]$sim[, 12],
                                 mod_sum_females_poor[[40]]$sim[, 12],
                                 mod_sum_males_good[[40]]$sim[, 12],
                                 mod_sum_males_poor[[40]]$sim[, 12],
                                 mod_sum_females_good[[41]]$sim[, 12],
                                 mod_sum_females_poor[[41]]$sim[, 12],
                                 mod_sum_males_good[[41]]$sim[, 12],
                                 mod_sum_males_poor[[41]]$sim[, 12]))


# Combining datasets
comp.data <- free.data %>%
  rbind(mci.data, dem.data)

# plot!
tiff("ov_grey.tiff", height = 6, width = 7, res = 300, units = "in")
comp.data %>%
  group_by(sex, pp, year, type) %>%
  summarise(value = mean(value)) %>%
  mutate(type = factor(type,
                       levels = c("Dementia",
                                  "MCI",
                                  "Dementia-free"))) %>%
  ggplot(aes(fill = type, x = year, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(pp ~ sex) +
  labs(x = "Age (years)",
       y = "Estimated Life Expectancy (years)",
       fill = "") +
  theme_base() +
  scale_x_continuous(breaks = seq(65, 105, 10)) +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  theme(legend.position = "bottom")+
  scale_fill_brewer(breaks = c("Dementia-free",
                             "MCI",
                             "Dementia"),
                    palette = "Greys")
dev.off()

# Numerical data corresponding to total LE and marginal LEs
table <- comp.data %>%
  group_by(sex, pp, year, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(type, value) %>%
  select(sex, pp, year, `Dementia-free`, MCI, Dementia) %>%
  mutate(prop.free = (`Dementia-free` / (`Dementia-free` + MCI + Dementia)) * 100,
         prop.dem = (Dementia / (`Dementia-free` + MCI + Dementia)) * 100)

write.table(table, file = "total.txt", 
            sep = ",", quote = FALSE, row.names = F)



### SENSITIVITY ANALYSIS

# upload the dataset
saveRDS(prueba2, "sens_data.Rda")
prueba2 <- readRDS("sens_data.Rda")

# create the variable sppb.sens, indicating a good physical functioning if men reached 8 points in the SPPPB, and women 7
prueba2 <- prueba2 %>%
  mutate(sppb.sens = case_when(
    sppb > 8 & sex == 1 ~ 1,
    sppb <= 8  & sex == 1 ~ 0,
    sppb > 7 & sex == 2 ~ 1,
    sppb <= 7 & sex == 2 ~ 0
  ))


# Fitting the model into msm
model.elect2.sens <- msm(state ~ age, 
                    subject = id, 
                    data = prueba2,
                    covariates = ~ age + sppb.sens +
                      sex,
                    qmatrix = Q, 
                    deathexact = 4, 
                    control = list(fnscale = 10000,
                                   maxit = 1000,
                                   trace = 1,
                                   REPORT = 1,
                                   reltol = 1e-8),
                    center = FALSE,
                    death = TRUE)

model.elect.age <- msm(state ~ age, 
                       subject = id, 
                       data = prueba2,
                       covariates = ~ age,
                       qmatrix = Q, 
                       deathexact = 4, 
                       control = list(fnscale = 10000,
                                      maxit = 1000,
                                      trace = 1,
                                      REPORT = 1,
                                      reltol = 1e-16),
                       center = FALSE,
                       death = TRUE)

# Model fit
AIC(model.elect2.sens)

# save the model
saveRDS(model.elect2.sens, file = "sens_model.Rda")
model.elect2.sens <- readRDS(file = "sens_model.Rda")

# Defining the data used for the distribution of the living states conditional on a specified age (time interval since baseline)
# This distribution is needed to compute marginal life expectancies
sddata <- prueba2[prueba2$state %in% c(1, 2, 3),]


# It is up the user how to choose this data
# Here we specify age = 0 (older adults with the mean age at baseline ~ mean(plm_exam %>% filter(wave == 1) %>% .$age, na.rm = TRUE)) and sppb_binary = 0, poor physical performance

### Defining the parameters for the LEs
age <- 75 - mean.age
age.max <- 110 - mean.age
sex <- 0
sppb10 <- 1
smk <- 0
exercise <- 1

# LE estimates
LEs <- elect(model.elect2.sens,
             b.covariates = list(age = age,
                                 sppb.sens = 1,
                                 sex = 1),
             statedistdata = sddata,
             h = 0.5,
             time.scale.msm = "years",
             age.max = age.max,
             S = 100,
             setseed = 2012024)
summary.elect(LEs)

LEs1 <- elect(model.elect2,
              b.covariates = list(age = age,
                                  sppb101 = 1,
                                  sex = 1),
              statedistdata = sddata,
              h = 0.5,
              time.scale.msm = "years",
              age.max = age.max,
              S = 100,
              setseed = 2012024)
summary.elect(LEs1)
plot.elect(LEs1)


### mean and 95% CI from the differences between predicted values of the 500 simulations
library(tidybayes)
mean_qi(LEs$sim[, 1] - LEs1$sim[, 1])
mean_qi(data.1$`LEs$sim[, 13]` - data.2$`LEs1$sim[, 13]`[1:430])


# LE estimates for a range of age
mod_summaries <- list()

for (i in seq(-14, 22, 0.5)) {
  mod_summaries[[i+15]] <- elect(model.elect.age, 
                                 b.covariates = list(age = i),
                                 statedistdata = sddata, 
                                 time.scale.msm = "years",
                                 h = 0.5, 
                                 age.max = 32, 
                                 S = 50,
                                 setseed = 2012024)
}

### organise data for plot
median_qi(mod_summaries[[1]]$sim[, 13]) %>% 
  rbind(mean_qi(mod_summaries[[2]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[3]]$sim[, 13])) %>% 
  rbind(mean_qi(mod_summaries[[4]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[5]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[6]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[7]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[8]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[9]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[10]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[11]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[12]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[13]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[14]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[15]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[16]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[17]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[18]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[19]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[20]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[21]]$sim[, 13])) %>%
  rbind(mean_qi(mod_summaries[[22]]$sim[, 13])) %>%
  cbind(age = seq(-14 + mean.age, -3.5 + mean.age, 0.5)) %>%
  ggplot(aes(x = age, y = y)) +
  geom_pointinterval(aes(ymin = ymin,
                         ymax = ymax))



# Life expectancy estimates for older adults with poor physical performance
mod_summaries <- list()

for (i in seq(-15.55227, 24.44773, 1)) {
  mod_summaries[[i+16.55227]] <- elect(model.elect2, 
                                       b.covariates = list(age = i,
                                                           sppb10 = 0,
                                                           sex = 1),
                                       statedistdata = sddata, 
                                       time.scale.msm = "years",
                                       h = 0.5, 
                                       age.max = age.max, 
                                       S = 100,
                                       setseed = 2012024)
}

# Life expectancy estimates for older adults with poor physical performance
mod_summaries_poor <- list()

for (i in seq(-15.55227, 24.44773, 1)) {
  mod_summaries_poor[[i+16.55227]] <- elect(model.elect2, 
                                            b.covariates = list(age = i,
                                                                sppb10 = 0,
                                                                sex = 2),
                                            statedistdata = sddata, 
                                            time.scale.msm = "years",
                                            h = 0.5, 
                                            age.max = age.max, 
                                            S = 100,
                                            setseed = 2012024)
}

mod_sum_males_poor <- mod_summaries
mod_sum_females_poor <- mod_summaries_poor
saveRDS(mod_sum_males_poor, file = "LE_males_poor.Rda")
saveRDS(mod_sum_females_poor, file = "LE_females_poor.Rda")

mod_summaries <- readRDS("LE_males_poor.Rda")
mod_summaries_poor <- readRDS("LE_females_poor.Rda")

length(unique(prueba2$id))
prueba2 %>% filter(sex == 1) %>% nrow()
prueba2 %>% filter(state == 1) %>% filter(!duplicated(id)) %>% nrow()
prueba2 %>% filter(sex == 2) %>% filter(!duplicated(id)) %>% nrow()

statetable.msm(demclas, subject = id, plm_exam)
summary.elect(mod_sum_males_poor[[21]])

mod_sum_females_good[[1]]$sim[, 10] # marginal dementia-free LE
mod_sum_females_good[[1]]$sim[, 11] # marginal MCI LE
mod_sum_females_good[[1]]$sim[, 12] # marginal dementia LE
mod_sum_females_good[[1]]$sim[, 13] # total LE


# Data frame with dementia-free estimates
free.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                        pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                        year = rep(seq(65, 105), each = 400),
                        type = "Dementia-free",
                        value = c(mod_sum_females_good[[1]]$sim[, 10],
                                  mod_sum_females_poor[[1]]$sim[, 10],
                                  mod_sum_males_good[[1]]$sim[, 10],
                                  mod_sum_males_poor[[1]]$sim[, 10],
                                  mod_sum_females_good[[2]]$sim[, 10],
                                  mod_sum_females_poor[[2]]$sim[, 10],
                                  mod_sum_males_good[[2]]$sim[, 10],
                                  mod_sum_males_poor[[2]]$sim[, 10],
                                  mod_sum_females_good[[3]]$sim[, 10],
                                  mod_sum_females_poor[[3]]$sim[, 10],
                                  mod_sum_males_good[[3]]$sim[, 10],
                                  mod_sum_males_poor[[3]]$sim[, 10],
                                  mod_sum_females_good[[4]]$sim[, 10],
                                  mod_sum_females_poor[[4]]$sim[, 10],
                                  mod_sum_males_good[[4]]$sim[, 10],
                                  mod_sum_males_poor[[4]]$sim[, 10],
                                  mod_sum_females_good[[5]]$sim[, 10],
                                  mod_sum_females_poor[[5]]$sim[, 10],
                                  mod_sum_males_good[[5]]$sim[, 10],
                                  mod_sum_males_poor[[5]]$sim[, 10],
                                  mod_sum_females_good[[6]]$sim[, 10],
                                  mod_sum_females_poor[[6]]$sim[, 10],
                                  mod_sum_males_good[[6]]$sim[, 10],
                                  mod_sum_males_poor[[6]]$sim[, 10],
                                  mod_sum_females_good[[7]]$sim[, 10],
                                  mod_sum_females_poor[[7]]$sim[, 10],
                                  mod_sum_males_good[[7]]$sim[, 10],
                                  mod_sum_males_poor[[7]]$sim[, 10],
                                  mod_sum_females_good[[8]]$sim[, 10],
                                  mod_sum_females_poor[[8]]$sim[, 10],
                                  mod_sum_males_good[[8]]$sim[, 10],
                                  mod_sum_males_poor[[8]]$sim[, 10],
                                  mod_sum_females_good[[9]]$sim[, 10],
                                  mod_sum_females_poor[[9]]$sim[, 10],
                                  mod_sum_males_good[[9]]$sim[, 10],
                                  mod_sum_males_poor[[9]]$sim[, 10],
                                  mod_sum_females_good[[10]]$sim[, 10],
                                  mod_sum_females_poor[[10]]$sim[, 10],
                                  mod_sum_males_good[[10]]$sim[, 10],
                                  mod_sum_males_poor[[10]]$sim[, 10],
                                  mod_sum_females_good[[11]]$sim[, 10],
                                  mod_sum_females_poor[[11]]$sim[, 10],
                                  mod_sum_males_good[[11]]$sim[, 10],
                                  mod_sum_males_poor[[11]]$sim[, 10],
                                  mod_sum_females_good[[12]]$sim[, 10],
                                  mod_sum_females_poor[[12]]$sim[, 10],
                                  mod_sum_males_good[[12]]$sim[, 10],
                                  mod_sum_males_poor[[12]]$sim[, 10],
                                  mod_sum_females_good[[13]]$sim[, 10],
                                  mod_sum_females_poor[[13]]$sim[, 10],
                                  mod_sum_males_good[[13]]$sim[, 10],
                                  mod_sum_males_poor[[13]]$sim[, 10],
                                  mod_sum_females_good[[14]]$sim[, 10],
                                  mod_sum_females_poor[[14]]$sim[, 10],
                                  mod_sum_males_good[[14]]$sim[, 10],
                                  mod_sum_males_poor[[14]]$sim[, 10],
                                  mod_sum_females_good[[15]]$sim[, 10],
                                  mod_sum_females_poor[[15]]$sim[, 10],
                                  mod_sum_males_good[[15]]$sim[, 10],
                                  mod_sum_males_poor[[15]]$sim[, 10],
                                  mod_sum_females_good[[16]]$sim[, 10],
                                  mod_sum_females_poor[[16]]$sim[, 10],
                                  mod_sum_males_good[[16]]$sim[, 10],
                                  mod_sum_males_poor[[16]]$sim[, 10],
                                  mod_sum_females_good[[17]]$sim[, 10],
                                  mod_sum_females_poor[[17]]$sim[, 10],
                                  mod_sum_males_good[[17]]$sim[, 10],
                                  mod_sum_males_poor[[17]]$sim[, 10],
                                  mod_sum_females_good[[18]]$sim[, 10],
                                  mod_sum_females_poor[[18]]$sim[, 10],
                                  mod_sum_males_good[[18]]$sim[, 10],
                                  mod_sum_males_poor[[18]]$sim[, 10],
                                  mod_sum_females_good[[19]]$sim[, 10],
                                  mod_sum_females_poor[[19]]$sim[, 10],
                                  mod_sum_males_good[[19]]$sim[, 10],
                                  mod_sum_males_poor[[19]]$sim[, 10],
                                  mod_sum_females_good[[20]]$sim[, 10],
                                  mod_sum_females_poor[[20]]$sim[, 10],
                                  mod_sum_males_good[[20]]$sim[, 10],
                                  mod_sum_males_poor[[20]]$sim[, 10],
                                  mod_sum_females_good[[21]]$sim[, 10],
                                  mod_sum_females_poor[[21]]$sim[, 10],
                                  mod_sum_males_good[[21]]$sim[, 10],
                                  mod_sum_males_poor[[21]]$sim[, 10],
                                  mod_sum_females_good[[22]]$sim[, 10],
                                  mod_sum_females_poor[[22]]$sim[, 10],
                                  mod_sum_males_good[[22]]$sim[, 10],
                                  mod_sum_males_poor[[22]]$sim[, 10],
                                  mod_sum_females_good[[23]]$sim[, 10],
                                  mod_sum_females_poor[[23]]$sim[, 10],
                                  mod_sum_males_good[[23]]$sim[, 10],
                                  mod_sum_males_poor[[23]]$sim[, 10],
                                  mod_sum_females_good[[24]]$sim[, 10],
                                  mod_sum_females_poor[[24]]$sim[, 10],
                                  mod_sum_males_good[[24]]$sim[, 10],
                                  mod_sum_males_poor[[24]]$sim[, 10],
                                  mod_sum_females_good[[25]]$sim[, 10],
                                  mod_sum_females_poor[[25]]$sim[, 10],
                                  mod_sum_males_good[[25]]$sim[, 10],
                                  mod_sum_males_poor[[25]]$sim[, 10],
                                  mod_sum_females_good[[26]]$sim[, 10],
                                  mod_sum_females_poor[[26]]$sim[, 10],
                                  mod_sum_males_good[[26]]$sim[, 10],
                                  mod_sum_males_poor[[26]]$sim[, 10],
                                  mod_sum_females_good[[27]]$sim[, 10],
                                  mod_sum_females_poor[[27]]$sim[, 10],
                                  mod_sum_males_good[[27]]$sim[, 10],
                                  mod_sum_males_poor[[27]]$sim[, 10],
                                  mod_sum_females_good[[28]]$sim[, 10],
                                  mod_sum_females_poor[[28]]$sim[, 10],
                                  mod_sum_males_good[[28]]$sim[, 10],
                                  mod_sum_males_poor[[28]]$sim[, 10],
                                  mod_sum_females_good[[29]]$sim[, 10],
                                  mod_sum_females_poor[[29]]$sim[, 10],
                                  mod_sum_males_good[[29]]$sim[, 10],
                                  mod_sum_males_poor[[29]]$sim[, 10],
                                  mod_sum_females_good[[30]]$sim[, 10],
                                  mod_sum_females_poor[[30]]$sim[, 10],
                                  mod_sum_males_good[[30]]$sim[, 10],
                                  mod_sum_males_poor[[30]]$sim[, 10],
                                  mod_sum_females_good[[31]]$sim[, 10],
                                  mod_sum_females_poor[[31]]$sim[, 10],
                                  mod_sum_males_good[[31]]$sim[, 10],
                                  mod_sum_males_poor[[31]]$sim[, 10],
                                  mod_sum_females_good[[32]]$sim[, 10],
                                  mod_sum_females_poor[[32]]$sim[, 10],
                                  mod_sum_males_good[[32]]$sim[, 10],
                                  mod_sum_males_poor[[32]]$sim[, 10],
                                  mod_sum_females_good[[33]]$sim[, 10],
                                  mod_sum_females_poor[[33]]$sim[, 10],
                                  mod_sum_males_good[[33]]$sim[, 10],
                                  mod_sum_males_poor[[33]]$sim[, 10],
                                  mod_sum_females_good[[34]]$sim[, 10],
                                  mod_sum_females_poor[[34]]$sim[, 10],
                                  mod_sum_males_good[[34]]$sim[, 10],
                                  mod_sum_males_poor[[34]]$sim[, 10],
                                  mod_sum_females_good[[35]]$sim[, 10],
                                  mod_sum_females_poor[[35]]$sim[, 10],
                                  mod_sum_males_good[[35]]$sim[, 10],
                                  mod_sum_males_poor[[35]]$sim[, 10],
                                  mod_sum_females_good[[36]]$sim[, 10],
                                  mod_sum_females_poor[[36]]$sim[, 10],
                                  mod_sum_males_good[[36]]$sim[, 10],
                                  mod_sum_males_poor[[36]]$sim[, 10],
                                  mod_sum_females_good[[37]]$sim[, 10],
                                  mod_sum_females_poor[[37]]$sim[, 10],
                                  mod_sum_males_good[[37]]$sim[, 10],
                                  mod_sum_males_poor[[37]]$sim[, 10],
                                  mod_sum_females_good[[38]]$sim[, 10],
                                  mod_sum_females_poor[[38]]$sim[, 10],
                                  mod_sum_males_good[[38]]$sim[, 10],
                                  mod_sum_males_poor[[38]]$sim[, 10],
                                  mod_sum_females_good[[39]]$sim[, 10],
                                  mod_sum_females_poor[[39]]$sim[, 10],
                                  mod_sum_males_good[[39]]$sim[, 10],
                                  mod_sum_males_poor[[39]]$sim[, 10],
                                  mod_sum_females_good[[40]]$sim[, 10],
                                  mod_sum_females_poor[[40]]$sim[, 10],
                                  mod_sum_males_good[[40]]$sim[, 10],
                                  mod_sum_males_poor[[40]]$sim[, 10],
                                  mod_sum_females_good[[41]]$sim[, 10],
                                  mod_sum_females_poor[[41]]$sim[, 10],
                                  mod_sum_males_good[[41]]$sim[, 10],
                                  mod_sum_males_poor[[41]]$sim[, 10]))


# Data frame with MCI estimates
mci.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                       pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                       year = rep(seq(65, 105), each = 400),
                       type = "MCI",
                       value = c(mod_sum_females_good[[1]]$sim[, 11],
                                 mod_sum_females_poor[[1]]$sim[, 11],
                                 mod_sum_males_good[[1]]$sim[, 11],
                                 mod_sum_males_poor[[1]]$sim[, 11],
                                 mod_sum_females_good[[2]]$sim[, 11],
                                 mod_sum_females_poor[[2]]$sim[, 11],
                                 mod_sum_males_good[[2]]$sim[, 11],
                                 mod_sum_males_poor[[2]]$sim[, 11],
                                 mod_sum_females_good[[3]]$sim[, 11],
                                 mod_sum_females_poor[[3]]$sim[, 11],
                                 mod_sum_males_good[[3]]$sim[, 11],
                                 mod_sum_males_poor[[3]]$sim[, 11],
                                 mod_sum_females_good[[4]]$sim[, 11],
                                 mod_sum_females_poor[[4]]$sim[, 11],
                                 mod_sum_males_good[[4]]$sim[, 11],
                                 mod_sum_males_poor[[4]]$sim[, 11],
                                 mod_sum_females_good[[5]]$sim[, 11],
                                 mod_sum_females_poor[[5]]$sim[, 11],
                                 mod_sum_males_good[[5]]$sim[, 11],
                                 mod_sum_males_poor[[5]]$sim[, 11],
                                 mod_sum_females_good[[6]]$sim[, 11],
                                 mod_sum_females_poor[[6]]$sim[, 11],
                                 mod_sum_males_good[[6]]$sim[, 11],
                                 mod_sum_males_poor[[6]]$sim[, 11],
                                 mod_sum_females_good[[7]]$sim[, 11],
                                 mod_sum_females_poor[[7]]$sim[, 11],
                                 mod_sum_males_good[[7]]$sim[, 11],
                                 mod_sum_males_poor[[7]]$sim[, 11],
                                 mod_sum_females_good[[8]]$sim[, 11],
                                 mod_sum_females_poor[[8]]$sim[, 11],
                                 mod_sum_males_good[[8]]$sim[, 11],
                                 mod_sum_males_poor[[8]]$sim[, 11],
                                 mod_sum_females_good[[9]]$sim[, 11],
                                 mod_sum_females_poor[[9]]$sim[, 11],
                                 mod_sum_males_good[[9]]$sim[, 11],
                                 mod_sum_males_poor[[9]]$sim[, 11],
                                 mod_sum_females_good[[10]]$sim[, 11],
                                 mod_sum_females_poor[[10]]$sim[, 11],
                                 mod_sum_males_good[[10]]$sim[, 11],
                                 mod_sum_males_poor[[10]]$sim[, 11],
                                 mod_sum_females_good[[11]]$sim[, 11],
                                 mod_sum_females_poor[[11]]$sim[, 11],
                                 mod_sum_males_good[[11]]$sim[, 11],
                                 mod_sum_males_poor[[11]]$sim[, 11],
                                 mod_sum_females_good[[12]]$sim[, 11],
                                 mod_sum_females_poor[[12]]$sim[, 11],
                                 mod_sum_males_good[[12]]$sim[, 11],
                                 mod_sum_males_poor[[12]]$sim[, 11],
                                 mod_sum_females_good[[13]]$sim[, 11],
                                 mod_sum_females_poor[[13]]$sim[, 11],
                                 mod_sum_males_good[[13]]$sim[, 11],
                                 mod_sum_males_poor[[13]]$sim[, 11],
                                 mod_sum_females_good[[14]]$sim[, 11],
                                 mod_sum_females_poor[[14]]$sim[, 11],
                                 mod_sum_males_good[[14]]$sim[, 11],
                                 mod_sum_males_poor[[14]]$sim[, 11],
                                 mod_sum_females_good[[15]]$sim[, 11],
                                 mod_sum_females_poor[[15]]$sim[, 11],
                                 mod_sum_males_good[[15]]$sim[, 11],
                                 mod_sum_males_poor[[15]]$sim[, 11],
                                 mod_sum_females_good[[16]]$sim[, 11],
                                 mod_sum_females_poor[[16]]$sim[, 11],
                                 mod_sum_males_good[[16]]$sim[, 11],
                                 mod_sum_males_poor[[16]]$sim[, 11],
                                 mod_sum_females_good[[17]]$sim[, 11],
                                 mod_sum_females_poor[[17]]$sim[, 11],
                                 mod_sum_males_good[[17]]$sim[, 11],
                                 mod_sum_males_poor[[17]]$sim[, 11],
                                 mod_sum_females_good[[18]]$sim[, 11],
                                 mod_sum_females_poor[[18]]$sim[, 11],
                                 mod_sum_males_good[[18]]$sim[, 11],
                                 mod_sum_males_poor[[18]]$sim[, 11],
                                 mod_sum_females_good[[19]]$sim[, 11],
                                 mod_sum_females_poor[[19]]$sim[, 11],
                                 mod_sum_males_good[[19]]$sim[, 11],
                                 mod_sum_males_poor[[19]]$sim[, 11],
                                 mod_sum_females_good[[20]]$sim[, 11],
                                 mod_sum_females_poor[[20]]$sim[, 11],
                                 mod_sum_males_good[[20]]$sim[, 11],
                                 mod_sum_males_poor[[20]]$sim[, 11],
                                 mod_sum_females_good[[21]]$sim[, 11],
                                 mod_sum_females_poor[[21]]$sim[, 11],
                                 mod_sum_males_good[[21]]$sim[, 11],
                                 mod_sum_males_poor[[21]]$sim[, 11],
                                 mod_sum_females_good[[22]]$sim[, 11],
                                 mod_sum_females_poor[[22]]$sim[, 11],
                                 mod_sum_males_good[[22]]$sim[, 11],
                                 mod_sum_males_poor[[22]]$sim[, 11],
                                 mod_sum_females_good[[23]]$sim[, 11],
                                 mod_sum_females_poor[[23]]$sim[, 11],
                                 mod_sum_males_good[[23]]$sim[, 11],
                                 mod_sum_males_poor[[23]]$sim[, 11],
                                 mod_sum_females_good[[24]]$sim[, 11],
                                 mod_sum_females_poor[[24]]$sim[, 11],
                                 mod_sum_males_good[[24]]$sim[, 11],
                                 mod_sum_males_poor[[24]]$sim[, 11],
                                 mod_sum_females_good[[25]]$sim[, 11],
                                 mod_sum_females_poor[[25]]$sim[, 11],
                                 mod_sum_males_good[[25]]$sim[, 11],
                                 mod_sum_males_poor[[25]]$sim[, 11],
                                 mod_sum_females_good[[26]]$sim[, 11],
                                 mod_sum_females_poor[[26]]$sim[, 11],
                                 mod_sum_males_good[[26]]$sim[, 11],
                                 mod_sum_males_poor[[26]]$sim[, 11],
                                 mod_sum_females_good[[27]]$sim[, 11],
                                 mod_sum_females_poor[[27]]$sim[, 11],
                                 mod_sum_males_good[[27]]$sim[, 11],
                                 mod_sum_males_poor[[27]]$sim[, 11],
                                 mod_sum_females_good[[28]]$sim[, 11],
                                 mod_sum_females_poor[[28]]$sim[, 11],
                                 mod_sum_males_good[[28]]$sim[, 11],
                                 mod_sum_males_poor[[28]]$sim[, 11],
                                 mod_sum_females_good[[29]]$sim[, 11],
                                 mod_sum_females_poor[[29]]$sim[, 11],
                                 mod_sum_males_good[[29]]$sim[, 11],
                                 mod_sum_males_poor[[29]]$sim[, 11],
                                 mod_sum_females_good[[30]]$sim[, 11],
                                 mod_sum_females_poor[[30]]$sim[, 11],
                                 mod_sum_males_good[[30]]$sim[, 11],
                                 mod_sum_males_poor[[30]]$sim[, 11],
                                 mod_sum_females_good[[31]]$sim[, 11],
                                 mod_sum_females_poor[[31]]$sim[, 11],
                                 mod_sum_males_good[[31]]$sim[, 11],
                                 mod_sum_males_poor[[31]]$sim[, 11],
                                 mod_sum_females_good[[32]]$sim[, 11],
                                 mod_sum_females_poor[[32]]$sim[, 11],
                                 mod_sum_males_good[[32]]$sim[, 11],
                                 mod_sum_males_poor[[32]]$sim[, 11],
                                 mod_sum_females_good[[33]]$sim[, 11],
                                 mod_sum_females_poor[[33]]$sim[, 11],
                                 mod_sum_males_good[[33]]$sim[, 11],
                                 mod_sum_males_poor[[33]]$sim[, 11],
                                 mod_sum_females_good[[34]]$sim[, 11],
                                 mod_sum_females_poor[[34]]$sim[, 11],
                                 mod_sum_males_good[[34]]$sim[, 11],
                                 mod_sum_males_poor[[34]]$sim[, 11],
                                 mod_sum_females_good[[35]]$sim[, 11],
                                 mod_sum_females_poor[[35]]$sim[, 11],
                                 mod_sum_males_good[[35]]$sim[, 11],
                                 mod_sum_males_poor[[35]]$sim[, 11],
                                 mod_sum_females_good[[36]]$sim[, 11],
                                 mod_sum_females_poor[[36]]$sim[, 11],
                                 mod_sum_males_good[[36]]$sim[, 11],
                                 mod_sum_males_poor[[36]]$sim[, 11],
                                 mod_sum_females_good[[37]]$sim[, 11],
                                 mod_sum_females_poor[[37]]$sim[, 11],
                                 mod_sum_males_good[[37]]$sim[, 11],
                                 mod_sum_males_poor[[37]]$sim[, 11],
                                 mod_sum_females_good[[38]]$sim[, 11],
                                 mod_sum_females_poor[[38]]$sim[, 11],
                                 mod_sum_males_good[[38]]$sim[, 11],
                                 mod_sum_males_poor[[38]]$sim[, 11],
                                 mod_sum_females_good[[39]]$sim[, 11],
                                 mod_sum_females_poor[[39]]$sim[, 11],
                                 mod_sum_males_good[[39]]$sim[, 11],
                                 mod_sum_males_poor[[39]]$sim[, 11],
                                 mod_sum_females_good[[40]]$sim[, 11],
                                 mod_sum_females_poor[[40]]$sim[, 11],
                                 mod_sum_males_good[[40]]$sim[, 11],
                                 mod_sum_males_poor[[40]]$sim[, 11],
                                 mod_sum_females_good[[41]]$sim[, 11],
                                 mod_sum_females_poor[[41]]$sim[, 11],
                                 mod_sum_males_good[[41]]$sim[, 11],
                                 mod_sum_males_poor[[41]]$sim[, 11]))

# Data frame with dementia estimates
dem.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                       pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                       year = rep(seq(65, 105), each = 400),
                       type = "Dementia",
                       value = c(mod_sum_females_good[[1]]$sim[, 12],
                                 mod_sum_females_poor[[1]]$sim[, 12],
                                 mod_sum_males_good[[1]]$sim[, 12],
                                 mod_sum_males_poor[[1]]$sim[, 12],
                                 mod_sum_females_good[[2]]$sim[, 12],
                                 mod_sum_females_poor[[2]]$sim[, 12],
                                 mod_sum_males_good[[2]]$sim[, 12],
                                 mod_sum_males_poor[[2]]$sim[, 12],
                                 mod_sum_females_good[[3]]$sim[, 12],
                                 mod_sum_females_poor[[3]]$sim[, 12],
                                 mod_sum_males_good[[3]]$sim[, 12],
                                 mod_sum_males_poor[[3]]$sim[, 12],
                                 mod_sum_females_good[[4]]$sim[, 12],
                                 mod_sum_females_poor[[4]]$sim[, 12],
                                 mod_sum_males_good[[4]]$sim[, 12],
                                 mod_sum_males_poor[[4]]$sim[, 12],
                                 mod_sum_females_good[[5]]$sim[, 12],
                                 mod_sum_females_poor[[5]]$sim[, 12],
                                 mod_sum_males_good[[5]]$sim[, 12],
                                 mod_sum_males_poor[[5]]$sim[, 12],
                                 mod_sum_females_good[[6]]$sim[, 12],
                                 mod_sum_females_poor[[6]]$sim[, 12],
                                 mod_sum_males_good[[6]]$sim[, 12],
                                 mod_sum_males_poor[[6]]$sim[, 12],
                                 mod_sum_females_good[[7]]$sim[, 12],
                                 mod_sum_females_poor[[7]]$sim[, 12],
                                 mod_sum_males_good[[7]]$sim[, 12],
                                 mod_sum_males_poor[[7]]$sim[, 12],
                                 mod_sum_females_good[[8]]$sim[, 12],
                                 mod_sum_females_poor[[8]]$sim[, 12],
                                 mod_sum_males_good[[8]]$sim[, 12],
                                 mod_sum_males_poor[[8]]$sim[, 12],
                                 mod_sum_females_good[[9]]$sim[, 12],
                                 mod_sum_females_poor[[9]]$sim[, 12],
                                 mod_sum_males_good[[9]]$sim[, 12],
                                 mod_sum_males_poor[[9]]$sim[, 12],
                                 mod_sum_females_good[[10]]$sim[, 12],
                                 mod_sum_females_poor[[10]]$sim[, 12],
                                 mod_sum_males_good[[10]]$sim[, 12],
                                 mod_sum_males_poor[[10]]$sim[, 12],
                                 mod_sum_females_good[[11]]$sim[, 12],
                                 mod_sum_females_poor[[11]]$sim[, 12],
                                 mod_sum_males_good[[11]]$sim[, 12],
                                 mod_sum_males_poor[[11]]$sim[, 12],
                                 mod_sum_females_good[[12]]$sim[, 12],
                                 mod_sum_females_poor[[12]]$sim[, 12],
                                 mod_sum_males_good[[12]]$sim[, 12],
                                 mod_sum_males_poor[[12]]$sim[, 12],
                                 mod_sum_females_good[[13]]$sim[, 12],
                                 mod_sum_females_poor[[13]]$sim[, 12],
                                 mod_sum_males_good[[13]]$sim[, 12],
                                 mod_sum_males_poor[[13]]$sim[, 12],
                                 mod_sum_females_good[[14]]$sim[, 12],
                                 mod_sum_females_poor[[14]]$sim[, 12],
                                 mod_sum_males_good[[14]]$sim[, 12],
                                 mod_sum_males_poor[[14]]$sim[, 12],
                                 mod_sum_females_good[[15]]$sim[, 12],
                                 mod_sum_females_poor[[15]]$sim[, 12],
                                 mod_sum_males_good[[15]]$sim[, 12],
                                 mod_sum_males_poor[[15]]$sim[, 12],
                                 mod_sum_females_good[[16]]$sim[, 12],
                                 mod_sum_females_poor[[16]]$sim[, 12],
                                 mod_sum_males_good[[16]]$sim[, 12],
                                 mod_sum_males_poor[[16]]$sim[, 12],
                                 mod_sum_females_good[[17]]$sim[, 12],
                                 mod_sum_females_poor[[17]]$sim[, 12],
                                 mod_sum_males_good[[17]]$sim[, 12],
                                 mod_sum_males_poor[[17]]$sim[, 12],
                                 mod_sum_females_good[[18]]$sim[, 12],
                                 mod_sum_females_poor[[18]]$sim[, 12],
                                 mod_sum_males_good[[18]]$sim[, 12],
                                 mod_sum_males_poor[[18]]$sim[, 12],
                                 mod_sum_females_good[[19]]$sim[, 12],
                                 mod_sum_females_poor[[19]]$sim[, 12],
                                 mod_sum_males_good[[19]]$sim[, 12],
                                 mod_sum_males_poor[[19]]$sim[, 12],
                                 mod_sum_females_good[[20]]$sim[, 12],
                                 mod_sum_females_poor[[20]]$sim[, 12],
                                 mod_sum_males_good[[20]]$sim[, 12],
                                 mod_sum_males_poor[[20]]$sim[, 12],
                                 mod_sum_females_good[[21]]$sim[, 12],
                                 mod_sum_females_poor[[21]]$sim[, 12],
                                 mod_sum_males_good[[21]]$sim[, 12],
                                 mod_sum_males_poor[[21]]$sim[, 12],
                                 mod_sum_females_good[[22]]$sim[, 12],
                                 mod_sum_females_poor[[22]]$sim[, 12],
                                 mod_sum_males_good[[22]]$sim[, 12],
                                 mod_sum_males_poor[[22]]$sim[, 12],
                                 mod_sum_females_good[[23]]$sim[, 12],
                                 mod_sum_females_poor[[23]]$sim[, 12],
                                 mod_sum_males_good[[23]]$sim[, 12],
                                 mod_sum_males_poor[[23]]$sim[, 12],
                                 mod_sum_females_good[[24]]$sim[, 12],
                                 mod_sum_females_poor[[24]]$sim[, 12],
                                 mod_sum_males_good[[24]]$sim[, 12],
                                 mod_sum_males_poor[[24]]$sim[, 12],
                                 mod_sum_females_good[[25]]$sim[, 12],
                                 mod_sum_females_poor[[25]]$sim[, 12],
                                 mod_sum_males_good[[25]]$sim[, 12],
                                 mod_sum_males_poor[[25]]$sim[, 12],
                                 mod_sum_females_good[[26]]$sim[, 12],
                                 mod_sum_females_poor[[26]]$sim[, 12],
                                 mod_sum_males_good[[26]]$sim[, 12],
                                 mod_sum_males_poor[[26]]$sim[, 12],
                                 mod_sum_females_good[[27]]$sim[, 12],
                                 mod_sum_females_poor[[27]]$sim[, 12],
                                 mod_sum_males_good[[27]]$sim[, 12],
                                 mod_sum_males_poor[[27]]$sim[, 12],
                                 mod_sum_females_good[[28]]$sim[, 12],
                                 mod_sum_females_poor[[28]]$sim[, 12],
                                 mod_sum_males_good[[28]]$sim[, 12],
                                 mod_sum_males_poor[[28]]$sim[, 12],
                                 mod_sum_females_good[[29]]$sim[, 12],
                                 mod_sum_females_poor[[29]]$sim[, 12],
                                 mod_sum_males_good[[29]]$sim[, 12],
                                 mod_sum_males_poor[[29]]$sim[, 12],
                                 mod_sum_females_good[[30]]$sim[, 12],
                                 mod_sum_females_poor[[30]]$sim[, 12],
                                 mod_sum_males_good[[30]]$sim[, 12],
                                 mod_sum_males_poor[[30]]$sim[, 12],
                                 mod_sum_females_good[[31]]$sim[, 12],
                                 mod_sum_females_poor[[31]]$sim[, 12],
                                 mod_sum_males_good[[31]]$sim[, 12],
                                 mod_sum_males_poor[[31]]$sim[, 12],
                                 mod_sum_females_good[[32]]$sim[, 12],
                                 mod_sum_females_poor[[32]]$sim[, 12],
                                 mod_sum_males_good[[32]]$sim[, 12],
                                 mod_sum_males_poor[[32]]$sim[, 12],
                                 mod_sum_females_good[[33]]$sim[, 12],
                                 mod_sum_females_poor[[33]]$sim[, 12],
                                 mod_sum_males_good[[33]]$sim[, 12],
                                 mod_sum_males_poor[[33]]$sim[, 12],
                                 mod_sum_females_good[[34]]$sim[, 12],
                                 mod_sum_females_poor[[34]]$sim[, 12],
                                 mod_sum_males_good[[34]]$sim[, 12],
                                 mod_sum_males_poor[[34]]$sim[, 12],
                                 mod_sum_females_good[[35]]$sim[, 12],
                                 mod_sum_females_poor[[35]]$sim[, 12],
                                 mod_sum_males_good[[35]]$sim[, 12],
                                 mod_sum_males_poor[[35]]$sim[, 12],
                                 mod_sum_females_good[[36]]$sim[, 12],
                                 mod_sum_females_poor[[36]]$sim[, 12],
                                 mod_sum_males_good[[36]]$sim[, 12],
                                 mod_sum_males_poor[[36]]$sim[, 12],
                                 mod_sum_females_good[[37]]$sim[, 12],
                                 mod_sum_females_poor[[37]]$sim[, 12],
                                 mod_sum_males_good[[37]]$sim[, 12],
                                 mod_sum_males_poor[[37]]$sim[, 12],
                                 mod_sum_females_good[[38]]$sim[, 12],
                                 mod_sum_females_poor[[38]]$sim[, 12],
                                 mod_sum_males_good[[38]]$sim[, 12],
                                 mod_sum_males_poor[[38]]$sim[, 12],
                                 mod_sum_females_good[[39]]$sim[, 12],
                                 mod_sum_females_poor[[39]]$sim[, 12],
                                 mod_sum_males_good[[39]]$sim[, 12],
                                 mod_sum_males_poor[[39]]$sim[, 12],
                                 mod_sum_females_good[[40]]$sim[, 12],
                                 mod_sum_females_poor[[40]]$sim[, 12],
                                 mod_sum_males_good[[40]]$sim[, 12],
                                 mod_sum_males_poor[[40]]$sim[, 12],
                                 mod_sum_females_good[[41]]$sim[, 12],
                                 mod_sum_females_poor[[41]]$sim[, 12],
                                 mod_sum_males_good[[41]]$sim[, 12],
                                 mod_sum_males_poor[[41]]$sim[, 12]))


# Combining datasets
comp.data <- free.data %>%
  rbind(mci.data, dem.data)

# plot!
tiff("ov_grey.tiff", height = 6, width = 7, res = 300, units = "in")
comp.data %>%
  group_by(sex, pp, year, type) %>%
  summarise(value = mean(value)) %>%
  mutate(type = factor(type,
                       levels = c("Dementia",
                                  "MCI",
                                  "Dementia-free"))) %>%
  ggplot(aes(fill = type, x = year, y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(pp ~ sex) +
  labs(x = "Age (years)",
       y = "Estimated Life Expectancy (years)",
       fill = "") +
  theme_base() +
  scale_x_continuous(breaks = seq(65, 105, 10)) +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  theme(legend.position = "bottom")+
  scale_fill_brewer(breaks = c("Dementia-free",
                               "MCI",
                               "Dementia"),
                    palette = "Greys")
dev.off()

# Numerical data corresponding to total LE and marginal LEs
table <- comp.data %>%
  group_by(sex, pp, year, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(type, value) %>%
  select(sex, pp, year, `Dementia-free`, MCI, Dementia) %>%
  mutate(prop.free = (`Dementia-free` / (`Dementia-free` + MCI + Dementia)) * 100,
         prop.dem = (Dementia / (`Dementia-free` + MCI + Dementia)) * 100)

write.table(table, file = "total.txt", 
            sep = ",", quote = FALSE, row.names = F)





# total people per round
plm_exam %>%
  group_by(wave) %>%
  summarise(n = n())

# original cohort and successive waves
id.1 <- plm_exam %>% filter(wave == 1) %>% .$id %>% unique()
id.2 <- plm_exam %>% filter(wave == 2 & !id %in% id.1) %>% .$id %>% unique()
id.11 <- plm_exam %>% filter(wave == 11 & !id %in% c(id.1, id.5)) %>% .$id %>% unique()
plm_exam %>% filter(wave == 12 & !id %in% c(id.1, id.5)) %>% nrow()

# incidence and prevalence
id.dem.12 <- plm_exam %>% filter(!is.na(demclas) & wave == 12 & demclas == 3) %>% .$id %>% unique()
plm_exam %>% filter(!is.na(demclas) & wave == 12 & demclas == 3) %>% nrow()
plm_exam %>% filter(!is.na(demclas) & wave == 12 & demclas == 3 & 
                      id %in% c(id.dem.1, id.dem.2, id.dem.3,
                                id.dem.4, id.dem.5, id.dem.6,
                                id.dem.7, id.dem.8, id.dem.9,
                                id.dem.10, id.dem.11)) %>% nrow()
  
# life expectations for a person who is healthy
## dementia-free
i <- 6

# dataset # cannot forget changing the name of the dataset
dem.data <- data.frame(sex = rep(c("Women", "Men"), each = 200, times = 41),
                       pp = rep(c("Good Physical Function", "Poor Physical Function"), each = 100, times = 82),
                       year = rep(seq(65, 105), each = 400),
                       type = "Dementia",
                       value = c(mod_sum_females_good[[1]]$sim[, i],
                                 mod_sum_females_poor[[1]]$sim[, i],
                                 mod_sum_males_good[[1]]$sim[, i],
                                 mod_sum_males_poor[[1]]$sim[, i],
                                 mod_sum_females_good[[2]]$sim[, i],
                                 mod_sum_females_poor[[2]]$sim[, i],
                                 mod_sum_males_good[[2]]$sim[, i],
                                 mod_sum_males_poor[[2]]$sim[, i],
                                 mod_sum_females_good[[3]]$sim[, i],
                                 mod_sum_females_poor[[3]]$sim[, i],
                                 mod_sum_males_good[[3]]$sim[, i],
                                 mod_sum_males_poor[[3]]$sim[, i],
                                 mod_sum_females_good[[4]]$sim[, i],
                                 mod_sum_females_poor[[4]]$sim[, i],
                                 mod_sum_males_good[[4]]$sim[, i],
                                 mod_sum_males_poor[[4]]$sim[, i],
                                 mod_sum_females_good[[5]]$sim[, i],
                                 mod_sum_females_poor[[5]]$sim[, i],
                                 mod_sum_males_good[[5]]$sim[, i],
                                 mod_sum_males_poor[[5]]$sim[, i],
                                 mod_sum_females_good[[6]]$sim[, i],
                                 mod_sum_females_poor[[6]]$sim[, i],
                                 mod_sum_males_good[[6]]$sim[, i],
                                 mod_sum_males_poor[[6]]$sim[, i],
                                 mod_sum_females_good[[7]]$sim[, i],
                                 mod_sum_females_poor[[7]]$sim[, i],
                                 mod_sum_males_good[[7]]$sim[, i],
                                 mod_sum_males_poor[[7]]$sim[, i],
                                 mod_sum_females_good[[8]]$sim[, i],
                                 mod_sum_females_poor[[8]]$sim[, i],
                                 mod_sum_males_good[[8]]$sim[, i],
                                 mod_sum_males_poor[[8]]$sim[, i],
                                 mod_sum_females_good[[9]]$sim[, i],
                                 mod_sum_females_poor[[9]]$sim[, i],
                                 mod_sum_males_good[[9]]$sim[, i],
                                 mod_sum_males_poor[[9]]$sim[, i],
                                 mod_sum_females_good[[10]]$sim[, i],
                                 mod_sum_females_poor[[10]]$sim[, i],
                                 mod_sum_males_good[[10]]$sim[, i],
                                 mod_sum_males_poor[[10]]$sim[, i],
                                 mod_sum_females_good[[11]]$sim[, i],
                                 mod_sum_females_poor[[11]]$sim[, i],
                                 mod_sum_males_good[[11]]$sim[, i],
                                 mod_sum_males_poor[[11]]$sim[, i],
                                 mod_sum_females_good[[12]]$sim[, i],
                                 mod_sum_females_poor[[12]]$sim[, i],
                                 mod_sum_males_good[[12]]$sim[, i],
                                 mod_sum_males_poor[[12]]$sim[, i],
                                 mod_sum_females_good[[13]]$sim[, i],
                                 mod_sum_females_poor[[13]]$sim[, i],
                                 mod_sum_males_good[[13]]$sim[, i],
                                 mod_sum_males_poor[[13]]$sim[, i],
                                 mod_sum_females_good[[14]]$sim[, i],
                                 mod_sum_females_poor[[14]]$sim[, i],
                                 mod_sum_males_good[[14]]$sim[, i],
                                 mod_sum_males_poor[[14]]$sim[, i],
                                 mod_sum_females_good[[15]]$sim[, i],
                                 mod_sum_females_poor[[15]]$sim[, i],
                                 mod_sum_males_good[[15]]$sim[, i],
                                 mod_sum_males_poor[[15]]$sim[, i],
                                 mod_sum_females_good[[16]]$sim[, i],
                                 mod_sum_females_poor[[16]]$sim[, i],
                                 mod_sum_males_good[[16]]$sim[, i],
                                 mod_sum_males_poor[[16]]$sim[, i],
                                 mod_sum_females_good[[17]]$sim[, i],
                                 mod_sum_females_poor[[17]]$sim[, i],
                                 mod_sum_males_good[[17]]$sim[, i],
                                 mod_sum_males_poor[[17]]$sim[, i],
                                 mod_sum_females_good[[18]]$sim[, i],
                                 mod_sum_females_poor[[18]]$sim[, i],
                                 mod_sum_males_good[[18]]$sim[, i],
                                 mod_sum_males_poor[[18]]$sim[, i],
                                 mod_sum_females_good[[19]]$sim[, i],
                                 mod_sum_females_poor[[19]]$sim[, i],
                                 mod_sum_males_good[[19]]$sim[, i],
                                 mod_sum_males_poor[[19]]$sim[, i],
                                 mod_sum_females_good[[20]]$sim[, i],
                                 mod_sum_females_poor[[20]]$sim[, i],
                                 mod_sum_males_good[[20]]$sim[, i],
                                 mod_sum_males_poor[[20]]$sim[, i],
                                 mod_sum_females_good[[21]]$sim[, i],
                                 mod_sum_females_poor[[21]]$sim[, i],
                                 mod_sum_males_good[[21]]$sim[, i],
                                 mod_sum_males_poor[[21]]$sim[, i],
                                 mod_sum_females_good[[22]]$sim[, i],
                                 mod_sum_females_poor[[22]]$sim[, i],
                                 mod_sum_males_good[[22]]$sim[, i],
                                 mod_sum_males_poor[[22]]$sim[, i],
                                 mod_sum_females_good[[23]]$sim[, i],
                                 mod_sum_females_poor[[23]]$sim[, i],
                                 mod_sum_males_good[[23]]$sim[, i],
                                 mod_sum_males_poor[[23]]$sim[, i],
                                 mod_sum_females_good[[24]]$sim[, i],
                                 mod_sum_females_poor[[24]]$sim[, i],
                                 mod_sum_males_good[[24]]$sim[, i],
                                 mod_sum_males_poor[[24]]$sim[, i],
                                 mod_sum_females_good[[25]]$sim[, i],
                                 mod_sum_females_poor[[25]]$sim[, i],
                                 mod_sum_males_good[[25]]$sim[, i],
                                 mod_sum_males_poor[[25]]$sim[, i],
                                 mod_sum_females_good[[26]]$sim[, i],
                                 mod_sum_females_poor[[26]]$sim[, i],
                                 mod_sum_males_good[[26]]$sim[, i],
                                 mod_sum_males_poor[[26]]$sim[, i],
                                 mod_sum_females_good[[27]]$sim[, i],
                                 mod_sum_females_poor[[27]]$sim[, i],
                                 mod_sum_males_good[[27]]$sim[, i],
                                 mod_sum_males_poor[[27]]$sim[, i],
                                 mod_sum_females_good[[28]]$sim[, i],
                                 mod_sum_females_poor[[28]]$sim[, i],
                                 mod_sum_males_good[[28]]$sim[, i],
                                 mod_sum_males_poor[[28]]$sim[, i],
                                 mod_sum_females_good[[29]]$sim[, i],
                                 mod_sum_females_poor[[29]]$sim[, i],
                                 mod_sum_males_good[[29]]$sim[, i],
                                 mod_sum_males_poor[[29]]$sim[, i],
                                 mod_sum_females_good[[30]]$sim[, i],
                                 mod_sum_females_poor[[30]]$sim[, i],
                                 mod_sum_males_good[[30]]$sim[, i],
                                 mod_sum_males_poor[[30]]$sim[, i],
                                 mod_sum_females_good[[31]]$sim[, i],
                                 mod_sum_females_poor[[31]]$sim[, i],
                                 mod_sum_males_good[[31]]$sim[, i],
                                 mod_sum_males_poor[[31]]$sim[, i],
                                 mod_sum_females_good[[32]]$sim[, i],
                                 mod_sum_females_poor[[32]]$sim[, i],
                                 mod_sum_males_good[[32]]$sim[, i],
                                 mod_sum_males_poor[[32]]$sim[, i],
                                 mod_sum_females_good[[33]]$sim[, i],
                                 mod_sum_females_poor[[33]]$sim[, i],
                                 mod_sum_males_good[[33]]$sim[, i],
                                 mod_sum_males_poor[[33]]$sim[, i],
                                 mod_sum_females_good[[34]]$sim[, i],
                                 mod_sum_females_poor[[34]]$sim[, i],
                                 mod_sum_males_good[[34]]$sim[, i],
                                 mod_sum_males_poor[[34]]$sim[, i],
                                 mod_sum_females_good[[35]]$sim[, i],
                                 mod_sum_females_poor[[35]]$sim[, i],
                                 mod_sum_males_good[[35]]$sim[, i],
                                 mod_sum_males_poor[[35]]$sim[, i],
                                 mod_sum_females_good[[36]]$sim[, i],
                                 mod_sum_females_poor[[36]]$sim[, i],
                                 mod_sum_males_good[[36]]$sim[, i],
                                 mod_sum_males_poor[[36]]$sim[, i],
                                 mod_sum_females_good[[37]]$sim[, i],
                                 mod_sum_females_poor[[37]]$sim[, i],
                                 mod_sum_males_good[[37]]$sim[, i],
                                 mod_sum_males_poor[[37]]$sim[, i],
                                 mod_sum_females_good[[38]]$sim[, i],
                                 mod_sum_females_poor[[38]]$sim[, i],
                                 mod_sum_males_good[[38]]$sim[, i],
                                 mod_sum_males_poor[[38]]$sim[, i],
                                 mod_sum_females_good[[39]]$sim[, i],
                                 mod_sum_females_poor[[39]]$sim[, i],
                                 mod_sum_males_good[[39]]$sim[, i],
                                 mod_sum_males_poor[[39]]$sim[, i],
                                 mod_sum_females_good[[40]]$sim[, i],
                                 mod_sum_females_poor[[40]]$sim[, i],
                                 mod_sum_males_good[[40]]$sim[, i],
                                 mod_sum_males_poor[[40]]$sim[, i],
                                 mod_sum_females_good[[41]]$sim[, i],
                                 mod_sum_females_poor[[41]]$sim[, i],
                                 mod_sum_males_good[[41]]$sim[, i],
                                 mod_sum_males_poor[[41]]$sim[, i]))

summary.elect(mod_sum_males_good[[1]])
# Mean difference in expected years lived in dementia state if a person was healthy at this specific age
## set the values 
i <- 9
x <- mod_sum_males_good
y <- mod_sum_males_poor
mean_qi(x[[1]]$sim[, i] - y[[1]]$sim[, i])[, 1:3]

# calculation of differences
males_good_males_poor <- as.data.frame(mean_qi(x[[1]]$sim[, i] - y[[1]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[2]]$sim[, i] - y[[2]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[3]]$sim[, i] - y[[3]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[4]]$sim[, i] - y[[4]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[5]]$sim[, i] - y[[5]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[6]]$sim[, i] - y[[6]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[7]]$sim[, i] - y[[7]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[8]]$sim[, i] - y[[8]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[9]]$sim[, i] - y[[9]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[10]]$sim[, i] - y[[10]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[11]]$sim[, i] - y[[11]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[12]]$sim[, i] - y[[12]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[13]]$sim[, i] - y[[13]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[14]]$sim[, i] - y[[14]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[15]]$sim[, i] - y[[15]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[16]]$sim[, i] - y[[16]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[17]]$sim[, i] - y[[17]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[18]]$sim[, i] - y[[18]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[19]]$sim[, i] - y[[19]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[20]]$sim[, i] - y[[20]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[21]]$sim[, i] - y[[21]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[22]]$sim[, i] - y[[22]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[23]]$sim[, i] - y[[23]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[24]]$sim[, i] - y[[24]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[25]]$sim[, i] - y[[25]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[26]]$sim[, i] - y[[26]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[27]]$sim[, i] - y[[27]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[28]]$sim[, i] - y[[28]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[29]]$sim[, i] - y[[29]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[30]]$sim[, i] - y[[30]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[31]]$sim[, i] - y[[31]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[32]]$sim[, i] - y[[32]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[33]]$sim[, i] - y[[33]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[34]]$sim[, i] - y[[34]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[35]]$sim[, i] - y[[35]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[36]]$sim[, i] - y[[36]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[37]]$sim[, i] - y[[37]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[38]]$sim[, i] - y[[38]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[39]]$sim[, i] - y[[39]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[40]]$sim[, i] - y[[40]]$sim[, i])[, 1:3]) %>%
  rbind(mean_qi(x[[41]]$sim[, i] - y[[41]]$sim[, i])[, 1:3]) %>%
  mutate_if(is.numeric, round, 2) %>%
  cbind(year = seq(65, 105),
        comparison = rep("Men with good PF\nvs.\nMen with poor PF"))

# tile plot
females_good_males_good %>%
  rbind(females_good_females_poor,
        males_good_males_poor,
        females_poor_males_poor) %>%
  ggplot(aes(x = year, y = comparison, fill = y)) +
  geom_tile(color = "white") +
  geom_text(aes(label = y), col = "white") +
  theme_minimal()

# interval plot
tiff("diff_free_from_healthy.tiff", height = 6, width = 10, res = 300, units = "in")
females_good_females_poor %>%
  rbind(males_good_males_poor) %>%
  mutate(comparison = ifelse(comparison == "Women with good PF\nvs.\nWomen with poor PF",
                             "Women", "Men")) %>%
  ggplot(aes(x = year, y = y)) +
  geom_pointinterval(aes(ymin = ymin,
                         ymax = ymax)) +
  facet_wrap(~ comparison) +
  labs(y = "Estimated years of life free of MCI and dementia gained with a good physical function",
       x = "Age (years)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(65, 105, 10)) +
  scale_y_continuous(breaks = seq(-5, 20, 0.5)) +
  theme(axis.title.y = element_text(size = 9,
                                    vjust = +3),
        axis.title.x = element_text(vjust = -0.75))
dev.off()

# numerical data
table <- rbind(females_good_females_poor,
        males_good_males_poor) %>% 
  mutate(comparison = ifelse(comparison == "Women with good PF\nvs.\nWomen with poor PF",
                             "Women", "Men"),
         `Current state` = "Dementia",
         `Remaining years in this state` = "Dementia") %>%
  dplyr::select(`Current state`, `Remaining years in this state`,
                comparison, year, y, ymin, ymax)

write.table(table, file = "specific.txt", 
            sep = ",", quote = FALSE, row.names = F)

### create a dataset with total LE estimates
LE_data <- data.frame(LE = c(mod_summaries[[1]]$sim[, 13],
                             mod_summaries[[2]]$sim[, 13],
                             mod_summaries[[3]]$sim[, 13],
                             mod_summaries[[4]]$sim[, 13],
                             mod_summaries[[5]]$sim[, 13],
                             mod_summaries[[6]]$sim[, 13],
                             mod_summaries[[7]]$sim[, 13],
                             mod_summaries[[8]]$sim[, 13],
                             mod_summaries[[9]]$sim[, 13],
                             mod_summaries[[10]]$sim[, 13],
                             mod_summaries[[11]]$sim[, 13],
                             mod_summaries[[12]]$sim[, 13],
                             mod_summaries[[13]]$sim[, 13],
                             mod_summaries[[14]]$sim[, 13],
                             mod_summaries[[15]]$sim[, 13],
                             mod_summaries[[16]]$sim[, 13],
                             mod_summaries[[17]]$sim[, 13],
                             mod_summaries[[18]]$sim[, 13],
                             mod_summaries[[19]]$sim[, 13],
                             mod_summaries[[20]]$sim[, 13],
                             mod_summaries[[21]]$sim[, 13],
                             mod_summaries[[22]]$sim[, 13],
                             mod_summaries[[23]]$sim[, 13],
                             mod_summaries[[24]]$sim[, 13],
                             mod_summaries[[25]]$sim[, 13],
                             mod_summaries[[26]]$sim[, 13],
                             mod_summaries[[27]]$sim[, 13],
                             mod_summaries[[28]]$sim[, 13],
                             mod_summaries[[29]]$sim[, 13],
                             mod_summaries[[30]]$sim[, 13],
                             mod_summaries[[31]]$sim[, 13],
                             mod_summaries[[32]]$sim[, 13],
                             mod_summaries[[33]]$sim[, 13],
                             mod_summaries[[34]]$sim[, 13],
                             mod_summaries[[35]]$sim[, 13],
                             mod_summaries[[36]]$sim[, 13],
                             mod_summaries[[37]]$sim[, 13],
                             mod_summaries[[38]]$sim[, 13],
                             mod_summaries[[39]]$sim[, 13],
                             mod_summaries[[40]]$sim[, 13],
                             mod_summaries[[41]]$sim[, 13],
                             mod_summaries_poor[[1]]$sim[, 13],
                             mod_summaries_poor[[2]]$sim[, 13],
                             mod_summaries_poor[[3]]$sim[, 13],
                             mod_summaries_poor[[4]]$sim[, 13],
                             mod_summaries_poor[[5]]$sim[, 13],
                             mod_summaries_poor[[6]]$sim[, 13],
                             mod_summaries_poor[[7]]$sim[, 13],
                             mod_summaries_poor[[8]]$sim[, 13],
                             mod_summaries_poor[[9]]$sim[, 13],
                             mod_summaries_poor[[10]]$sim[, 13],
                             mod_summaries_poor[[11]]$sim[, 13],
                             mod_summaries_poor[[12]]$sim[, 13],
                             mod_summaries_poor[[13]]$sim[, 13],
                             mod_summaries_poor[[14]]$sim[, 13],
                             mod_summaries_poor[[15]]$sim[, 13],
                             mod_summaries_poor[[16]]$sim[, 13],
                             mod_summaries_poor[[17]]$sim[, 13],
                             mod_summaries_poor[[18]]$sim[, 13],
                             mod_summaries_poor[[19]]$sim[, 13],
                             mod_summaries_poor[[20]]$sim[, 13],
                             mod_summaries_poor[[21]]$sim[, 13],
                             mod_summaries_poor[[22]]$sim[, 13],
                             mod_summaries_poor[[23]]$sim[, 13],
                             mod_summaries_poor[[24]]$sim[, 13],
                             mod_summaries_poor[[25]]$sim[, 13],
                             mod_summaries_poor[[26]]$sim[, 13],
                             mod_summaries_poor[[27]]$sim[, 13],
                             mod_summaries_poor[[28]]$sim[, 13],
                             mod_summaries_poor[[29]]$sim[, 13],
                             mod_summaries_poor[[30]]$sim[, 13],
                             mod_summaries_poor[[31]]$sim[, 13],
                             mod_summaries_poor[[32]]$sim[, 13],
                             mod_summaries_poor[[33]]$sim[, 13],
                             mod_summaries_poor[[34]]$sim[, 13],
                             mod_summaries_poor[[35]]$sim[, 13],
                             mod_summaries_poor[[36]]$sim[, 13],
                             mod_summaries_poor[[37]]$sim[, 13],
                             mod_summaries_poor[[38]]$sim[, 13],
                             mod_summaries_poor[[39]]$sim[, 13],
                             mod_summaries_poor[[40]]$sim[, 13],
                             mod_summaries_poor[[41]]$sim[, 13]),
                      age = rep(seq(-15.55227 + mean.age,
                                    24.44773 + mean.age, 1), 
                                each = 500, times = 2),
                      sppb = rep(c("Poor Physical Function",
                                   "Good Physical Function"), each = 20500)) 

### create a dataset with dementia-free LE estimates
LE_data <- data.frame(LE = c(mod_summaries_poor[[1]]$sim[, 1],
                             mod_summaries_poor[[2]]$sim[, 1],
                             mod_summaries_poor[[3]]$sim[, 1],
                             mod_summaries_poor[[4]]$sim[, 1],
                             mod_summaries_poor[[5]]$sim[, 1],
                             mod_summaries_poor[[6]]$sim[, 1],
                             mod_summaries_poor[[7]]$sim[, 1],
                             mod_summaries_poor[[8]]$sim[, 1],
                             mod_summaries_poor[[9]]$sim[, 1],
                             mod_summaries_poor[[10]]$sim[, 1],
                             mod_summaries_poor[[11]]$sim[, 1],
                             mod_summaries_poor[[12]]$sim[, 1],
                             mod_summaries_poor[[13]]$sim[, 1],
                             mod_summaries_poor[[14]]$sim[, 1],
                             mod_summaries_poor[[15]]$sim[, 1],
                             mod_summaries_poor[[16]]$sim[, 1],
                             mod_summaries_poor[[17]]$sim[, 1],
                             mod_summaries_poor[[18]]$sim[, 1],
                             mod_summaries_poor[[19]]$sim[, 1],
                             mod_summaries_poor[[20]]$sim[, 1],
                             mod_summaries_poor[[21]]$sim[, 1],
                             mod_summaries_poor[[22]]$sim[, 1],
                             mod_summaries_poor[[23]]$sim[, 1],
                             mod_summaries_poor[[24]]$sim[, 1],
                             mod_summaries_poor[[25]]$sim[, 1],
                             mod_summaries_poor[[26]]$sim[, 1],
                             mod_summaries_poor[[27]]$sim[, 1],
                             mod_summaries_poor[[28]]$sim[, 1],
                             mod_summaries_poor[[29]]$sim[, 1],
                             mod_summaries_poor[[30]]$sim[, 1],
                             mod_summaries_poor[[31]]$sim[, 1],
                             mod_summaries_poor[[32]]$sim[, 1],
                             mod_summaries_poor[[33]]$sim[, 1],
                             mod_summaries_poor[[34]]$sim[, 1],
                             mod_summaries_poor[[35]]$sim[, 1],
                             mod_summaries_poor[[36]]$sim[, 1],
                             mod_summaries_poor[[37]]$sim[, 1],
                             mod_summaries_poor[[38]]$sim[, 1],
                             mod_summaries_poor[[39]]$sim[, 1],
                             mod_summaries_poor[[40]]$sim[, 1],
                             mod_summaries_poor[[41]]$sim[, 1],
                             mod_summaries[[1]]$sim[, 1],
                             mod_summaries[[2]]$sim[, 1],
                             mod_summaries[[3]]$sim[, 1],
                             mod_summaries[[4]]$sim[, 1],
                             mod_summaries[[5]]$sim[, 1],
                             mod_summaries[[6]]$sim[, 1],
                             mod_summaries[[7]]$sim[, 1],
                             mod_summaries[[8]]$sim[, 1],
                             mod_summaries[[9]]$sim[, 1],
                             mod_summaries[[10]]$sim[, 1],
                             mod_summaries[[11]]$sim[, 1],
                             mod_summaries[[12]]$sim[, 1],
                             mod_summaries[[13]]$sim[, 1],
                             mod_summaries[[14]]$sim[, 1],
                             mod_summaries[[15]]$sim[, 1],
                             mod_summaries[[16]]$sim[, 1],
                             mod_summaries[[17]]$sim[, 1],
                             mod_summaries[[18]]$sim[, 1],
                             mod_summaries[[19]]$sim[, 1],
                             mod_summaries[[20]]$sim[, 1],
                             mod_summaries[[21]]$sim[, 1],
                             mod_summaries[[22]]$sim[, 1],
                             mod_summaries[[23]]$sim[, 1],
                             mod_summaries[[24]]$sim[, 1],
                             mod_summaries[[25]]$sim[, 1],
                             mod_summaries[[26]]$sim[, 1],
                             mod_summaries[[27]]$sim[, 1],
                             mod_summaries[[28]]$sim[, 1],
                             mod_summaries[[29]]$sim[, 1],
                             mod_summaries[[30]]$sim[, 1],
                             mod_summaries[[31]]$sim[, 1],
                             mod_summaries[[32]]$sim[, 1],
                             mod_summaries[[33]]$sim[, 1],
                             mod_summaries[[34]]$sim[, 1],
                             mod_summaries[[35]]$sim[, 1],
                             mod_summaries[[36]]$sim[, 1],
                             mod_summaries[[37]]$sim[, 1],
                             mod_summaries[[38]]$sim[, 1],
                             mod_summaries[[39]]$sim[, 1],
                             mod_summaries[[40]]$sim[, 1],
                             mod_summaries[[41]]$sim[, 1]),
                      age = rep(seq(-15.55227 + mean.age,
                                    24.44773 + mean.age, 1), 
                                each = 500, times = 2),
                      sppb = rep(c("Good Physical Function",
                                   "Poor Physical Function"), each = 20500)) 


LE_diff <- data.frame(LE = c(mod_summaries_poor[[1]]$sim[, 13] - mod_summaries[[1]]$sim[, 13],
                             mod_summaries_poor[[2]]$sim[, 13] - mod_summaries[[2]]$sim[, 13],
                             mod_summaries_poor[[3]]$sim[, 13] - mod_summaries[[3]]$sim[, 13],
                             mod_summaries_poor[[4]]$sim[, 13] - mod_summaries[[4]]$sim[, 13],
                             mod_summaries_poor[[5]]$sim[, 13] - mod_summaries[[5]]$sim[, 13],
                             mod_summaries_poor[[6]]$sim[, 13] - mod_summaries[[6]]$sim[, 13],
                             mod_summaries_poor[[7]]$sim[, 13]- mod_summaries[[7]]$sim[, 13],
                             mod_summaries_poor[[8]]$sim[, 13]- mod_summaries[[8]]$sim[, 13],
                             mod_summaries_poor[[9]]$sim[, 13]- mod_summaries[[9]]$sim[, 13],
                             mod_summaries_poor[[10]]$sim[, 13]- mod_summaries[[10]]$sim[, 13],
                             mod_summaries_poor[[11]]$sim[, 13]- mod_summaries[[11]]$sim[, 13],
                             mod_summaries_poor[[12]]$sim[, 13]- mod_summaries[[12]]$sim[, 13],
                             mod_summaries_poor[[13]]$sim[, 13]- mod_summaries[[13]]$sim[, 13],
                             mod_summaries_poor[[14]]$sim[, 13]- mod_summaries[[14]]$sim[, 13],
                             mod_summaries_poor[[15]]$sim[, 13]- mod_summaries[[15]]$sim[, 13],
                             mod_summaries_poor[[16]]$sim[, 13]- mod_summaries[[16]]$sim[, 13],
                             mod_summaries_poor[[17]]$sim[, 13]- mod_summaries[[17]]$sim[, 13],
                             mod_summaries_poor[[18]]$sim[, 13]- mod_summaries[[18]]$sim[, 13],
                             mod_summaries_poor[[19]]$sim[, 13]- mod_summaries[[19]]$sim[, 13],
                             mod_summaries_poor[[20]]$sim[, 13]- mod_summaries[[20]]$sim[, 13],
                             mod_summaries_poor[[21]]$sim[, 13]- mod_summaries[[21]]$sim[, 13],
                             mod_summaries_poor[[22]]$sim[, 13]- mod_summaries[[22]]$sim[, 13],
                             mod_summaries_poor[[23]]$sim[, 13]- mod_summaries[[23]]$sim[, 13],
                             mod_summaries_poor[[24]]$sim[, 13]- mod_summaries[[24]]$sim[, 13],
                             mod_summaries_poor[[25]]$sim[, 13]- mod_summaries[[25]]$sim[, 13],
                             mod_summaries_poor[[26]]$sim[, 13]- mod_summaries[[26]]$sim[, 13],
                             mod_summaries_poor[[27]]$sim[, 13]- mod_summaries[[27]]$sim[, 13],
                             mod_summaries_poor[[28]]$sim[, 13]- mod_summaries[[28]]$sim[, 13],
                             mod_summaries_poor[[29]]$sim[, 13]- mod_summaries[[29]]$sim[, 13],
                             mod_summaries_poor[[30]]$sim[, 13]- mod_summaries[[30]]$sim[, 13],
                             mod_summaries_poor[[31]]$sim[, 13]- mod_summaries[[31]]$sim[, 13],
                             mod_summaries_poor[[32]]$sim[, 13]- mod_summaries[[32]]$sim[, 13],
                             mod_summaries_poor[[33]]$sim[, 13]- mod_summaries[[33]]$sim[, 13],
                             mod_summaries_poor[[34]]$sim[, 13]- mod_summaries[[34]]$sim[, 13],
                             mod_summaries_poor[[35]]$sim[, 13]- mod_summaries[[35]]$sim[, 13],
                             mod_summaries_poor[[36]]$sim[, 13]- mod_summaries[[36]]$sim[, 13],
                             mod_summaries_poor[[37]]$sim[, 13]- mod_summaries[[37]]$sim[, 13],
                             mod_summaries_poor[[38]]$sim[, 13]- mod_summaries[[38]]$sim[, 13],
                             mod_summaries_poor[[39]]$sim[, 13]- mod_summaries[[39]]$sim[, 13],
                             mod_summaries_poor[[40]]$sim[, 13]- mod_summaries[[40]]$sim[, 13],
                             mod_summaries_poor[[41]]$sim[, 13]- mod_summaries[[41]]$sim[, 13]),
                      age = rep(seq(-15.55227 + mean.age,
                                    24.44773 + mean.age, 1), 
                                each = 500, times = 1)) 


LE_diff_free <- data.frame(LE = c(mod_summaries_poor[[1]]$sim[, 1] - mod_summaries[[1]]$sim[, 1],
                             mod_summaries_poor[[2]]$sim[, 1] - mod_summaries[[2]]$sim[, 1],
                             mod_summaries_poor[[3]]$sim[, 1] - mod_summaries[[3]]$sim[, 1],
                             mod_summaries_poor[[4]]$sim[, 1] - mod_summaries[[4]]$sim[, 1],
                             mod_summaries_poor[[5]]$sim[, 1] - mod_summaries[[5]]$sim[, 1],
                             mod_summaries_poor[[6]]$sim[, 1] - mod_summaries[[6]]$sim[, 1],
                             mod_summaries_poor[[7]]$sim[, 1]- mod_summaries[[7]]$sim[, 1],
                             mod_summaries_poor[[8]]$sim[, 1]- mod_summaries[[8]]$sim[, 1],
                             mod_summaries_poor[[9]]$sim[, 1]- mod_summaries[[9]]$sim[, 1],
                             mod_summaries_poor[[10]]$sim[, 1]- mod_summaries[[10]]$sim[, 1],
                             mod_summaries_poor[[11]]$sim[, 1]- mod_summaries[[11]]$sim[, 1],
                             mod_summaries_poor[[12]]$sim[, 1]- mod_summaries[[12]]$sim[, 1],
                             mod_summaries_poor[[13]]$sim[, 1]- mod_summaries[[13]]$sim[, 1],
                             mod_summaries_poor[[14]]$sim[, 1]- mod_summaries[[14]]$sim[, 1],
                             mod_summaries_poor[[15]]$sim[, 1]- mod_summaries[[15]]$sim[, 1],
                             mod_summaries_poor[[16]]$sim[, 1]- mod_summaries[[16]]$sim[, 1],
                             mod_summaries_poor[[17]]$sim[, 1]- mod_summaries[[17]]$sim[, 1],
                             mod_summaries_poor[[18]]$sim[, 1]- mod_summaries[[18]]$sim[, 1],
                             mod_summaries_poor[[19]]$sim[, 1]- mod_summaries[[19]]$sim[, 1],
                             mod_summaries_poor[[20]]$sim[, 1]- mod_summaries[[20]]$sim[, 1],
                             mod_summaries_poor[[21]]$sim[, 1]- mod_summaries[[21]]$sim[, 1],
                             mod_summaries_poor[[22]]$sim[, 1]- mod_summaries[[22]]$sim[, 1],
                             mod_summaries_poor[[23]]$sim[, 1]- mod_summaries[[23]]$sim[, 1],
                             mod_summaries_poor[[24]]$sim[, 1]- mod_summaries[[24]]$sim[, 1],
                             mod_summaries_poor[[25]]$sim[, 1]- mod_summaries[[25]]$sim[, 1],
                             mod_summaries_poor[[26]]$sim[, 1]- mod_summaries[[26]]$sim[, 1],
                             mod_summaries_poor[[27]]$sim[, 1]- mod_summaries[[27]]$sim[, 1],
                             mod_summaries_poor[[28]]$sim[, 1]- mod_summaries[[28]]$sim[, 1],
                             mod_summaries_poor[[29]]$sim[, 1]- mod_summaries[[29]]$sim[, 1],
                             mod_summaries_poor[[30]]$sim[, 1]- mod_summaries[[30]]$sim[, 1],
                             mod_summaries_poor[[31]]$sim[, 1]- mod_summaries[[31]]$sim[, 1],
                             mod_summaries_poor[[32]]$sim[, 1]- mod_summaries[[32]]$sim[, 1],
                             mod_summaries_poor[[33]]$sim[, 1]- mod_summaries[[33]]$sim[, 1],
                             mod_summaries_poor[[34]]$sim[, 1]- mod_summaries[[34]]$sim[, 1],
                             mod_summaries_poor[[35]]$sim[, 1]- mod_summaries[[35]]$sim[, 1],
                             mod_summaries_poor[[36]]$sim[, 1]- mod_summaries[[36]]$sim[, 1],
                             mod_summaries_poor[[37]]$sim[, 1]- mod_summaries[[37]]$sim[, 1],
                             mod_summaries_poor[[38]]$sim[, 1]- mod_summaries[[38]]$sim[, 1],
                             mod_summaries_poor[[39]]$sim[, 1]- mod_summaries[[39]]$sim[, 1],
                             mod_summaries_poor[[40]]$sim[, 1]- mod_summaries[[40]]$sim[, 1],
                             mod_summaries_poor[[41]]$sim[, 1]- mod_summaries[[41]]$sim[, 1]),
                      age = rep(seq(-15.55227 + mean.age,
                                    24.44773 + mean.age, 1), 
                                each = 500, times = 1)) 



LE_diff %>%
  group_by(age) %>%
  mean_qi() %>% 
  as.data.frame()

dataset <- list()
for (i in seq(1, 41, 1)) {
  dataset[[i+65]] <- mean_qi(mod_summaries_poor[[i]]$sim[, 13]) - mean_qi(mod_summaries[[i]]$sim[, 13])
}



### Data for older adults younger than 80 years old
filter_data <- LE_data %>%
  dplyr::filter(age < 80 & sppb == "Good Physical Function"
                & LE > 8)
 
good_data <- filter_data %>%
  rbind(LE_data %>%
          dplyr::filter(age >= 80 & sppb == "Good Physical Function"))

LE_data_mod <- LE_data %>%
  filter(sppb == "Poor Physical Function") %>%
  rbind(good_data)

LE_data_mod <- LE_data_mod %>%
  group_by(sppb, age) %>%
  mean_qi() %>% as.data.frame()


total_sppb <- LE_data_mod %>%
  select(sppb, age, LE, `.lower`, `.upper`) %>%
  filter(sppb == "Good PF") %>%
  cbind(LE_data_mod %>%
          select(sppb, LE, `.lower`, `.upper`) %>%
          filter(sppb == "Poor PF") %>%
          rename(LE.1 = LE,
                 lower1 = .lower,
                 upper1 = .upper) %>%
          select(LE.1, upper1, lower1)) %>%
  dplyr::mutate(diff = LE - LE.1,
                lower = .lower - lower1,
                upper = .upper - upper1) %>%
  ggplot(aes(x = age, y = diff)) +
  geom_pointrange(aes(ymin = lower,
                         ymax = upper)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  labs(x = "Age",
       y = "Average Life Expectancy Gain (years)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(-5, 10, 1))

le_plot <- LE_data %>%
  ggplot(aes(x = age, y = LE)) +
  stat_pointinterval() +
  facet_wrap(~sppb) +
  labs(x = "Age",
       y = "Dementia-free Life Expectancy (years)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 20, 2))

le_plot <- LE_data_mod %>%
  dplyr::mutate(sppb = ifelse(sppb == "Good PF", "Good Physical Function", "Poor Physical Function")) %>%
  ggplot(aes(x = age, y = LE)) +
  stat_pointinterval() +
  facet_wrap(~sppb) +
  labs(x = "Age",
       y = "Life Expectancy (years)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 25, 2))

# total LE differences
total_smoke <- LE_diff %>%
  group_by(age) %>%
  mean_qi() %>% 
  as.data.frame() %>%
  ggplot(aes(x = age, y = LE)) +
  geom_pointrange(aes(ymin = .lower,
                      ymax = .upper)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  labs(x = element_blank(),
       y = element_blank()) +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(-5, 8, 1))

# dementia-free LE differences
gain_plot <- LE_diff %>%
  group_by(age) %>%
  mean_qi() %>% 
  as.data.frame() %>%
  ggplot(aes(x = age, y = LE)) +
  geom_pointrange(aes(ymin = .lower,
                      ymax = .upper)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  labs(x = "Age",
       y = "Average Life Expectancy Gain (years)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 8, 0.5))

#arrange
tiff("total_pf.tiff", height = 8, width = 8, res = 300, units = "in")
ggarrange(le_plot, total_sppb,
          ncol = 1,
          labels = c("A", "B"))
dev.off()

# table
table <- LE_data_mod %>%
  select(sppb, age, LE, `.lower`, `.upper`) %>%
  filter(sppb == "Good PF") %>%
  cbind(LE_data_mod %>%
          select(sppb, LE, `.lower`, `.upper`) %>%
          filter(sppb == "Poor PF") %>%
          rename(LE.1 = LE,
                 lower1 = .lower,
                 upper1 = .upper) %>%
          select(LE.1, upper1, lower1)) %>%
  dplyr::mutate(diff = LE - LE.1,
                lower = .lower - lower1,
                upper = .upper - upper1) %>%
  dplyr::mutate(outcome = "SPPB") %>%
  dplyr::select(outcome, age, diff, lower, upper) %>%
  mutate_if(is.numeric, round, 2)


### table for covariates values
table <- LE_diff %>%
  group_by(age) %>%
  mean_qi() %>% 
  as.data.frame() %>%
  dplyr::select(age, LE, .lower, .upper) %>%
  mutate_if(is.numeric, round, 2)

table <- LE_data_mod %>%
  select(sppb, age, LE, `.lower`, `.upper`) %>%
  filter(sppb == "Good PF") %>%
  cbind(LE_data_mod %>%
          select(sppb, LE, `.lower`, `.upper`) %>%
          filter(sppb == "Poor PF") %>%
          rename(LE.1 = LE,
                 lower1 = .lower,
                 upper1 = .upper) %>%
          select(LE.1, upper1, lower1)) %>%
  dplyr::mutate(diff = LE - LE.1,
                lower = .lower - lower1,
                upper = .upper - upper1) %>%
  as.data.frame() %>%
  dplyr::select(age, diff, lower, upper) %>%
  mutate_if(is.numeric, round, 2)

write.table(table, file = "total.txt", 
            sep = ",", quote = FALSE, row.names = F)


LE_new <- LE_data %>%
  group_by(sppb, age) %>%
  mean_qi() %>% as.data.frame()

free_sppb <- LE_new %>%
  select(sppb, age, LE, `.lower`, `.upper`) %>%
  filter(sppb == "Good PF") %>%
  cbind(LE_new %>%
          select(sppb, LE, `.lower`, `.upper`) %>%
          filter(sppb == "Poor PF") %>%
          rename(LE.1 = LE,
                 lower1 = .lower,
                 upper1 = .upper) %>%
          select(LE.1, upper1, lower1)) %>%
  dplyr::mutate(diff = LE - LE.1,
                lower = .lower - lower1,
                upper = .upper - upper1) %>%
  ggplot(aes(x = age, y = diff)) +
  geom_pointrange(aes(ymin = lower,
                         ymax = upper)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  labs(x = "Age",
       y = "Expected time gain in dementia-free state (years)") +
  theme_bw() +
  scale_x_continuous(breaks = seq(65, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 5, 0.5))


# table
free <- LE_new %>%
  select(sppb, age, LE, `.lower`, `.upper`) %>%
  filter(sppb == "Good PF") %>%
  cbind(LE_new %>%
          select(sppb, LE, `.lower`, `.upper`) %>%
          filter(sppb == "Poor PF") %>%
          rename(LE.1 = LE,
                 lower1 = .lower,
                 upper1 = .upper) %>%
          select(LE.1, upper1, lower1)) %>%
  dplyr::mutate(diff = LE - LE.1,
                lower = .lower - lower1,
                upper = .upper - upper1) %>%
  dplyr::mutate(outcome = "SPPB") %>%
  dplyr::select(outcome, age, diff, lower, upper) %>%
  mutate_if(is.numeric, round, 2)

write.table(free, file = "free.txt", 
            sep = ",", quote = FALSE, row.names = F)


### table for covariates values
table <- LE_diff_free %>%
  group_by(age) %>%
  mean_qi() %>% 
  as.data.frame() %>%
  dplyr::select(age, LE, .lower, .upper) %>%
  mutate_if(is.numeric, round, 2)

write.table(table, file = "free.txt", 
            sep = ",", quote = FALSE, row.names = F)



### ggarrange
# removing axis names
free_exercise <- free_exercise +
  labs(y = "",
       x = "")
library(ggpubr)
# create the panel plot
figure_free <- ggarrange(free_sppb, free_sex, free_exercise,
                    free_income, free_smoke,
          labels = c("A", "B", "C",
                     "D", "E"),
          ncol = 3,
          nrow = 2)

# annotate figure
tiff("figure_free.tiff", height = 10, width = 14, res = 300, units = "in")
annotate_figure(figure_free, 
                left = text_grob("Average Life Expectancy Gain in a dementia-free state (years)",
                                 size = 15,
                                 rot = 90),
                bottom = text_grob("Age",
                                   size = 16.5))
dev.off()



# plot for Life Expectancies
LE_data %>%
  ggplot(aes(x = age, y = LE, col = sppb)) +
  stat_pointinterval(position = position_dodge(width = 1)) +
  labs(x = "Age",
       y = "Life Expectancy estimates",
       col = "Physical functioning\nby SPPB score") +
  scale_color_lancet(labels = c("Good", "Poor")) +
  theme_bw() +
  theme(legend.position = "bottom")


LE_data %>%
  ggplot(aes(x = age, y = LE, col = sppb)) +
  stat_pointinterval() +
  geom_smooth(method = "lm",
              formula = y ~ splines::ns(x, 3)) +
  labs(x = "Age",
       y = "Life Expectancy estimates",
       col = "Physical functioning\nby SPPB score") +
  scale_color_lancet(labels = c("Good", "Poor")) +
  theme_bw() +
  theme(legend.position = "bottom")

tiff("free_smoke.tiff", width = 8, height = 6, res = 300, units = "in")
LE_data %>%
  ggplot(aes(x = age, y = LE)) +
  stat_pointinterval() +
  facet_wrap(~ sppb) +
  labs(x = "Age",
       y = "Expected time spent in free-dementia state") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_x_continuous(breaks = seq(65, 105, 5))
dev.off()

# Life expectancy estimates for older adults with good physical performance
mod_summaries_good <- list()

for (i in seq(-14, 22, 1)) {
  mod_summaries_good[[i+15]] <- elect(model.elect.1, 
                                 b.covariates = list(age = i,
                                                     sex = 0,
                                                     sppb_10 = 1),
                                 statedistdata = sddata, 
                                 time.scale.msm = "years",
                                 h = 0.5, 
                                 age.max = 32, 
                                 S = 50)
}


# LEs for high-income male older adults with and without an associated chronic condition
mod_summaries_high_no <- list()

for (i in seq(-14, 22, 1)) {
  mod_summaries_high_no[[i+15]] <- elect(model.elect.1, 
                                      b.covariates = list(age = i,
                                                          sex = 1,
                                                          sppb_10 = 0),
                                      statedistdata = sddata, 
                                      time.scale.msm = "years",
                                      h = 0.5, 
                                      age.max = 32, 
                                      S = 50)
}

mod_summaries_high_yes <- list()

for (i in seq(-14, 22, 1)) {
  mod_summaries_high_yes[[i+15]] <- elect(model.elect.1, 
                                      b.covariates = list(age = i,
                                                          sex = 1,
                                                          sppb_10 = 1),
                                      statedistdata = sddata, 
                                      time.scale.msm = "years",
                                      h = 0.5, 
                                      age.max = 32, 
                                      S = 50)
}


# Estimated state-specific life expectancies (LEs) in the output below are e11, e12, e13, e21,...
# Where eij is LE in state j for an individual who is in state i at age X years
# Marginal LE ej is expected years in state j when state at age X is not taken into account explicitly
# Total LE at age X is e
summary.elect(mod_summaries[[9]], digits = 2, probs = c(.025, .975)) # as example we observe the life expectancy estimates at age 75

summary.elect(mod_summaries_good[[9]], digits = 2, probs = c(.025, .975))

# Total LEs 
LE.data <- data.frame(LE = c(mod_summaries[[1]]$sim[, 13],
                  mod_summaries[[2]]$sim[, 13],
                  mod_summaries[[3]]$sim[, 13],
                  mod_summaries[[4]]$sim[, 13],
                  mod_summaries[[5]]$sim[, 13],
                  mod_summaries[[6]]$sim[, 13],
                  mod_summaries[[7]]$sim[, 13],
                  mod_summaries[[8]]$sim[, 13],
                  mod_summaries[[9]]$sim[, 13],
                  mod_summaries[[10]]$sim[, 13],
                  mod_summaries[[11]]$sim[, 13],
                  mod_summaries[[12]]$sim[, 13],
                  mod_summaries[[13]]$sim[, 13],
                  mod_summaries[[14]]$sim[, 13],
                  mod_summaries[[15]]$sim[, 13],
                  mod_summaries[[16]]$sim[, 13],
                  mod_summaries[[17]]$sim[, 13],
                  mod_summaries[[18]]$sim[, 13],
                  mod_summaries[[19]]$sim[, 13],
                  mod_summaries[[20]]$sim[, 13],
                  mod_summaries[[21]]$sim[, 13],
                  mod_summaries[[22]]$sim[, 13],
                  mod_summaries[[23]]$sim[, 13],
                  mod_summaries[[24]]$sim[, 13],
                  mod_summaries[[25]]$sim[, 13],
                  mod_summaries[[26]]$sim[, 13],
                  mod_summaries[[27]]$sim[, 13],
                  mod_summaries[[28]]$sim[, 13],
                  mod_summaries[[29]]$sim[, 13],
                  mod_summaries[[30]]$sim[, 13],
                  mod_summaries[[31]]$sim[, 13],
                  mod_summaries[[32]]$sim[, 13],
                  mod_summaries[[33]]$sim[, 13],
                  mod_summaries[[34]]$sim[, 13],
                  mod_summaries[[35]]$sim[, 13],
                  mod_summaries[[36]]$sim[, 13],
                  mod_summaries[[37]]$sim[, 13],
                  mod_summaries_good[[1]]$sim[, 13],
                  mod_summaries_good[[2]]$sim[, 13],
                  mod_summaries_good[[3]]$sim[, 13],
                  mod_summaries_good[[4]]$sim[, 13],
                  mod_summaries_good[[5]]$sim[, 13],
                  mod_summaries_good[[6]]$sim[, 13],
                  mod_summaries_good[[7]]$sim[, 13],
                  mod_summaries_good[[8]]$sim[, 13],
                  mod_summaries_good[[9]]$sim[, 13],
                  mod_summaries_good[[10]]$sim[, 13],
                  mod_summaries_good[[11]]$sim[, 13],
                  mod_summaries_good[[12]]$sim[, 13],
                  mod_summaries_good[[13]]$sim[, 13],
                  mod_summaries_good[[14]]$sim[, 13],
                  mod_summaries_good[[15]]$sim[, 13],
                  mod_summaries_good[[16]]$sim[, 13],
                  mod_summaries_good[[17]]$sim[, 13],
                  mod_summaries_good[[18]]$sim[, 13],
                  mod_summaries_good[[19]]$sim[, 13],
                  mod_summaries_good[[20]]$sim[, 13],
                  mod_summaries_good[[21]]$sim[, 13],
                  mod_summaries_good[[22]]$sim[, 13],
                  mod_summaries_good[[23]]$sim[, 13],
                  mod_summaries_good[[24]]$sim[, 13],
                  mod_summaries_good[[25]]$sim[, 13],
                  mod_summaries_good[[26]]$sim[, 13],
                  mod_summaries_good[[27]]$sim[, 13],
                  mod_summaries_good[[28]]$sim[, 13],
                  mod_summaries_good[[29]]$sim[, 13],
                  mod_summaries_good[[30]]$sim[, 13],
                  mod_summaries_good[[31]]$sim[, 13],
                  mod_summaries_good[[32]]$sim[, 13],
                  mod_summaries_good[[33]]$sim[, 13],
                  mod_summaries_good[[34]]$sim[, 13],
                  mod_summaries_good[[35]]$sim[, 13],
                  mod_summaries_good[[36]]$sim[, 13],
                  mod_summaries_good[[37]]$sim[, 13],
                  mod_summaries_high_no[[1]]$sim[, 13],
                  mod_summaries_high_no[[2]]$sim[, 13],
                  mod_summaries_high_no[[3]]$sim[, 13],
                  mod_summaries_high_no[[4]]$sim[, 13],
                  mod_summaries_high_no[[5]]$sim[, 13],
                  mod_summaries_high_no[[6]]$sim[, 13],
                  mod_summaries_high_no[[7]]$sim[, 13],
                  mod_summaries_high_no[[8]]$sim[, 13],
                  mod_summaries_high_no[[9]]$sim[, 13],
                  mod_summaries_high_no[[10]]$sim[, 13],
                  mod_summaries_high_no[[11]]$sim[, 13],
                  mod_summaries_high_no[[12]]$sim[, 13],
                  mod_summaries_high_no[[13]]$sim[, 13],
                  mod_summaries_high_no[[14]]$sim[, 13],
                  mod_summaries_high_no[[15]]$sim[, 13],
                  mod_summaries_high_no[[16]]$sim[, 13],
                  mod_summaries_high_no[[17]]$sim[, 13],
                  mod_summaries_high_no[[18]]$sim[, 13],
                  mod_summaries_high_no[[19]]$sim[, 13],
                  mod_summaries_high_no[[20]]$sim[, 13],
                  mod_summaries_high_no[[21]]$sim[, 13],
                  mod_summaries_high_no[[22]]$sim[, 13],
                  mod_summaries_high_no[[23]]$sim[, 13],
                  mod_summaries_high_no[[24]]$sim[, 13],
                  mod_summaries_high_no[[25]]$sim[, 13],
                  mod_summaries_high_no[[26]]$sim[, 13],
                  mod_summaries_high_no[[27]]$sim[, 13],
                  mod_summaries_high_no[[28]]$sim[, 13],
                  mod_summaries_high_no[[29]]$sim[, 13],
                  mod_summaries_high_no[[30]]$sim[, 13],
                  mod_summaries_high_no[[31]]$sim[, 13],
                  mod_summaries_high_no[[32]]$sim[, 13],
                  mod_summaries_high_no[[33]]$sim[, 13],
                  mod_summaries_high_no[[34]]$sim[, 13],
                  mod_summaries_high_no[[35]]$sim[, 13],
                  mod_summaries_high_no[[36]]$sim[, 13],
                  mod_summaries_high_no[[37]]$sim[, 13],
                  mod_summaries_high_yes[[1]]$sim[, 13],
                  mod_summaries_high_yes[[2]]$sim[, 13],
                  mod_summaries_high_yes[[3]]$sim[, 13],
                  mod_summaries_high_yes[[4]]$sim[, 13],
                  mod_summaries_high_yes[[5]]$sim[, 13],
                  mod_summaries_high_yes[[6]]$sim[, 13],
                  mod_summaries_high_yes[[7]]$sim[, 13],
                  mod_summaries_high_yes[[8]]$sim[, 13],
                  mod_summaries_high_yes[[9]]$sim[, 13],
                  mod_summaries_high_yes[[10]]$sim[, 13],
                  mod_summaries_high_yes[[11]]$sim[, 13],
                  mod_summaries_high_yes[[12]]$sim[, 13],
                  mod_summaries_high_yes[[13]]$sim[, 13],
                  mod_summaries_high_yes[[14]]$sim[, 13],
                  mod_summaries_high_yes[[15]]$sim[, 13],
                  mod_summaries_high_yes[[16]]$sim[, 13],
                  mod_summaries_high_yes[[17]]$sim[, 13],
                  mod_summaries_high_yes[[18]]$sim[, 13],
                  mod_summaries_high_yes[[19]]$sim[, 13],
                  mod_summaries_high_yes[[20]]$sim[, 13],
                  mod_summaries_high_yes[[21]]$sim[, 13],
                  mod_summaries_high_yes[[22]]$sim[, 13],
                  mod_summaries_high_yes[[23]]$sim[, 13],
                  mod_summaries_high_yes[[24]]$sim[, 13],
                  mod_summaries_high_yes[[25]]$sim[, 13],
                  mod_summaries_high_yes[[26]]$sim[, 13],
                  mod_summaries_high_yes[[27]]$sim[, 13],
                  mod_summaries_high_yes[[28]]$sim[, 13],
                  mod_summaries_high_yes[[29]]$sim[, 13],
                  mod_summaries_high_yes[[30]]$sim[, 13],
                  mod_summaries_high_yes[[31]]$sim[, 13],
                  mod_summaries_high_yes[[32]]$sim[, 13],
                  mod_summaries_high_yes[[33]]$sim[, 13],
                  mod_summaries_high_yes[[34]]$sim[, 13],
                  mod_summaries_high_yes[[35]]$sim[, 13],
                  mod_summaries_high_yes[[36]]$sim[, 13],
                  mod_summaries_high_yes[[37]]$sim[, 13]),
           age = rep(seq(65, 101, 1), each = 10, times = 4),
           sex = rep(c("Males", "Females"), each = 740),
           sppb = rep(c("Poor Physical Function",
                        "Good Physical Function"), each = 370, times = 2)) 

ids <- unique(plm_exam_clean$id)

plm_exam_clean %>% 
  dplyr::filter(id %in% ids) %>%
  dplyr::filter(!duplicated(id)) %>%
  .$sex %>% table()
 
### plot!
tiff("life_exp.tiff", height = 6, width = 9, res = 300, units = "in")
LE.data %>%
  ggplot(aes(x = age, y = LE)) +
  geom_pointinterval(aes(y = LE,
                         ymin = .lower,
                         ymax = .upper), 
                     data = LE.data %>% 
                       group_by(sex, age, sppb) %>% 
                       mean_qi(LE) %>% 
                       as.data.frame() %>%
                       mutate(.lower = ifelse(.lower < 0, 0, .lower)), # dataset with difference summaries
                     size = 1, alpha = 0.5) +
  geom_smooth() +
  labs(x = "Age",
       y = "Life Expectancy",
       title = "Overall Life Expectancy Estimates") +
  facet_grid(vars(sex), vars(sppb)) +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light()
dev.off()

# Marginal LEs
# Expected years in state j when state at age X is not taken into account explicitly

# Dementia-free older adults
LE.data.dementia.free <- data.frame(LE = c(mod_summaries[[1]]$sim[, 10],
                             mod_summaries[[2]]$sim[, 10],
                             mod_summaries[[3]]$sim[, 10],
                             mod_summaries[[4]]$sim[, 10],
                             mod_summaries[[5]]$sim[, 10],
                             mod_summaries[[6]]$sim[, 10],
                             mod_summaries[[7]]$sim[, 10],
                             mod_summaries[[8]]$sim[, 10],
                             mod_summaries[[9]]$sim[, 10],
                             mod_summaries[[10]]$sim[, 10],
                             mod_summaries[[11]]$sim[, 10],
                             mod_summaries[[12]]$sim[, 10],
                             mod_summaries[[13]]$sim[, 10],
                             mod_summaries[[14]]$sim[, 10],
                             mod_summaries[[15]]$sim[, 10],
                             mod_summaries[[16]]$sim[, 10],
                             mod_summaries[[17]]$sim[, 10],
                             mod_summaries[[18]]$sim[, 10],
                             mod_summaries[[19]]$sim[, 10],
                             mod_summaries[[20]]$sim[, 10],
                             mod_summaries[[21]]$sim[, 10],
                             mod_summaries[[22]]$sim[, 10],
                             mod_summaries[[23]]$sim[, 10],
                             mod_summaries[[24]]$sim[, 10],
                             mod_summaries[[25]]$sim[, 10],
                             mod_summaries[[26]]$sim[, 10],
                             mod_summaries[[27]]$sim[, 10],
                             mod_summaries[[28]]$sim[, 10],
                             mod_summaries[[29]]$sim[, 10],
                             mod_summaries[[30]]$sim[, 10],
                             mod_summaries[[31]]$sim[, 10],
                             mod_summaries[[32]]$sim[, 10],
                             mod_summaries[[33]]$sim[, 10],
                             mod_summaries[[34]]$sim[, 10],
                             mod_summaries[[35]]$sim[, 10],
                             mod_summaries[[36]]$sim[, 10],
                             mod_summaries[[37]]$sim[, 10],
                             mod_summaries_good[[1]]$sim[, 10],
                             mod_summaries_good[[2]]$sim[, 10],
                             mod_summaries_good[[3]]$sim[, 10],
                             mod_summaries_good[[4]]$sim[, 10],
                             mod_summaries_good[[5]]$sim[, 10],
                             mod_summaries_good[[6]]$sim[, 10],
                             mod_summaries_good[[7]]$sim[, 10],
                             mod_summaries_good[[8]]$sim[, 10],
                             mod_summaries_good[[9]]$sim[, 10],
                             mod_summaries_good[[10]]$sim[, 10],
                             mod_summaries_good[[11]]$sim[, 10],
                             mod_summaries_good[[12]]$sim[, 10],
                             mod_summaries_good[[13]]$sim[, 10],
                             mod_summaries_good[[14]]$sim[, 10],
                             mod_summaries_good[[15]]$sim[, 10],
                             mod_summaries_good[[16]]$sim[, 10],
                             mod_summaries_good[[17]]$sim[, 10],
                             mod_summaries_good[[18]]$sim[, 10],
                             mod_summaries_good[[19]]$sim[, 10],
                             mod_summaries_good[[20]]$sim[, 10],
                             mod_summaries_good[[21]]$sim[, 10],
                             mod_summaries_good[[22]]$sim[, 10],
                             mod_summaries_good[[23]]$sim[, 10],
                             mod_summaries_good[[24]]$sim[, 10],
                             mod_summaries_good[[25]]$sim[, 10],
                             mod_summaries_good[[26]]$sim[, 10],
                             mod_summaries_good[[27]]$sim[, 10],
                             mod_summaries_good[[28]]$sim[, 10],
                             mod_summaries_good[[29]]$sim[, 10],
                             mod_summaries_good[[30]]$sim[, 10],
                             mod_summaries_good[[31]]$sim[, 10],
                             mod_summaries_good[[32]]$sim[, 10],
                             mod_summaries_good[[33]]$sim[, 10],
                             mod_summaries_good[[34]]$sim[, 10],
                             mod_summaries_good[[35]]$sim[, 10],
                             mod_summaries_good[[36]]$sim[, 10],
                             mod_summaries_good[[37]]$sim[, 10],
                             mod_summaries_high_no[[1]]$sim[, 10],
                             mod_summaries_high_no[[2]]$sim[, 10],
                             mod_summaries_high_no[[3]]$sim[, 10],
                             mod_summaries_high_no[[4]]$sim[, 10],
                             mod_summaries_high_no[[5]]$sim[, 10],
                             mod_summaries_high_no[[6]]$sim[, 10],
                             mod_summaries_high_no[[7]]$sim[, 10],
                             mod_summaries_high_no[[8]]$sim[, 10],
                             mod_summaries_high_no[[9]]$sim[, 10],
                             mod_summaries_high_no[[10]]$sim[, 10],
                             mod_summaries_high_no[[11]]$sim[, 10],
                             mod_summaries_high_no[[12]]$sim[, 10],
                             mod_summaries_high_no[[13]]$sim[, 10],
                             mod_summaries_high_no[[14]]$sim[, 10],
                             mod_summaries_high_no[[15]]$sim[, 10],
                             mod_summaries_high_no[[16]]$sim[, 10],
                             mod_summaries_high_no[[17]]$sim[, 10],
                             mod_summaries_high_no[[18]]$sim[, 10],
                             mod_summaries_high_no[[19]]$sim[, 10],
                             mod_summaries_high_no[[20]]$sim[, 10],
                             mod_summaries_high_no[[21]]$sim[, 10],
                             mod_summaries_high_no[[22]]$sim[, 10],
                             mod_summaries_high_no[[23]]$sim[, 10],
                             mod_summaries_high_no[[24]]$sim[, 10],
                             mod_summaries_high_no[[25]]$sim[, 10],
                             mod_summaries_high_no[[26]]$sim[, 10],
                             mod_summaries_high_no[[27]]$sim[, 10],
                             mod_summaries_high_no[[28]]$sim[, 10],
                             mod_summaries_high_no[[29]]$sim[, 10],
                             mod_summaries_high_no[[30]]$sim[, 10],
                             mod_summaries_high_no[[31]]$sim[, 10],
                             mod_summaries_high_no[[32]]$sim[, 10],
                             mod_summaries_high_no[[33]]$sim[, 10],
                             mod_summaries_high_no[[34]]$sim[, 10],
                             mod_summaries_high_no[[35]]$sim[, 10],
                             mod_summaries_high_no[[36]]$sim[, 10],
                             mod_summaries_high_no[[37]]$sim[, 10],
                             mod_summaries_high_yes[[1]]$sim[, 10],
                             mod_summaries_high_yes[[2]]$sim[, 10],
                             mod_summaries_high_yes[[3]]$sim[, 10],
                             mod_summaries_high_yes[[4]]$sim[, 10],
                             mod_summaries_high_yes[[5]]$sim[, 10],
                             mod_summaries_high_yes[[6]]$sim[, 10],
                             mod_summaries_high_yes[[7]]$sim[, 10],
                             mod_summaries_high_yes[[8]]$sim[, 10],
                             mod_summaries_high_yes[[9]]$sim[, 10],
                             mod_summaries_high_yes[[10]]$sim[, 10],
                             mod_summaries_high_yes[[11]]$sim[, 10],
                             mod_summaries_high_yes[[12]]$sim[, 10],
                             mod_summaries_high_yes[[13]]$sim[, 10],
                             mod_summaries_high_yes[[14]]$sim[, 10],
                             mod_summaries_high_yes[[15]]$sim[, 10],
                             mod_summaries_high_yes[[16]]$sim[, 10],
                             mod_summaries_high_yes[[17]]$sim[, 10],
                             mod_summaries_high_yes[[18]]$sim[, 10],
                             mod_summaries_high_yes[[19]]$sim[, 10],
                             mod_summaries_high_yes[[20]]$sim[, 10],
                             mod_summaries_high_yes[[21]]$sim[, 10],
                             mod_summaries_high_yes[[22]]$sim[, 10],
                             mod_summaries_high_yes[[23]]$sim[, 10],
                             mod_summaries_high_yes[[24]]$sim[, 10],
                             mod_summaries_high_yes[[25]]$sim[, 10],
                             mod_summaries_high_yes[[26]]$sim[, 10],
                             mod_summaries_high_yes[[27]]$sim[, 10],
                             mod_summaries_high_yes[[28]]$sim[, 10],
                             mod_summaries_high_yes[[29]]$sim[, 10],
                             mod_summaries_high_yes[[30]]$sim[, 10],
                             mod_summaries_high_yes[[31]]$sim[, 10],
                             mod_summaries_high_yes[[32]]$sim[, 10],
                             mod_summaries_high_yes[[33]]$sim[, 10],
                             mod_summaries_high_yes[[34]]$sim[, 10],
                             mod_summaries_high_yes[[35]]$sim[, 10],
                             mod_summaries_high_yes[[36]]$sim[, 10],
                             mod_summaries_high_yes[[37]]$sim[, 10]),
                             age = rep(seq(65, 101, 1), each = 10, times = 4),
                             sex = rep(c("Males", "Females"), each = 740),
                             sppb = rep(c("Poor Physical Function",
                                          "Good Physical Function"), each = 370, times = 2))  


### plot!
tiff("marg_life_exp.tiff", height = 6, width = 9, res = 300, units = "in")
LE.data.dementia.free %>%
  ggplot(aes(x = age, y = LE)) +
  geom_pointinterval(aes(y = LE,
                         ymin = .lower,
                         ymax = .upper), 
                     data = LE.data.dementia.free %>% 
                       group_by(sex, age, sppb) %>% 
                       mean_qi(LE) %>% 
                       as.data.frame() %>%
                       mutate(.lower = ifelse(.lower < 0, 0, .lower)), # dataset with difference summaries
                     size = 1, alpha = 0.5) +
  geom_smooth() +
  labs(x = "Age",
       y = "Life Expectancy",
       title = "Marginal Life Expectancy in free-dementia state") +
  facet_grid(vars(sex), vars(sppb)) +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light()
dev.off()


# Older adults classified as possible dementia
LE.data.dementia.2 <- data.frame(LE = c(mod_summaries[[1]]$sim[, 11],
                                           mod_summaries[[2]]$sim[, 11],
                                           mod_summaries[[3]]$sim[, 11],
                                           mod_summaries[[4]]$sim[, 11],
                                           mod_summaries[[5]]$sim[, 11],
                                           mod_summaries[[6]]$sim[, 11],
                                           mod_summaries[[7]]$sim[, 11],
                                           mod_summaries[[8]]$sim[, 11],
                                           mod_summaries[[9]]$sim[, 11],
                                           mod_summaries[[10]]$sim[, 11],
                                           mod_summaries[[11]]$sim[, 11],
                                           mod_summaries[[12]]$sim[, 11],
                                           mod_summaries[[13]]$sim[, 11],
                                           mod_summaries[[14]]$sim[, 11],
                                           mod_summaries[[15]]$sim[, 11],
                                           mod_summaries[[16]]$sim[, 11],
                                           mod_summaries[[17]]$sim[, 11],
                                           mod_summaries[[18]]$sim[, 11],
                                           mod_summaries[[19]]$sim[, 11],
                                           mod_summaries[[20]]$sim[, 11],
                                           mod_summaries[[21]]$sim[, 11],
                                           mod_summaries[[22]]$sim[, 11],
                                           mod_summaries[[23]]$sim[, 11],
                                           mod_summaries[[24]]$sim[, 11],
                                           mod_summaries[[25]]$sim[, 11],
                                           mod_summaries[[26]]$sim[, 11],
                                           mod_summaries[[27]]$sim[, 11],
                                           mod_summaries[[28]]$sim[, 11],
                                           mod_summaries[[29]]$sim[, 11],
                                           mod_summaries[[30]]$sim[, 11],
                                           mod_summaries[[31]]$sim[, 11],
                                           mod_summaries[[32]]$sim[, 11],
                                           mod_summaries[[33]]$sim[, 11],
                                           mod_summaries[[34]]$sim[, 11],
                                           mod_summaries[[35]]$sim[, 11],
                                           mod_summaries_good[[1]]$sim[, 11],
                                           mod_summaries_good[[2]]$sim[, 11],
                                           mod_summaries_good[[3]]$sim[, 11],
                                           mod_summaries_good[[4]]$sim[, 11],
                                           mod_summaries_good[[5]]$sim[, 11],
                                           mod_summaries_good[[6]]$sim[, 11],
                                           mod_summaries_good[[7]]$sim[, 11],
                                           mod_summaries_good[[8]]$sim[, 11],
                                           mod_summaries_good[[9]]$sim[, 11],
                                           mod_summaries_good[[10]]$sim[, 11],
                                           mod_summaries_good[[11]]$sim[, 11],
                                           mod_summaries_good[[12]]$sim[, 11],
                                           mod_summaries_good[[13]]$sim[, 11],
                                           mod_summaries_good[[14]]$sim[, 11],
                                           mod_summaries_good[[15]]$sim[, 11],
                                           mod_summaries_good[[16]]$sim[, 11],
                                           mod_summaries_good[[17]]$sim[, 11],
                                           mod_summaries_good[[18]]$sim[, 11],
                                           mod_summaries_good[[19]]$sim[, 11],
                                           mod_summaries_good[[20]]$sim[, 11],
                                           mod_summaries_good[[21]]$sim[, 11],
                                           mod_summaries_good[[22]]$sim[, 11],
                                           mod_summaries_good[[23]]$sim[, 11],
                                           mod_summaries_good[[24]]$sim[, 11],
                                           mod_summaries_good[[25]]$sim[, 11],
                                           mod_summaries_good[[26]]$sim[, 11],
                                           mod_summaries_good[[27]]$sim[, 11],
                                           mod_summaries_good[[28]]$sim[, 11],
                                           mod_summaries_good[[29]]$sim[, 11],
                                           mod_summaries_good[[30]]$sim[, 11],
                                           mod_summaries_good[[31]]$sim[, 11],
                                           mod_summaries_good[[32]]$sim[, 11],
                                           mod_summaries_good[[33]]$sim[, 11],
                                           mod_summaries_good[[34]]$sim[, 11],
                                           mod_summaries_good[[35]]$sim[, 11]),
                                    age = rep(seq(65, 99, 1), each = 10, times = 2),
                                    type = rep(c("Poor", "Good"), each = 350)) 

### plot!
LE.data.dementia.2 %>%
  ggplot(aes(x = age, y = LE)) +
  geom_smooth(aes(col = type)) +
  labs(x = "Age",
       y = "Life Expectancy",
       col = "Physical performance") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 0.5)) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_lancet()


# Older adults classified as probable dementia
LE.data.dementia.3 <- data.frame(LE = c(mod_summaries[[1]]$sim[, 12],
                                        mod_summaries[[2]]$sim[, 12],
                                        mod_summaries[[3]]$sim[, 12],
                                        mod_summaries[[4]]$sim[, 12],
                                        mod_summaries[[5]]$sim[, 12],
                                        mod_summaries[[6]]$sim[, 12],
                                        mod_summaries[[7]]$sim[, 12],
                                        mod_summaries[[8]]$sim[, 12],
                                        mod_summaries[[9]]$sim[, 12],
                                        mod_summaries[[10]]$sim[, 12],
                                        mod_summaries[[11]]$sim[, 12],
                                        mod_summaries[[12]]$sim[, 12],
                                        mod_summaries[[13]]$sim[, 12],
                                        mod_summaries[[14]]$sim[, 12],
                                        mod_summaries[[15]]$sim[, 12],
                                        mod_summaries[[16]]$sim[, 12],
                                        mod_summaries[[17]]$sim[, 12],
                                        mod_summaries[[18]]$sim[, 12],
                                        mod_summaries[[19]]$sim[, 12],
                                        mod_summaries[[20]]$sim[, 12],
                                        mod_summaries[[21]]$sim[, 12],
                                        mod_summaries[[22]]$sim[, 12],
                                        mod_summaries[[23]]$sim[, 12],
                                        mod_summaries[[24]]$sim[, 12],
                                        mod_summaries[[25]]$sim[, 12],
                                        mod_summaries[[26]]$sim[, 12],
                                        mod_summaries[[27]]$sim[, 12],
                                        mod_summaries[[28]]$sim[, 12],
                                        mod_summaries[[29]]$sim[, 12],
                                        mod_summaries[[30]]$sim[, 12],
                                        mod_summaries[[31]]$sim[, 12],
                                        mod_summaries[[32]]$sim[, 12],
                                        mod_summaries[[33]]$sim[, 12],
                                        mod_summaries[[34]]$sim[, 12],
                                        mod_summaries[[35]]$sim[, 12],
                                        mod_summaries_good[[1]]$sim[, 12],
                                        mod_summaries_good[[2]]$sim[, 12],
                                        mod_summaries_good[[3]]$sim[, 12],
                                        mod_summaries_good[[4]]$sim[, 12],
                                        mod_summaries_good[[5]]$sim[, 12],
                                        mod_summaries_good[[6]]$sim[, 12],
                                        mod_summaries_good[[7]]$sim[, 12],
                                        mod_summaries_good[[8]]$sim[, 12],
                                        mod_summaries_good[[9]]$sim[, 12],
                                        mod_summaries_good[[10]]$sim[, 12],
                                        mod_summaries_good[[11]]$sim[, 12],
                                        mod_summaries_good[[12]]$sim[, 12],
                                        mod_summaries_good[[13]]$sim[, 12],
                                        mod_summaries_good[[14]]$sim[, 12],
                                        mod_summaries_good[[15]]$sim[, 12],
                                        mod_summaries_good[[16]]$sim[, 12],
                                        mod_summaries_good[[17]]$sim[, 12],
                                        mod_summaries_good[[18]]$sim[, 12],
                                        mod_summaries_good[[19]]$sim[, 12],
                                        mod_summaries_good[[20]]$sim[, 12],
                                        mod_summaries_good[[21]]$sim[, 12],
                                        mod_summaries_good[[22]]$sim[, 12],
                                        mod_summaries_good[[23]]$sim[, 12],
                                        mod_summaries_good[[24]]$sim[, 12],
                                        mod_summaries_good[[25]]$sim[, 12],
                                        mod_summaries_good[[26]]$sim[, 12],
                                        mod_summaries_good[[27]]$sim[, 12],
                                        mod_summaries_good[[28]]$sim[, 12],
                                        mod_summaries_good[[29]]$sim[, 12],
                                        mod_summaries_good[[30]]$sim[, 12],
                                        mod_summaries_good[[31]]$sim[, 12],
                                        mod_summaries_good[[32]]$sim[, 12],
                                        mod_summaries_good[[33]]$sim[, 12],
                                        mod_summaries_good[[34]]$sim[, 12],
                                        mod_summaries_good[[35]]$sim[, 12]),
                                 age = rep(seq(65, 99, 1), each = 10, times = 2),
                                 type = rep(c("Poor", "Good"), each = 350)) 

### plot!
LE.data.dementia.3 %>%
  ggplot(aes(x = age, y = LE)) +
  geom_smooth(aes(col = type)) +
  labs(x = "Age",
       y = "Life Expectancy",
       col = "Physical performance") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 0.5)) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_lancet()


# Estimated state-specific life expectancies (LEs) 
# Where eij is LE in state j for an individual who is in state i at age X years

# Life expectancy in dementia-free state in older adults who are in dementia-free state at different ages
LE.data.1.1 <- data.frame(LE = c(mod_summaries[[1]]$sim[, 1],
                                        mod_summaries[[2]]$sim[, 1],
                                        mod_summaries[[3]]$sim[, 1],
                                        mod_summaries[[4]]$sim[, 1],
                                        mod_summaries[[5]]$sim[, 1],
                                        mod_summaries[[6]]$sim[, 1],
                                        mod_summaries[[7]]$sim[, 1],
                                        mod_summaries[[8]]$sim[, 1],
                                        mod_summaries[[9]]$sim[, 1],
                                        mod_summaries[[10]]$sim[, 1],
                                        mod_summaries[[11]]$sim[, 1],
                                        mod_summaries[[12]]$sim[, 1],
                                        mod_summaries[[13]]$sim[, 1],
                                        mod_summaries[[14]]$sim[, 1],
                                        mod_summaries[[15]]$sim[, 1],
                                        mod_summaries[[16]]$sim[, 1],
                                        mod_summaries[[17]]$sim[, 1],
                                        mod_summaries[[18]]$sim[, 1],
                                        mod_summaries[[19]]$sim[, 1],
                                        mod_summaries[[20]]$sim[, 1],
                                        mod_summaries[[21]]$sim[, 1],
                                        mod_summaries[[22]]$sim[, 1],
                                        mod_summaries[[23]]$sim[, 1],
                                        mod_summaries[[24]]$sim[, 1],
                                        mod_summaries[[25]]$sim[, 1],
                                        mod_summaries[[26]]$sim[, 1],
                                        mod_summaries[[27]]$sim[, 1],
                                        mod_summaries[[28]]$sim[, 1],
                                        mod_summaries[[29]]$sim[, 1],
                                        mod_summaries[[30]]$sim[, 1],
                                        mod_summaries[[31]]$sim[, 1],
                                        mod_summaries[[32]]$sim[, 1],
                                        mod_summaries[[33]]$sim[, 1],
                                        mod_summaries[[34]]$sim[, 1],
                                        mod_summaries[[35]]$sim[, 1],
                                        mod_summaries_good[[1]]$sim[, 1],
                                        mod_summaries_good[[2]]$sim[, 1],
                                        mod_summaries_good[[3]]$sim[, 1],
                                        mod_summaries_good[[4]]$sim[, 1],
                                        mod_summaries_good[[5]]$sim[, 1],
                                        mod_summaries_good[[6]]$sim[, 1],
                                        mod_summaries_good[[7]]$sim[, 1],
                                        mod_summaries_good[[8]]$sim[, 1],
                                        mod_summaries_good[[9]]$sim[, 1],
                                        mod_summaries_good[[10]]$sim[, 1],
                                        mod_summaries_good[[11]]$sim[, 1],
                                        mod_summaries_good[[12]]$sim[, 1],
                                        mod_summaries_good[[13]]$sim[, 1],
                                        mod_summaries_good[[14]]$sim[, 1],
                                        mod_summaries_good[[15]]$sim[, 1],
                                        mod_summaries_good[[16]]$sim[, 1],
                                        mod_summaries_good[[17]]$sim[, 1],
                                        mod_summaries_good[[18]]$sim[, 1],
                                        mod_summaries_good[[19]]$sim[, 1],
                                        mod_summaries_good[[20]]$sim[, 1],
                                        mod_summaries_good[[21]]$sim[, 1],
                                        mod_summaries_good[[22]]$sim[, 1],
                                        mod_summaries_good[[23]]$sim[, 1],
                                        mod_summaries_good[[24]]$sim[, 1],
                                        mod_summaries_good[[25]]$sim[, 1],
                                        mod_summaries_good[[26]]$sim[, 1],
                                        mod_summaries_good[[27]]$sim[, 1],
                                        mod_summaries_good[[28]]$sim[, 1],
                                        mod_summaries_good[[29]]$sim[, 1],
                                        mod_summaries_good[[30]]$sim[, 1],
                                        mod_summaries_good[[31]]$sim[, 1],
                                        mod_summaries_good[[32]]$sim[, 1],
                                        mod_summaries_good[[33]]$sim[, 1],
                                        mod_summaries_good[[34]]$sim[, 1],
                                        mod_summaries_good[[35]]$sim[, 1]),
                                 age = rep(seq(65, 99, 1), each = 10, times = 2),
                                 type = rep(c("Poor", "Good"), each = 350)) 

### plot!
LE.data.1.1 %>%
  ggplot(aes(x = age, y = LE)) +
  geom_smooth(aes(col = type)) +
  labs(x = "Age",
       y = "Life Expectancy",
       col = "Physical performance") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_lancet()

mod_summaries[[1]]
### Difference in Life Expectancy estimates

# difference between poor and good physical performance
LE.data.diff <- data.frame(diff = c(mod_summaries_good[[1]]$sim[, 13] - mod_summaries[[1]]$sim[, 13],
                               mod_summaries_good[[2]]$sim[, 13] - mod_summaries[[2]]$sim[, 13],
                               mod_summaries_good[[3]]$sim[, 13] - mod_summaries[[3]]$sim[, 13],
                               mod_summaries_good[[4]]$sim[, 13] - mod_summaries[[4]]$sim[, 13],
                               mod_summaries_good[[5]]$sim[, 13] - mod_summaries[[5]]$sim[, 13],
                               mod_summaries_good[[6]]$sim[, 13] - mod_summaries[[6]]$sim[, 13],
                               mod_summaries_good[[7]]$sim[, 13] - mod_summaries[[7]]$sim[, 13],
                               mod_summaries_good[[8]]$sim[, 13] - mod_summaries[[8]]$sim[, 13],
                               mod_summaries_good[[9]]$sim[, 13] - mod_summaries[[9]]$sim[, 13],
                               mod_summaries_good[[10]]$sim[, 13] - mod_summaries[[10]]$sim[, 13],
                               mod_summaries_good[[11]]$sim[, 13] - mod_summaries[[11]]$sim[, 13],
                               mod_summaries_good[[12]]$sim[, 13] - mod_summaries[[12]]$sim[, 13],
                               mod_summaries_good[[13]]$sim[, 13] - mod_summaries[[13]]$sim[, 13],
                               mod_summaries_good[[14]]$sim[, 13] - mod_summaries[[14]]$sim[, 13],
                               mod_summaries_good[[15]]$sim[, 13] - mod_summaries[[15]]$sim[, 13],
                               mod_summaries_good[[16]]$sim[, 13] - mod_summaries[[16]]$sim[, 13],
                               mod_summaries_good[[17]]$sim[, 13] - mod_summaries[[17]]$sim[, 13],
                               mod_summaries_good[[18]]$sim[, 13] - mod_summaries[[18]]$sim[, 13],
                               mod_summaries_good[[19]]$sim[, 13] - mod_summaries[[19]]$sim[, 13],
                               mod_summaries_good[[20]]$sim[, 13] - mod_summaries[[20]]$sim[, 13],
                               mod_summaries_good[[21]]$sim[, 13] - mod_summaries[[21]]$sim[, 13],
                               mod_summaries_good[[22]]$sim[, 13] - mod_summaries[[22]]$sim[, 13],
                               mod_summaries_good[[23]]$sim[, 13] - mod_summaries[[23]]$sim[, 13],
                               mod_summaries_good[[24]]$sim[, 13] - mod_summaries[[24]]$sim[, 13],
                               mod_summaries_good[[25]]$sim[, 13] - mod_summaries[[25]]$sim[, 13],
                               mod_summaries_good[[26]]$sim[, 13] - mod_summaries[[26]]$sim[, 13],
                               mod_summaries_good[[27]]$sim[, 13] - mod_summaries[[27]]$sim[, 13],
                               mod_summaries_good[[28]]$sim[, 13] - mod_summaries[[28]]$sim[, 13],
                               mod_summaries_good[[29]]$sim[, 13] - mod_summaries[[29]]$sim[, 13],
                               mod_summaries_good[[30]]$sim[, 13] - mod_summaries[[30]]$sim[, 13],
                               mod_summaries_good[[31]]$sim[, 13] - mod_summaries[[31]]$sim[, 13],
                               mod_summaries_good[[32]]$sim[, 13] - mod_summaries[[32]]$sim[, 13],
                               mod_summaries_good[[33]]$sim[, 13] - mod_summaries[[33]]$sim[, 13],
                               mod_summaries_good[[34]]$sim[, 13] - mod_summaries[[34]]$sim[, 13],
                               mod_summaries_good[[35]]$sim[, 13] - mod_summaries[[35]]$sim[, 13],
                               mod_summaries_good[[36]]$sim[, 13] - mod_summaries[[36]]$sim[, 13],
                               mod_summaries_good[[37]]$sim[, 13] - mod_summaries[[37]]$sim[, 13],
                               mod_summaries_high_yes[[1]]$sim[, 13] - mod_summaries_high_no[[1]]$sim[, 13],
                               mod_summaries_high_yes[[2]]$sim[, 13] - mod_summaries_high_no[[2]]$sim[, 13],
                               mod_summaries_high_yes[[3]]$sim[, 13] - mod_summaries_high_no[[3]]$sim[, 13],
                               mod_summaries_high_yes[[4]]$sim[, 13] - mod_summaries_high_no[[4]]$sim[, 13],
                               mod_summaries_high_yes[[5]]$sim[, 13] - mod_summaries_high_no[[5]]$sim[, 13],
                               mod_summaries_high_yes[[6]]$sim[, 13] - mod_summaries_high_no[[6]]$sim[, 13],
                               mod_summaries_high_yes[[7]]$sim[, 13] - mod_summaries_high_no[[7]]$sim[, 13],
                               mod_summaries_high_yes[[8]]$sim[, 13] - mod_summaries_high_no[[8]]$sim[, 13],
                               mod_summaries_high_yes[[9]]$sim[, 13] - mod_summaries_high_no[[9]]$sim[, 13],
                               mod_summaries_high_yes[[10]]$sim[, 13] - mod_summaries_high_no[[10]]$sim[, 13],
                               mod_summaries_high_yes[[11]]$sim[, 13] - mod_summaries_high_no[[11]]$sim[, 13],
                               mod_summaries_high_yes[[12]]$sim[, 13] - mod_summaries_high_no[[12]]$sim[, 13],
                               mod_summaries_high_yes[[13]]$sim[, 13] - mod_summaries_high_no[[13]]$sim[, 13],
                               mod_summaries_high_yes[[14]]$sim[, 13] - mod_summaries_high_no[[14]]$sim[, 13],
                               mod_summaries_high_yes[[15]]$sim[, 13] - mod_summaries_high_no[[15]]$sim[, 13],
                               mod_summaries_high_yes[[16]]$sim[, 13] - mod_summaries_high_no[[16]]$sim[, 13],
                               mod_summaries_high_yes[[17]]$sim[, 13] - mod_summaries_high_no[[17]]$sim[, 13],
                               mod_summaries_high_yes[[18]]$sim[, 13] - mod_summaries_high_no[[18]]$sim[, 13],
                               mod_summaries_high_yes[[19]]$sim[, 13] - mod_summaries_high_no[[19]]$sim[, 13],
                               mod_summaries_high_yes[[20]]$sim[, 13] - mod_summaries_high_no[[20]]$sim[, 13],
                               mod_summaries_high_yes[[21]]$sim[, 13] - mod_summaries_high_no[[21]]$sim[, 13],
                               mod_summaries_high_yes[[22]]$sim[, 13] - mod_summaries_high_no[[22]]$sim[, 13],
                               mod_summaries_high_yes[[23]]$sim[, 13] - mod_summaries_high_no[[23]]$sim[, 13],
                               mod_summaries_high_yes[[24]]$sim[, 13] - mod_summaries_high_no[[24]]$sim[, 13],
                               mod_summaries_high_yes[[25]]$sim[, 13] - mod_summaries_high_no[[25]]$sim[, 13],
                               mod_summaries_high_yes[[26]]$sim[, 13] - mod_summaries_high_no[[26]]$sim[, 13],
                               mod_summaries_high_yes[[27]]$sim[, 13] - mod_summaries_high_no[[27]]$sim[, 13],
                               mod_summaries_high_yes[[28]]$sim[, 13] - mod_summaries_high_no[[28]]$sim[, 13],
                               mod_summaries_high_yes[[29]]$sim[, 13] - mod_summaries_high_no[[29]]$sim[, 13],
                               mod_summaries_high_yes[[30]]$sim[, 13] - mod_summaries_high_no[[30]]$sim[, 13],
                               mod_summaries_high_yes[[31]]$sim[, 13] - mod_summaries_high_no[[31]]$sim[, 13],
                               mod_summaries_high_yes[[32]]$sim[, 13] - mod_summaries_high_no[[32]]$sim[, 13],
                               mod_summaries_high_yes[[33]]$sim[, 13] - mod_summaries_high_no[[33]]$sim[, 13],
                               mod_summaries_high_yes[[34]]$sim[, 13] - mod_summaries_high_no[[34]]$sim[, 13],
                               mod_summaries_high_yes[[35]]$sim[, 13] - mod_summaries_high_no[[35]]$sim[, 13],
                               mod_summaries_high_yes[[36]]$sim[, 13] - mod_summaries_high_no[[36]]$sim[, 13],
                               mod_summaries_high_yes[[37]]$sim[, 13] - mod_summaries_high_no[[37]]$sim[, 13]),
                      age = rep(seq(65, 101, 1), each = 10, times = 2),
                      sex = rep(c("Males (Good vs. poor physical function)",
                                  "Females (Good vs. poor physical function)"),
                                each = 370)) 


### plot!
library(tidybayes)

### point interval geometry
tiff("diff_life_exp.tiff", height = 6, width = 9, res = 300, units = "in")
LE.data.diff %>%
  ggplot(aes(x = age, y = diff)) +
  geom_pointinterval(aes(y = diff,
                         ymin = .lower,
                         ymax = .upper), 
                     data = LE.data.diff %>% 
                       group_by(sex, age) %>% 
                       mean_qi(diff) %>% 
                       as.data.frame() %>%
                       mutate(.lower = ifelse(.lower < 0, 0, .lower)), # dataset with difference summaries
                     size = 1, alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~ sex) +
  labs(x = "Age",
       y = "Expected Life Expectancy Difference (Years)") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light()
dev.off()

# free-dementia LEs when someone is in the free-dementia state (state 1)
LE.data.diff <- data.frame(diff = c(mod_summaries_good[[1]]$sim[, 1] - mod_summaries[[1]]$sim[, 1],
                                    mod_summaries_good[[2]]$sim[, 1] - mod_summaries[[2]]$sim[, 1],
                                    mod_summaries_good[[3]]$sim[, 1] - mod_summaries[[3]]$sim[, 1],
                                    mod_summaries_good[[4]]$sim[, 1] - mod_summaries[[4]]$sim[, 1],
                                    mod_summaries_good[[5]]$sim[, 1] - mod_summaries[[5]]$sim[, 1],
                                    mod_summaries_good[[6]]$sim[, 1] - mod_summaries[[6]]$sim[, 1],
                                    mod_summaries_good[[7]]$sim[, 1] - mod_summaries[[7]]$sim[, 1],
                                    mod_summaries_good[[8]]$sim[, 1] - mod_summaries[[8]]$sim[, 1],
                                    mod_summaries_good[[9]]$sim[, 1] - mod_summaries[[9]]$sim[, 1],
                                    mod_summaries_good[[10]]$sim[, 1] - mod_summaries[[10]]$sim[, 1],
                                    mod_summaries_good[[11]]$sim[, 1] - mod_summaries[[11]]$sim[, 1],
                                    mod_summaries_good[[12]]$sim[, 1] - mod_summaries[[12]]$sim[, 1],
                                    mod_summaries_good[[13]]$sim[, 1] - mod_summaries[[13]]$sim[, 1],
                                    mod_summaries_good[[14]]$sim[, 1] - mod_summaries[[14]]$sim[, 1],
                                    mod_summaries_good[[15]]$sim[, 1] - mod_summaries[[15]]$sim[, 1],
                                    mod_summaries_good[[16]]$sim[, 1] - mod_summaries[[16]]$sim[, 1],
                                    mod_summaries_good[[17]]$sim[, 1] - mod_summaries[[17]]$sim[, 1],
                                    mod_summaries_good[[18]]$sim[, 1] - mod_summaries[[18]]$sim[, 1],
                                    mod_summaries_good[[19]]$sim[, 1] - mod_summaries[[19]]$sim[, 1],
                                    mod_summaries_good[[20]]$sim[, 1] - mod_summaries[[20]]$sim[, 1],
                                    mod_summaries_good[[21]]$sim[, 1] - mod_summaries[[21]]$sim[, 1],
                                    mod_summaries_good[[22]]$sim[, 1] - mod_summaries[[22]]$sim[, 1],
                                    mod_summaries_good[[23]]$sim[, 1] - mod_summaries[[23]]$sim[, 1],
                                    mod_summaries_good[[24]]$sim[, 1] - mod_summaries[[24]]$sim[, 1],
                                    mod_summaries_good[[25]]$sim[, 1] - mod_summaries[[25]]$sim[, 1],
                                    mod_summaries_good[[26]]$sim[, 1] - mod_summaries[[26]]$sim[, 1],
                                    mod_summaries_good[[27]]$sim[, 1] - mod_summaries[[27]]$sim[, 1],
                                    mod_summaries_good[[28]]$sim[, 1] - mod_summaries[[28]]$sim[, 1],
                                    mod_summaries_good[[29]]$sim[, 1] - mod_summaries[[29]]$sim[, 1],
                                    mod_summaries_good[[30]]$sim[, 1] - mod_summaries[[30]]$sim[, 1],
                                    mod_summaries_good[[31]]$sim[, 1] - mod_summaries[[31]]$sim[, 1],
                                    mod_summaries_good[[32]]$sim[, 1] - mod_summaries[[32]]$sim[, 1],
                                    mod_summaries_good[[33]]$sim[, 1] - mod_summaries[[33]]$sim[, 1],
                                    mod_summaries_good[[34]]$sim[, 1] - mod_summaries[[34]]$sim[, 1],
                                    mod_summaries_good[[35]]$sim[, 1] - mod_summaries[[35]]$sim[, 1],
                                    mod_summaries_good[[36]]$sim[, 1] - mod_summaries[[36]]$sim[, 1],
                                    mod_summaries_good[[37]]$sim[, 1] - mod_summaries[[37]]$sim[, 1],
                                    mod_summaries_high_yes[[1]]$sim[, 1] - mod_summaries_high_no[[1]]$sim[, 1],
                                    mod_summaries_high_yes[[2]]$sim[, 1] - mod_summaries_high_no[[2]]$sim[, 1],
                                    mod_summaries_high_yes[[3]]$sim[, 1] - mod_summaries_high_no[[3]]$sim[, 1],
                                    mod_summaries_high_yes[[4]]$sim[, 1] - mod_summaries_high_no[[4]]$sim[, 1],
                                    mod_summaries_high_yes[[5]]$sim[, 1] - mod_summaries_high_no[[5]]$sim[, 1],
                                    mod_summaries_high_yes[[6]]$sim[, 1] - mod_summaries_high_no[[6]]$sim[, 1],
                                    mod_summaries_high_yes[[7]]$sim[, 1] - mod_summaries_high_no[[7]]$sim[, 1],
                                    mod_summaries_high_yes[[8]]$sim[, 1] - mod_summaries_high_no[[8]]$sim[, 1],
                                    mod_summaries_high_yes[[9]]$sim[, 1] - mod_summaries_high_no[[9]]$sim[, 1],
                                    mod_summaries_high_yes[[10]]$sim[, 1] - mod_summaries_high_no[[10]]$sim[, 1],
                                    mod_summaries_high_yes[[11]]$sim[, 1] - mod_summaries_high_no[[11]]$sim[, 1],
                                    mod_summaries_high_yes[[12]]$sim[, 1] - mod_summaries_high_no[[12]]$sim[, 1],
                                    mod_summaries_high_yes[[13]]$sim[, 1] - mod_summaries_high_no[[13]]$sim[, 1],
                                    mod_summaries_high_yes[[14]]$sim[, 1] - mod_summaries_high_no[[14]]$sim[, 1],
                                    mod_summaries_high_yes[[15]]$sim[, 1] - mod_summaries_high_no[[15]]$sim[, 1],
                                    mod_summaries_high_yes[[16]]$sim[, 1] - mod_summaries_high_no[[16]]$sim[, 1],
                                    mod_summaries_high_yes[[17]]$sim[, 1] - mod_summaries_high_no[[17]]$sim[, 1],
                                    mod_summaries_high_yes[[18]]$sim[, 1] - mod_summaries_high_no[[18]]$sim[, 1],
                                    mod_summaries_high_yes[[19]]$sim[, 1] - mod_summaries_high_no[[19]]$sim[, 1],
                                    mod_summaries_high_yes[[20]]$sim[, 1] - mod_summaries_high_no[[20]]$sim[, 1],
                                    mod_summaries_high_yes[[21]]$sim[, 1] - mod_summaries_high_no[[21]]$sim[, 1],
                                    mod_summaries_high_yes[[22]]$sim[, 1] - mod_summaries_high_no[[22]]$sim[, 1],
                                    mod_summaries_high_yes[[23]]$sim[, 1] - mod_summaries_high_no[[23]]$sim[, 1],
                                    mod_summaries_high_yes[[24]]$sim[, 1] - mod_summaries_high_no[[24]]$sim[, 1],
                                    mod_summaries_high_yes[[25]]$sim[, 1] - mod_summaries_high_no[[25]]$sim[, 1],
                                    mod_summaries_high_yes[[26]]$sim[, 1] - mod_summaries_high_no[[26]]$sim[, 1],
                                    mod_summaries_high_yes[[27]]$sim[, 1] - mod_summaries_high_no[[27]]$sim[, 1],
                                    mod_summaries_high_yes[[28]]$sim[, 1] - mod_summaries_high_no[[28]]$sim[, 1],
                                    mod_summaries_high_yes[[29]]$sim[, 1] - mod_summaries_high_no[[29]]$sim[, 1],
                                    mod_summaries_high_yes[[30]]$sim[, 1] - mod_summaries_high_no[[30]]$sim[, 1],
                                    mod_summaries_high_yes[[31]]$sim[, 1] - mod_summaries_high_no[[31]]$sim[, 1],
                                    mod_summaries_high_yes[[32]]$sim[, 1] - mod_summaries_high_no[[32]]$sim[, 1],
                                    mod_summaries_high_yes[[33]]$sim[, 1] - mod_summaries_high_no[[33]]$sim[, 1],
                                    mod_summaries_high_yes[[34]]$sim[, 1] - mod_summaries_high_no[[34]]$sim[, 1],
                                    mod_summaries_high_yes[[35]]$sim[, 1] - mod_summaries_high_no[[35]]$sim[, 1],
                                    mod_summaries_high_yes[[36]]$sim[, 1] - mod_summaries_high_no[[36]]$sim[, 1],
                                    mod_summaries_high_yes[[37]]$sim[, 1] - mod_summaries_high_no[[37]]$sim[, 1]),
                           age = rep(seq(65, 101, 1), each = 50, times = 2),
                           sex = rep(c("Males (Good vs. poor physical function)",
                                       "Females (Good vs. poor physical function)"),
                                     each = 1850)) 


### plot!
library(tidybayes)

### point interval geometry
tiff("diff_free_life_exp_4bs.tiff", height = 6, width = 9, res = 300, units = "in")
LE.data.diff %>%
  ggplot(aes(x = age, y = diff)) +
  geom_pointinterval(aes(y = diff,
                         ymin = .lower,
                         ymax = .upper), 
                     data = LE.data.diff %>% 
                       group_by(sex, age) %>% 
                       mean_qi(diff) %>% 
                       as.data.frame() %>%
                       mutate(.lower = ifelse(.lower < 0, 0, .lower)), # dataset with difference summaries
                     size = 1, alpha = 0.5) +
  geom_smooth(formula = y ~ bs(x, 4)) +
  facet_wrap(~ sex) +
  labs(x = "Age",
       y = "Expected Life Expectancy Difference (Years)") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light()
dev.off()


### point interval geometry
LE.data.diff %>%
  ggplot(aes(x = age, y = diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  labs(x = "Age",
       y = "Expected Life Expectancy Difference (Years)") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_lancet()

### Difference in Marginal Life Expectancy estimates

# difference in expected years in dementia-free state when state at age X is not taken into account explicitly
LE.data.df.diff <- data.frame(diff = c(mod_summaries_good[[1]]$sim[, 10] - mod_summaries[[2]]$sim[, 10],
                                    mod_summaries_good[[2]]$sim[, 10] - mod_summaries[[2]]$sim[, 10],
                                    mod_summaries_good[[3]]$sim[, 10] - mod_summaries[[3]]$sim[, 10],
                                    mod_summaries_good[[4]]$sim[, 10] - mod_summaries[[4]]$sim[, 10],
                                    mod_summaries_good[[5]]$sim[, 10] - mod_summaries[[5]]$sim[, 10],
                                    mod_summaries_good[[6]]$sim[, 10] - mod_summaries[[6]]$sim[, 10],
                                    mod_summaries_good[[7]]$sim[, 10] - mod_summaries[[7]]$sim[, 10],
                                    mod_summaries_good[[8]]$sim[, 10] - mod_summaries[[8]]$sim[, 10],
                                    mod_summaries_good[[9]]$sim[, 10] - mod_summaries[[9]]$sim[, 10],
                                    mod_summaries_good[[10]]$sim[, 10] - mod_summaries[[10]]$sim[, 10],
                                    mod_summaries_good[[11]]$sim[, 10] - mod_summaries[[11]]$sim[, 10],
                                    mod_summaries_good[[12]]$sim[, 10] - mod_summaries[[12]]$sim[, 10],
                                    mod_summaries_good[[13]]$sim[, 10] - mod_summaries[[13]]$sim[, 10],
                                    mod_summaries_good[[14]]$sim[, 10] - mod_summaries[[14]]$sim[, 10],
                                    mod_summaries_good[[15]]$sim[, 10] - mod_summaries[[15]]$sim[, 10],
                                    mod_summaries_good[[16]]$sim[, 10] - mod_summaries[[16]]$sim[, 10],
                                    mod_summaries_good[[17]]$sim[, 10] - mod_summaries[[17]]$sim[, 10],
                                    mod_summaries_good[[18]]$sim[, 10] - mod_summaries[[18]]$sim[, 10],
                                    mod_summaries_good[[19]]$sim[, 10] - mod_summaries[[19]]$sim[, 10],
                                    mod_summaries_good[[20]]$sim[, 10] - mod_summaries[[20]]$sim[, 10],
                                    mod_summaries_good[[21]]$sim[, 10] - mod_summaries[[21]]$sim[, 10],
                                    mod_summaries_good[[22]]$sim[, 10] - mod_summaries[[22]]$sim[, 10],
                                    mod_summaries_good[[23]]$sim[, 10] - mod_summaries[[23]]$sim[, 10],
                                    mod_summaries_good[[24]]$sim[, 10] - mod_summaries[[24]]$sim[, 10],
                                    mod_summaries_good[[25]]$sim[, 10] - mod_summaries[[25]]$sim[, 10],
                                    mod_summaries_good[[26]]$sim[, 10] - mod_summaries[[26]]$sim[, 10],
                                    mod_summaries_good[[27]]$sim[, 10] - mod_summaries[[27]]$sim[, 10],
                                    mod_summaries_good[[28]]$sim[, 10] - mod_summaries[[28]]$sim[, 10],
                                    mod_summaries_good[[29]]$sim[, 10] - mod_summaries[[29]]$sim[, 10],
                                    mod_summaries_good[[30]]$sim[, 10] - mod_summaries[[30]]$sim[, 10],
                                    mod_summaries_good[[31]]$sim[, 10] - mod_summaries[[31]]$sim[, 10],
                                    mod_summaries_good[[32]]$sim[, 10] - mod_summaries[[32]]$sim[, 10],
                                    mod_summaries_good[[33]]$sim[, 10] - mod_summaries[[33]]$sim[, 10],
                                    mod_summaries_good[[34]]$sim[, 10] - mod_summaries[[34]]$sim[, 10],
                                    mod_summaries_good[[35]]$sim[, 10] - mod_summaries[[35]]$sim[, 10]),
                           age = rep(seq(65, 99, 1), each = 10, times = 2)) 


### plot!
LE.data.df.diff %>%
  ggplot(aes(x = age, y = diff)) +
  geom_pointinterval(aes(y = diff,
                         ymin = .lower,
                         ymax = .upper), 
                     data = LE.data.diff %>% 
                       group_by(age) %>% 
                       mean_qi(diff) %>% 
                       as.data.frame() %>%
                       mutate(.lower = ifelse(.lower < 0, 0, .lower)), # dataset with difference summaries
                     size = 1) +
  geom_smooth() +
  labs(x = "Age",
       y = "Life Expectancy Difference in dementia-free state",
       col = "Physical performance") +
  scale_x_continuous(breaks = seq(65, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_color_lancet()




###### Fitting models with SPPB as continuous outcome

# Fitting the model
model.elect.1 <- msm(state ~ age, 
                     subject = id, 
                     data = plm_exam_elect,
                     covariates = list("1-2" = ~sppb), # specifying that the effect of this covariate occurs in this 1-2 state transition
                     qmatrix = Q, 
                     deathexact = 4, 
                     control = list(fnscale = 100000,
                                    maxit = 1000,
                                    trace = 1,
                                    REPORT = 1),
                     center = FALSE,
                     death = TRUE)

# Comparing model fits: model with binary covariate vs. continuous covariate
data.frame(Model = c("Model including SPPB as a binary outcome",
                     "Model including SPPB as a continuous outcome"), # worse model fit
           AIC = c(AIC(model.elect), AIC(model.elect.1)))


# Subsets including data on different SPPB scores
# SPPB score = 3
data.3 <- unique(plm_exam$id[plm_exam$sppb==3])

# SPPB score = 6
data.6 <- unique(plm_exam$id[plm_exam$sppb==6])

# SPPB score = 10
data.10 <- unique(plm_exam$id[plm_exam$sppb==10])


# Prevalence analysis in older adults with different SPPB scores
prev.3 <- prevalence.msm(model.elect.1, covariates = list(sppb = 3), subset = data.3)
prev.6 <- prevalence.msm(model.elect.1, covariates = list(sppb = 6), subset = data.6)
prev.10 <- prevalence.msm(model.elect.1, covariates = list(sppb = 10), subset = data.10)

# reshape observed prevalence
## SPPB score = 3
do1 <-as_tibble(row.names(prev.3$Observed)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
do2 <-as_tibble(prev.3$Observed) %>% mutate(type = "observed")
do <- cbind(do1,do2) %>% dplyr::select(-Total)
do_3 <- do %>% gather(state, number, -time, -type)

## SPPB score = 6
do1 <-as_tibble(row.names(prev.6$Observed)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
do2 <-as_tibble(prev.6$Observed) %>% mutate(type = "observed")
do <- cbind(do1,do2) %>% dplyr::select(-Total)
do_6 <- do %>% gather(state, number, -time, -type)

## SPPB score = 10
do1 <-as_tibble(row.names(prev.10$Observed)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
do2 <-as_tibble(prev.10$Observed) %>% mutate(type = "observed")
do <- cbind(do1,do2) %>% dplyr::select(-Total)
do_10 <- do %>% gather(state, number, -time, -type)


# reshape expected prevalence
de1 <-as_tibble(row.names(prev$Expected)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
de2 <-as_tibble(prev$Expected) %>% mutate(type = "expected")
de <- cbind(de1,de2) %>% dplyr::select(-Total) 
de_l <- de %>% gather(state, number, -time, -type) 

# bind into a single data frame
prev.obs <- rbind(do_3, do_6, do_10) %>% mutate(type = factor(type),
                                                state = factor(state),
                                                sppb = rep(c(3, 6, 10), each = 44),
                                                time = round(time,3))
# plot for comparison
prev.obs %>%
  mutate(sppb = factor(sppb)) %>%
  group_by(state) %>%
  ggplot(aes(time, number)) +
  geom_line(aes(col = sppb)) +
  facet_wrap(~state) +
  labs(x = "Years",
       y = "Number of older adults at risk",
       title = "Empirical evidence",
       col = "SPPB score") + 
  theme_light() +
  theme(legend.position = "bottom")


# reshape expected prevalence
# SPPB score = 3
de1 <-as_tibble(row.names(prev.3$Expected)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
de2 <-as_tibble(prev.3$Expected) %>% mutate(type = "expected")
de <- cbind(de1,de2) %>% dplyr::select(-Total) 
de_3 <- de %>% gather(state, number, -time, -type) 

# SPPB score = 6
de1 <-as_tibble(row.names(prev.6$Expected)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
de2 <-as_tibble(prev.6$Expected) %>% mutate(type = "expected")
de <- cbind(de1,de2) %>% dplyr::select(-Total) 
de_6 <- de %>% gather(state, number, -time, -type)

# SPPB score = 10
de1 <-as_tibble(row.names(prev.10$Expected)) %>% dplyr::rename(time = value) %>% 
  mutate(time = as.numeric(time))
de2 <-as_tibble(prev.10$Expected) %>% mutate(type = "expected")
de <- cbind(de1,de2) %>% dplyr::select(-Total) 
de_10 <- de %>% gather(state, number, -time, -type)


# bind into a single data frame
prev.exp <- rbind(de_3, de_6, de_10) %>% mutate(type = factor(type),
                                                state = factor(state),
                                                sppb = rep(c(3, 6, 10), each = 44),
                                                time = round(time,3))
# plot for comparison
prev.exp %>%
  mutate(sppb = factor(sppb)) %>%
  group_by(state) %>%
  ggplot(aes(time, number)) +
  geom_line(aes(col = sppb)) +
  facet_wrap(~state) +
  labs(x = "Years",
       y = "Number of older adults at risk",
       title = "Fitted evidence",
       col = "SPPB score") + 
  theme_light() +
  theme(legend.position = "bottom")





# Predictions for each 0.25-years increase
LE.1.poor <- elect(model.elect, 
      b.covariates = list(age = -12,
                          sppb_binary = 0),
      statedistdata = sddata, 
      time.scale.msm = "years",
      h = 0.5, 
      age.max = 32, 
      S = 10)

LE.2.poor <- elect(model.elect, 
                   b.covariates = list(age = -11.75,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.3.poor <- elect(model.elect, 
                   b.covariates = list(age = -11.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.4.poor <- elect(model.elect, 
                   b.covariates = list(age = -11.25,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.5.poor <- elect(model.elect, 
                   b.covariates = list(age = -11,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.6.poor <- elect(model.elect, 
                   b.covariates = list(age = -10.75,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.7.poor <- elect(model.elect, 
                   b.covariates = list(age = -10.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.8.poor <- elect(model.elect, 
                   b.covariates = list(age = -10.25,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.9.poor <- elect(model.elect, 
                   b.covariates = list(age = -10,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)



LE.10.poor <- elect(model.elect, 
                      b.covariates = list(age = -9.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.11.poor <- elect(model.elect, 
                     b.covariates = list(age = -9.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.12.poor <- elect(model.elect, 
                      b.covariates = list(age = -9.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.13.poor <- elect(model.elect, 
                   b.covariates = list(age = -9,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.14.poor <- elect(model.elect, 
                      b.covariates = list(age = -8.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.15.poor <- elect(model.elect, 
                     b.covariates = list(age = -8.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.16.poor <- elect(model.elect, 
                      b.covariates = list(age = -8.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.17.poor <- elect(model.elect, 
                   b.covariates = list(age = -8,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.18.poor <- elect(model.elect, 
                      b.covariates = list(age = -7.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)


LE.19.poor <- elect(model.elect, 
                     b.covariates = list(age = -7.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.20.poor <- elect(model.elect, 
                      b.covariates = list(age = -7.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.21.poor <- elect(model.elect, 
                   b.covariates = list(age = -7,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.22.poor <- elect(model.elect, 
                      b.covariates = list(age = -6.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.23.poor <- elect(model.elect, 
                     b.covariates = list(age = -6.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.24.poor <- elect(model.elect, 
                      b.covariates = list(age = -6.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.25.poor <- elect(model.elect, 
                   b.covariates = list(age = -6,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.26.poor <- elect(model.elect, 
                      b.covariates = list(age = -5.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.27.poor <- elect(model.elect, 
                     b.covariates = list(age = -5.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.28.poor <- elect(model.elect, 
                      b.covariates = list(age = -5.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.29.poor <- elect(model.elect, 
                   b.covariates = list(age = -5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.30.poor <- elect(model.elect, 
                      b.covariates = list(age = -4.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.31.poor <- elect(model.elect, 
                     b.covariates = list(age = -4.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.32.poor <- elect(model.elect, 
                      b.covariates = list(age = -4.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.33.poor <- elect(model.elect, 
                   b.covariates = list(age = -4,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.34.poor <- elect(model.elect, 
                      b.covariates = list(age = -3.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.35.poor <- elect(model.elect, 
                     b.covariates = list(age = -3.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.36.poor <- elect(model.elect, 
                      b.covariates = list(age = -3.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.37.poor <- elect(model.elect, 
                   b.covariates = list(age = -3,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.38.poor <- elect(model.elect, 
                      b.covariates = list(age = -2.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.39.poor <- elect(model.elect, 
                     b.covariates = list(age = -2.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.40.poor <- elect(model.elect, 
                      b.covariates = list(age = -2.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.41.poor <- elect(model.elect, 
                   b.covariates = list(age = -2,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.42.poor <- elect(model.elect, 
                      b.covariates = list(age = -1.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.43.poor <- elect(model.elect, 
                     b.covariates = list(age = -1.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.44.poor <- elect(model.elect, 
                      b.covariates = list(age = -1.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.45.poor <- elect(model.elect, 
                   b.covariates = list(age = -1,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.46.poor <- elect(model.elect, 
                      b.covariates = list(age = -0.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.47.poor <- elect(model.elect, 
                     b.covariates = list(age = -0.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.48.poor <- elect(model.elect, 
                      b.covariates = list(age = -0.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.49.poor <- elect(model.elect, 
                   b.covariates = list(age = 0,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.50.poor <- elect(model.elect, 
                      b.covariates = list(age = 0.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.51.poor <- elect(model.elect, 
                     b.covariates = list(age = .5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.52.poor <- elect(model.elect, 
                      b.covariates = list(age = 0.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.53.poor <- elect(model.elect, 
                   b.covariates = list(age = 1,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.54.poor <- elect(model.elect, 
                      b.covariates = list(age = 1.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.55.poor <- elect(model.elect, 
                     b.covariates = list(age = 1.5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.56.poor <- elect(model.elect, 
                      b.covariates = list(age = 1.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.57.poor <- elect(model.elect, 
                   b.covariates = list(age = 2,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.58.poor <- elect(model.elect, 
                      b.covariates = list(age = 2.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)


LE.59.poor <- elect(model.elect, 
                   b.covariates = list(age = 2.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.60.poor <- elect(model.elect, 
                      b.covariates = list(age = 2.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)


LE.61.poor <- elect(model.elect, 
                     b.covariates = list(age = 3,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.62.poor <- elect(model.elect, 
                      b.covariates = list(age = 3.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.63.poor <- elect(model.elect, 
                   b.covariates = list(age = 3.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.64.poor <- elect(model.elect, 
                      b.covariates = list(age = 3.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.65.poor <- elect(model.elect, 
                    b.covariates = list(age = 4,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.66.poor <- elect(model.elect, 
                    b.covariates = list(age = 4.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.67.poor <- elect(model.elect, 
                    b.covariates = list(age = 4.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.68.poor <- elect(model.elect, 
                    b.covariates = list(age = 4.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.69.poor <- elect(model.elect, 
                     b.covariates = list(age = 5,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.70.poor <- elect(model.elect, 
                      b.covariates = list(age = 5.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.71.poor <- elect(model.elect, 
                   b.covariates = list(age = 5.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.72.poor <- elect(model.elect, 
                      b.covariates = list(age = 5.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.73.poor <- elect(model.elect, 
                     b.covariates = list(age = 6,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.74.poor <- elect(model.elect, 
                      b.covariates = list(age = 6.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.75.poor <- elect(model.elect, 
                   b.covariates = list(age = 6.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.76.poor <- elect(model.elect, 
                      b.covariates = list(age = 6.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.77.poor <- elect(model.elect, 
                     b.covariates = list(age = 7,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.78.poor <- elect(model.elect, 
                      b.covariates = list(age = 7.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.79.poor <- elect(model.elect, 
                   b.covariates = list(age = 7.5,
                                       sppb_binary = 0),
                   statedistdata = sddata, 
                   time.scale.msm = "years",
                   h = 0.5, 
                   age.max = 32, 
                   S = 10)

LE.80.poor <- elect(model.elect, 
                      b.covariates = list(age = 7.75,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.81.poor <- elect(model.elect, 
                     b.covariates = list(age = 8,
                                         sppb_binary = 0),
                     statedistdata = sddata, 
                     time.scale.msm = "years",
                     h = 0.5, 
                     age.max = 32, 
                     S = 10)

LE.82.poor <- elect(model.elect, 
                      b.covariates = list(age = 8.25,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.83.poor <- elect(model.elect, 
                    b.covariates = list(age = 8.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.84.poor <- elect(model.elect, 
                       b.covariates = list(age = 8.75,
                                           sppb_binary = 0),
                       statedistdata = sddata, 
                       time.scale.msm = "years",
                       h = 0.5, 
                       age.max = 32, 
                       S = 10)

LE.85.poor <- elect(model.elect, 
                      b.covariates = list(age = 9,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.86.poor <- elect(model.elect, 
                       b.covariates = list(age = 9.25,
                                           sppb_binary = 0),
                       statedistdata = sddata, 
                       time.scale.msm = "years",
                       h = 0.5, 
                       age.max = 32, 
                       S = 10)

LE.87.poor <- elect(model.elect, 
                    b.covariates = list(age = 9.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.88.poor <- elect(model.elect, 
                       b.covariates = list(age = 9.75,
                                           sppb_binary = 0),
                       statedistdata = sddata, 
                       time.scale.msm = "years",
                       h = 0.5, 
                       age.max = 32, 
                       S = 10)

LE.89.poor <- elect(model.elect, 
                      b.covariates = list(age = 10,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.90.poor <- elect(model.elect, 
                       b.covariates = list(age = 10.25,
                                           sppb_binary = 0),
                       statedistdata = sddata, 
                       time.scale.msm = "years",
                       h = 0.5, 
                       age.max = 32, 
                       S = 10)

LE.91.poor <- elect(model.elect, 
                    b.covariates = list(age = 10.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.92.poor <- elect(model.elect, 
                       b.covariates = list(age = 10.75,
                                           sppb_binary = 0),
                       statedistdata = sddata, 
                       time.scale.msm = "years",
                       h = 0.5, 
                       age.max = 32, 
                       S = 10)

LE.93.poor <- elect(model.elect, 
                      b.covariates = list(age = 11,
                                          sppb_binary = 0),
                      statedistdata = sddata, 
                      time.scale.msm = "years",
                      h = 0.5, 
                      age.max = 32, 
                      S = 10)

LE.94.poor <- elect(model.elect, 
                    b.covariates = list(age = 11.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.95.poor <- elect(model.elect, 
                    b.covariates = list(age = 11.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.96.poor <- elect(model.elect, 
                    b.covariates = list(age = 11.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.97.poor <- elect(model.elect, 
                    b.covariates = list(age = 12,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.98.poor <- elect(model.elect, 
                    b.covariates = list(age = 12.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.99.poor <- elect(model.elect, 
                    b.covariates = list(age = 12.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.100.poor <- elect(model.elect, 
                    b.covariates = list(age = 12.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.101.poor <- elect(model.elect, 
                    b.covariates = list(age = 13,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.102.poor <- elect(model.elect, 
                    b.covariates = list(age = 13.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.103.poor <- elect(model.elect, 
                    b.covariates = list(age = 13.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.104.poor <- elect(model.elect, 
                    b.covariates = list(age = 13.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.105.poor <- elect(model.elect, 
                    b.covariates = list(age = 14,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.106.poor <- elect(model.elect, 
                    b.covariates = list(age = 14.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.107.poor <- elect(model.elect, 
                    b.covariates = list(age = 14.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.108.poor <- elect(model.elect, 
                    b.covariates = list(age = 14.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.109.poor <- elect(model.elect, 
                    b.covariates = list(age = 15,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.110.poor <- elect(model.elect, 
                    b.covariates = list(age = 15.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.111.poor <- elect(model.elect, 
                    b.covariates = list(age = 15.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.112.poor <- elect(model.elect, 
                    b.covariates = list(age = 15.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.113.poor <- elect(model.elect, 
                    b.covariates = list(age = 16,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.114.poor <- elect(model.elect, 
                    b.covariates = list(age = 16.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.115.poor <- elect(model.elect, 
                    b.covariates = list(age = 16.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.116.poor <- elect(model.elect, 
                    b.covariates = list(age = 16.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.117.poor <- elect(model.elect, 
                    b.covariates = list(age = 17,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.118.poor <- elect(model.elect, 
                    b.covariates = list(age = 17.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.119.poor <- elect(model.elect, 
                    b.covariates = list(age = 17.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.120.poor <- elect(model.elect, 
                    b.covariates = list(age = 17.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.121.poor <- elect(model.elect, 
                    b.covariates = list(age = 18,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.122.poor <- elect(model.elect, 
                    b.covariates = list(age = 18.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.123.poor <- elect(model.elect, 
                    b.covariates = list(age = 18.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)
# 
LE.124.poor <- elect(model.elect, 
                    b.covariates = list(age = 18.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)


LE.125.poor <- elect(model.elect, 
                    b.covariates = list(age = 19,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.126.poor <- elect(model.elect, 
                    b.covariates = list(age = 19.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.127.poor <- elect(model.elect, 
                    b.covariates = list(age = 19.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.128.poor <- elect(model.elect, 
                    b.covariates = list(age = 19.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.129.poor <- elect(model.elect, 
                    b.covariates = list(age = 20,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.130.poor <- elect(model.elect, 
                    b.covariates = list(age = 20.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.131.poor <- elect(model.elect, 
                    b.covariates = list(age = 20.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.132.poor <- elect(model.elect, 
                    b.covariates = list(age = 20.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.133.poor <- elect(model.elect, 
                    b.covariates = list(age = 21,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.134.poor <- elect(model.elect, 
                    b.covariates = list(age = 21.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.135.poor <- elect(model.elect, 
                    b.covariates = list(age = 21.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.136.poor <- elect(model.elect, 
                    b.covariates = list(age = 21.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)


LE.137.poor <- elect(model.elect, 
                    b.covariates = list(age = 22,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.138.poor <- elect(model.elect, 
                    b.covariates = list(age = 22.25,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.139.poor <- elect(model.elect, 
                    b.covariates = list(age = 22.5,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.140.poor <- elect(model.elect, 
                    b.covariates = list(age = 22.75,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)

LE.141.poor <- elect(model.elect, 
                    b.covariates = list(age = 23,
                                        sppb_binary = 0),
                    statedistdata = sddata, 
                    time.scale.msm = "years",
                    h = 0.5, 
                    age.max = 32, 
                    S = 10)


# dataset creation
LE.data.1 <- data.frame(LE = c(LE.1.poor$sim[, 13],
                               LE.1.poor$sim[, 13],
                               LE.3.poor$sim[, 13],
                               LE.4.poor$sim[, 13],
                               LE.5.poor$sim[, 13],
                               LE.6.poor$sim[, 13],
                               LE.7.poor$sim[, 13],
                               LE.8.poor$sim[, 13],
                               LE.9.poor$sim[, 13],
                               LE.10.poor$sim[, 13],
                               LE.11.poor$sim[, 13],
                               LE.12.poor$sim[, 13],
                               LE.13.poor$sim[, 13],
                               LE.14.poor$sim[, 13],
                               LE.15.poor$sim[, 13],
                               LE.16.poor$sim[, 13],
                               LE.17.poor$sim[, 13],
                               LE.18.poor$sim[, 13],
                               LE.19.poor$sim[, 13],
                               LE.20.poor$sim[, 13],
                               LE.21.poor$sim[, 13],
                               LE.22.poor$sim[, 13],
                               LE.23.poor$sim[, 13],
                               LE.24.poor$sim[, 13],
                               LE.25.poor$sim[, 13],
                               LE.26.poor$sim[, 13],
                               LE.27.poor$sim[, 13],
                               LE.28.poor$sim[, 13],
                               LE.29.poor$sim[, 13],
                               LE.30.poor$sim[, 13],
                               LE.31.poor$sim[, 13],
                               LE.32.poor$sim[, 13],
                               LE.33.poor$sim[, 13],
                               LE.34.poor$sim[, 13],
                               LE.35.poor$sim[, 13],
                               LE.36.poor$sim[, 13],
                               LE.37.poor$sim[, 13],
                               LE.38.poor$sim[, 13],
                               LE.39.poor$sim[, 13],
                               LE.40.poor$sim[, 13],
                               LE.41.poor$sim[, 13],
                               LE.42.poor$sim[, 13],
                               LE.43.poor$sim[, 13],
                               LE.44.poor$sim[, 13],
                               LE.45.poor$sim[, 13],
                               LE.46.poor$sim[, 13],
                               LE.47.poor$sim[, 13],
                               LE.48.poor$sim[, 13],
                               LE.49.poor$sim[, 13],
                               LE.50.poor$sim[, 13],
                               LE.51.poor$sim[, 13],
                               LE.52.poor$sim[, 13],
                               LE.53.poor$sim[, 13],
                               LE.54.poor$sim[, 13],
                               LE.55.poor$sim[, 13],
                               LE.56.poor$sim[, 13],
                               LE.57.poor$sim[, 13],
                               LE.58.poor$sim[, 13],
                               LE.59.poor$sim[, 13],
                               LE.60.poor$sim[, 13],
                               LE.61.poor$sim[, 13],
                               LE.62.poor$sim[, 13],
                               LE.63.poor$sim[, 13],
                               LE.64.poor$sim[, 13],
                               LE.65.poor$sim[, 13],
                               LE.66.poor$sim[, 13],
                               LE.67.poor$sim[, 13],
                               LE.68.poor$sim[, 13],
                               LE.69.poor$sim[, 13],
                               LE.70.poor$sim[, 13],
                               LE.71.poor$sim[, 13],
                               LE.72.poor$sim[, 13],
                               LE.73.poor$sim[, 13],
                               LE.74.poor$sim[, 13],
                               LE.75.poor$sim[, 13],
                               LE.76.poor$sim[, 13],
                               LE.77.poor$sim[, 13],
                               LE.78.poor$sim[, 13],
                               LE.79.poor$sim[, 13],
                               LE.80.poor$sim[, 13],
                               LE.81.poor$sim[, 13],
                               LE.82.poor$sim[, 13],
                               LE.83.poor$sim[, 13],
                               LE.84.poor$sim[, 13],
                               LE.85.poor$sim[, 13],
                               LE.86.poor$sim[, 13],
                               LE.87.poor$sim[, 13],
                               LE.88.poor$sim[, 13],
                               LE.89.poor$sim[, 13],
                               LE.90.poor$sim[, 13],
                               LE.91.poor$sim[, 13],
                               LE.92.poor$sim[, 13],
                               LE.93.poor$sim[, 13],
                               LE.94.poor$sim[, 13],
                               LE.95.poor$sim[, 13],
                               LE.96.poor$sim[, 13],
                               LE.97.poor$sim[, 13],
                               LE.98.poor$sim[, 13],
                               LE.99.poor$sim[, 13],
                               LE.100.poor$sim[, 13],
                               LE.101.poor$sim[, 13],
                               LE.102.poor$sim[, 13],
                               LE.103.poor$sim[, 13],
                               LE.104.poor$sim[, 13],
                               LE.105.poor$sim[, 13],
                               LE.106.poor$sim[, 13],
                               LE.107.poor$sim[, 13],
                               LE.108.poor$sim[, 13],
                               LE.109.poor$sim[, 13],
                               LE.110.poor$sim[, 13],
                               LE.111.poor$sim[, 13],
                               LE.112.poor$sim[, 13],
                               LE.113.poor$sim[, 13],
                               LE.114.poor$sim[, 13],
                               LE.115.poor$sim[, 13],
                               LE.116.poor$sim[, 13],
                               LE.117.poor$sim[, 13],
                               LE.118.poor$sim[, 13],
                               LE.119.poor$sim[, 13],
                               LE.120.poor$sim[, 13],
                               LE.121.poor$sim[, 13],
                               LE.122.poor$sim[, 13],
                               LE.123.poor$sim[, 13],
                               LE.124.poor$sim[, 13],
                               LE.125.poor$sim[, 13],
                               LE.126.poor$sim[, 13],
                               LE.127.poor$sim[, 13],
                               LE.128.poor$sim[, 13],
                               LE.129.poor$sim[, 13],
                               LE.130.poor$sim[, 13],
                               LE.131.poor$sim[, 13],
                               LE.132.poor$sim[, 13],
                               LE.133.poor$sim[, 13],
                               LE.134.poor$sim[, 13],
                               LE.135.poor$sim[, 13],
                               LE.136.poor$sim[, 13],
                               LE.137.poor$sim[, 13],
                               LE.138.poor$sim[, 13],
                               LE.139.poor$sim[, 13],
                               LE.140.poor$sim[, 13],
                               LE.141.poor$sim[, 13]),
                        age = rep(seq(65, 100, 0.25), each = 10)) 


### Borja study

library(tidyverse)
library(dmetar)
library(meta)
library(metafor)

data <- data.frame(studyID = c("Ostir, 2013",
                               "Pedersen, 2022",
                               "Villumsen, 2015",
                               "Ramsey, 2021",
                               "Schafthuizen, 2024",
                               "Lim, 2018",
                               "Jawad, 2022",
                               "Fisher, 2011",
                               "McCullagh, 2016",
                               "Fisher, 2016"),
                   mean = c(537.75,
                            mean,
                            474.58,
                            406.96,
                            1805.73,
                            658.17,
                            802.99, 467.08,
                            806.5,
                            653.48),
                   sd = c(264.58, sd,
                          229.67,
                          157.93,
                          1992.74,
                          313.84,
                          470.43,163.48,
                          740.5,
                          219.18),
                   n = c(224, 68, 100, 
                         141, 21, 38, 80,
                         239, 154, 164))

m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = studyID,
                   data = data,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "DL",
                   hakn = TRUE,
                   title = "Steps per day in hospitals settings")

summary(m.mean)

tiff("forest_plot.tiff", height = 10, width = 11, res = 300, units = "in")
forest.meta(m.mean,
            sortvar = TE,
            prediction = TRUE,
            print.tau2 = FALSE,
            print.tau = TRUE,
            leftlabs = c("Study",
                         "Sample size",
                         "Steps",
                         "SD"),
            rightlabs = c("Steps",
                          "95% CI",
                          "Weight"))
dev.off()
n1 <- 68
n2 <- 20
m1 <- mean
m2 <- 1208.53
sd1 <- sd
sd2 <- 551.46
  
mean <- ((n1*m1) + (n2*m2))/(n1+n2)
sd = sqrt(((n1-1) * sd1^2 + (n2-1) * sd2^2 + ((n1*n2)/(n1+n2)) * (m1^2 + m2^2 + - 2*m1*m2)) / (n1 + n2 - 1))


