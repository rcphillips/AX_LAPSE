#Relating demographics to lapses
#R.Phillips
#150301
###
install.packages("stringr")
install.packages("MASS")
install.packages("R.utils")
library(stringr)
library(MASS)
library(R.utils)
###
###Housekeeping
#CNS comp
setwd("~/Box Sync/Proj_AX_LAPSE/Data_demographics/Relate_demographics_to_lapse")
#IRC comp
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_demographics/Relate_demographics_to_lapse")
###
HC_namelist<-c('epc106','epc107','epc112','epc116',
               'epc121','epc132','epc138','epc174',
               'epc189','epc20','epc21','epc24',
               'epc46','epc48','epc54','epc66',
               'epc76','epc84','epc87','epc93',
               'epc94','epc98','epc99')
SZ_namelist<-c('epp05','epp13','epp141','epp149',
               'epp158','epp173','epp177','epp179',
               'epp215','epp217','epp234','epp241',
               'epp252','epp273','epp275','epp276',
               'epp279','epp282','epp290','epp367',
               'epp385','epp388','epp394','epp414',
               'epp423','epp427','epp441','epp51',
               'epp59','epp63','epp66','epp85')
###
#a much better start:
HC_demos<-read.csv("HC_demos.csv", stringsAsFactors=FALSE)
colnames(HC_demos)[1]<-"ID"
SZ_demos<-read.csv("SZ_demos.csv", stringsAsFactors=FALSE)
colnames(SZ_demos)[1]<-"ID"
HC_lapses<-read.csv("HC_lapses.csv", stringsAsFactors=FALSE)
HC_lapses$ID<-str_replace_all(HC_lapses$ID, " ", "")
SZ_lapses<-read.csv("SZ_lapses.csv",stringsAsFactors=FALSE)
SZ_lapses$ID<-str_replace_all(SZ_lapses$ID, " ", "")
l_dlpfc_betas<-read.csv('FOX_L_DLPFC_-42_45_21_r5_betas.csv', stringsAsFactors=FALSE)
colnames(l_dlpfc_betas)[1]<-"ID"
l_vmpfc_betas<-read.csv('L_MPFC_-4_58_2_r5_betas.csv', stringsAsFactors=FALSE)
colnames(l_vmpfc_betas)[1]<-"ID"
r_dlpfc_betas<-read.csv('FOX_R_DLPFC_42_47_15_r5_betas.csv', stringsAsFactors=FALSE)
colnames(r_dlpfc_betas)[1]<-"ID"
pcc_betas<-read.csv('FOX_I_PCC_-0_-33_40_r5_betas.csv', stringsAsFactors=FALSE)
colnames(pcc_betas)[1]<-"ID"
#lining the groups up with each other
HC_demos<-HC_demos[-(which((HC_demos$ID %in% HC_namelist)=="FALSE")),]
HC_lapses<-HC_lapses[-(which((HC_lapses$ID %in% HC_namelist)=="FALSE")),]
#epc138 is missing demographic data, and so is dropped.
HC_lapses<-HC_lapses[-(which((HC_lapses$ID %in% HC_demos$ID)=="FALSE")),]
SZ_lapses<-SZ_lapses[-(which((SZ_lapses$ID %in% SZ_namelist)=="FALSE")),]
SZ_lapses<-SZ_lapses[order(SZ_lapses$ID),]
#betas, too
#l_dlpfc
HC_l_dlpfc_betas<-l_dlpfc_betas[grep("epc", l_dlpfc_betas$ID),]
HC_l_dlpfc_betas<-HC_l_dlpfc_betas[-(which((HC_l_dlpfc_betas$ID %in% HC_demos$ID)=="FALSE")),]
HC_l_dlpfc_betas<-HC_l_dlpfc_betas[order(HC_l_dlpfc_betas$ID),]
SZ_l_dlpfc_betas<-l_dlpfc_betas[grep("epp", l_dlpfc_betas$ID),]
SZ_l_dlpfc_betas<-SZ_l_dlpfc_betas[order(SZ_l_dlpfc_betas$ID),]
#r_dlpfc
HC_r_dlpfc_betas<-r_dlpfc_betas[grep("epc", r_dlpfc_betas$ID),]
HC_r_dlpfc_betas<-HC_r_dlpfc_betas[-(which((HC_r_dlpfc_betas$ID %in% HC_demos$ID)=="FALSE")),]
HC_r_dlpfc_betas<-HC_r_dlpfc_betas[order(HC_r_dlpfc_betas$ID),]
SZ_r_dlpfc_betas<-r_dlpfc_betas[grep("epp", r_dlpfc_betas$ID),]
SZ_r_dlpfc_betas<-SZ_r_dlpfc_betas[order(SZ_r_dlpfc_betas$ID),]
#l_vmpfc
HC_l_vmpfc_betas<-l_vmpfc_betas[grep("epc", l_vmpfc_betas$ID),]
HC_l_vmpfc_betas<-HC_l_vmpfc_betas[-(which((HC_l_vmpfc_betas$ID %in% HC_demos$ID)=="FALSE")),]
HC_l_vmpfc_betas<-HC_l_vmpfc_betas[order(HC_l_vmpfc_betas$ID),]
SZ_l_vmpfc_betas<-l_vmpfc_betas[grep("epp", l_vmpfc_betas$ID),]
SZ_l_vmpfc_betas<-SZ_l_vmpfc_betas[order(SZ_l_vmpfc_betas$ID),]
#pcc
HC_pcc_betas<-pcc_betas[grep("epc", pcc_betas$ID),]
HC_pcc_betas<-HC_pcc_betas[-(which((HC_pcc_betas$ID %in% HC_demos$ID)=="FALSE")),]
HC_pcc_betas<-HC_pcc_betas[order(HC_pcc_betas$ID),]
SZ_pcc_betas<-pcc_betas[grep("epp", pcc_betas$ID),]
SZ_pcc_betas<-SZ_pcc_betas[order(SZ_pcc_betas$ID),]
###
#modeling
#l_dlpfc:
HC_data<-merge(HC_demos,HC_lapses,by.x="ID")
HC_data<-merge(HC_data,HC_l_dlpfc_betas,by.x="ID",)
HC_age_on_lapses<-summary(lm(HC_data$N_CueCorr_AX_err ~ HC_data$Current.Age))
HC_age_on_l_dlpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$Current.Age))
HC_IQ_on_lapses<-summary(lm(HC_data$N_CueCorr_AX_err ~ HC_data$WASI.Score))
HC_IQ_on_l_dlpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$WASI.Score))
#r_dlpfc
HC_data<-merge(HC_demos,HC_lapses,by.x="ID")
HC_data<-merge(HC_data,HC_r_dlpfc_betas,by.x="ID",)
HC_age_on_r_dlpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$Current.Age))
HC_IQ_on_r_dlpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$WASI.Score))
#l_vmpfc
HC_data<-merge(HC_demos,HC_lapses,by.x="ID")
HC_data<-merge(HC_data,HC_l_vmpfc_betas,by.x="ID",)
HC_age_on_l_vmpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$Current.Age))
HC_IQ_on_l_vmpfc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$WASI.Score))
#pcc
HC_data<-merge(HC_demos,HC_lapses,by.x="ID")
HC_data<-merge(HC_data,HC_pcc_betas,by.x="ID",)
HC_age_on_pcc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$Current.Age))
HC_IQ_on_pcc<-summary(lm(HC_data$AX_CuCo_PrIn_Cu ~ HC_data$WASI.Score))
#SZIQ
#l_dlpfc:
SZ_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
SZ_data<-merge(SZ_data,SZ_l_dlpfc_betas,by.x="ID",)
SZ_age_on_lapses<-summary(lm(SZ_data$N_CueCorr_AX_err ~ SZ_data$Current.Age))
SZ_age_on_l_dlpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ SZ_data$Current.Age))
SZ_IQ_on_lapses<-summary(lm(SZ_data$N_CueCorr_AX_err ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))
SZ_IQ_on_l_dlpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))
#r_dlpfc
SZ_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
SZ_data<-merge(SZ_data,SZ_r_dlpfc_betas,by.x="ID",)
SZ_age_on_r_dlpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ SZ_data$Current.Age))
SZ_IQ_on_r_dlpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))
#l_vmpfc
SZ_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
SZ_data<-merge(SZ_data,SZ_l_vmpfc_betas,by.x="ID",)
SZ_age_on_l_vmpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ SZ_data$Current.Age))
SZ_IQ_on_l_vmpfc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))
#pcc
SZ_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
SZ_data<-merge(SZ_data,SZ_pcc_betas,by.x="ID",)
SZ_age_on_pcc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ SZ_data$Current.Age))
SZ_IQ_on_pcc<-summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))

###
#Chi-squared tests of gender
HC_demos$Sex<-capitalize(HC_demos$Sex)
HC_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
HC_tbl<-table(HC_lapses$N_CueCorr_AX_err, HC_demos$Sex)
HC_gender_on_lapses<-chisq.test(HC_tbl)
#repeating for SZ
SZ_demos$sex<-str_replace_all(SZ_demos$sex, " ", "")
SZ_data<-merge(SZ_demos,SZ_lapses,by.x="ID")
SZ_tbl<-table(SZ_lapses$N_CueCorr_AX_err, SZ_demos$sex)
SZ_gender_on_lapses<-chisq.test(SZ_tbl)

#repeating tests for gender for betas:
#l_dlpfc
HC_demos$Sex<-capitalize(HC_demos$Sex)
HC_data<-merge(HC_demos,HC_l_dlpfc_betas,by.x="ID")
HC_tbl<-table(HC_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, HC_data$Sex)
HC_gender_on_l_dlpfc<-chisq.test(HC_tbl)
SZ_data<-merge(SZ_demos,SZ_l_dlpfc_betas,by.x="ID")
SZ_tbl<-table(SZ_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, SZ_data$sex)
SZ_gender_on_l_dlpfc<-chisq.test(SZ_tbl)
#r_dlpfc
HC_demos$Sex<-capitalize(HC_demos$Sex)
HC_data<-merge(HC_demos,HC_r_dlpfc_betas,by.x="ID")
HC_tbl<-table(HC_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, HC_data$Sex)
HC_gender_on_r_dlpfc<-chisq.test(HC_tbl)
SZ_data<-merge(SZ_demos,SZ_r_dlpfc_betas,by.x="ID")
SZ_tbl<-table(SZ_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, SZ_data$sex)
SZ_gender_on_r_dlpfc<-chisq.test(SZ_tbl)
#l_vmpfc
HC_demos$Sex<-capitalize(HC_demos$Sex)
HC_data<-merge(HC_demos,HC_l_vmpfc_betas,by.x="ID")
HC_tbl<-table(HC_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, HC_data$Sex)
HC_gender_on_l_vmpfc<-chisq.test(HC_tbl)
SZ_data<-merge(SZ_demos,SZ_l_vmpfc_betas,by.x="ID")
SZ_tbl<-table(SZ_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, SZ_data$sex)
SZ_gender_on_l_vmpfc<-chisq.test(SZ_tbl)
#pcc
HC_demos$Sex<-capitalize(HC_demos$Sex)
HC_data<-merge(HC_demos,HC_pcc_betas,by.x="ID")
HC_tbl<-table(HC_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, HC_data$Sex)
HC_gender_on_pcc<-chisq.test(HC_tbl)
SZ_data<-merge(SZ_demos,SZ_pcc_betas,by.x="ID")
SZ_tbl<-table(SZ_data$AX_CuCo_PrIn_Cu.AX_CuCo_PrCo_Cu, SZ_data$sex)
SZ_gender_on_pcc<-chisq.test(SZ_tbl)


###
#results of all this - age
HC_age_results<-rbind(
  HC_age_on_lapses$r.squared,
  HC_age_on_l_dlpfc$r.squared,
  HC_age_on_r_dlpfc$r.squared,
  HC_age_on_l_vmpfc$r.squared,
  HC_age_on_pcc$r.squared)
rownames(HC_age_results)<-c("HC_lapses","HC_l_dlpfc", "HC_r_dlpfc","HC_l_vmpfc","HC_pcc")
colnames(HC_age_results)<-c("HC")

SZ_age_results<-rbind(
  SZ_age_on_lapses$r.squared,
  SZ_age_on_l_dlpfc$r.squared,
  SZ_age_on_r_dlpfc$r.squared,
  SZ_age_on_l_vmpfc$r.squared,
  SZ_age_on_pcc$r.squared)
rownames(SZ_age_results)<-c("SZ_lapses","SZ_l_dlpfc", "SZ_r_dlpfc","SZ_l_vmpfc","SZ_pcc")
colnames(SZ_age_results)<-c("SZ")

#results of all this - IQ
HC_IQ_results<-rbind(
  HC_IQ_on_lapses$r.squared,
  HC_IQ_on_l_dlpfc$r.squared,
  HC_IQ_on_r_dlpfc$r.squared,
  HC_IQ_on_l_vmpfc$r.squared,
  HC_IQ_on_pcc$r.squared)
rownames(HC_IQ_results)<-c("HC_lapses","HC_l_dlpfc", "HC_r_dlpfc","HC_l_vmpfc","HC_pcc")
colnames(HC_IQ_results)<-c("HC")

SZ_IQ_results<-rbind(
  SZ_IQ_on_lapses$r.squared,
  SZ_IQ_on_l_dlpfc$r.squared,
  SZ_IQ_on_r_dlpfc$r.squared,
  SZ_IQ_on_l_vmpfc$r.squared,
  SZ_IQ_on_pcc$r.squared)
rownames(SZ_IQ_results)<-c("SZ_lapses","SZ_l_dlpfc", "SZ_r_dlpfc","SZ_l_vmpfc","SZ_pcc")
colnames(SZ_IQ_results)<-c("SZ")

HC_gender_results<-rbind(
  HC_gender_on_lapses$p.value,
  HC_gender_on_l_dlpfc$p.value,
  HC_gender_on_r_dlpfc$p.value,
  HC_gender_on_l_vmpfc$p.value,
  HC_gender_on_pcc$p.value)
rownames(HC_gender_results)<-c("HC_lapses","HC_l_dlpfc", "HC_r_dlpfc","HC_l_vmpfc","HC_pcc")
colnames(HC_gender_results)<-c("HC")

SZ_gender_results<-rbind(
  SZ_gender_on_lapses$p.value,
  SZ_gender_on_l_dlpfc$p.value,
  SZ_gender_on_r_dlpfc$p.value,
  SZ_gender_on_l_vmpfc$p.value,
  SZ_gender_on_pcc$p.value)
rownames(SZ_gender_results)<-c("SZ_lapses","SZ_l_dlpfc", "SZ_r_dlpfc","SZ_l_vmpfc","SZ_pcc")
colnames(SZ_gender_results)<-c("SZ")

HC_age_results
SZ_age_results
HC_IQ_results
SZ_IQ_results
HC_gender_results
SZ_gender_results

cor(SZ_data$N_CueCorr_AX_err[which(as.numeric(SZ_data$WASI.IQ.Full.2., na.rm=FALSE)!="NA")],as.numeric(SZ_data$WASI.IQ.Full.2., na.rm=FALSE)[which(as.numeric(SZ_data$WASI.IQ.Full.2., na.rm=FALSE)!="NA")])
summary(lm(SZ_data$AX_CuCo_PrIn_Cu ~ as.numeric(SZ_data$WASI.IQ.Full.2.)))

