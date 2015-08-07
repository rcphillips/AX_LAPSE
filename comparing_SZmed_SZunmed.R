#AXCPT_Lapse_meds
#By R. Phillips
#150706
#Goal here is to do a logistic regression on lapse rate for SZmed and SZunmed
###
#get full list of SZ
#break it into med and unmed
#relate SZs to betas, lapses
#do t-tests between groups
###
#Setwd
#IRC
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_lapses")
#libraries
library(stringr)
###
#get lapse data
SZ_lapses<-read.csv("SZ_lapses.csv",stringsAsFactors=FALSE)
SZ_lapses$ID<-str_replace_all(SZ_lapses$ID, " ", "")
#get full list of SZ
SZ_med<-c('epp13','epp141','epp149','epp158',
               'epp173','epp177','epp215','epp217',
               'epp234','epp241','epp252','epp273',
               'epp276','epp290','epp367','epp385',
               'epp388','epp394','epp414','epp423',
               'epp427','epp441','epp59','epp63',
               'epp66','epp85')
SZ_unmed<-c('epp282','epp279','epp275','epp179','epp51','epp05')

SZ_med_lapses<-SZ_lapses[-(which((SZ_lapses$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_lapses<-SZ_lapses[-(which((SZ_lapses$ID %in% SZ_unmed)=="FALSE")),]
t.test(SZ_med_lapses$N_CueCorr_AX_err,SZ_unmed_lapses$N_CueCorr_AX_err)
#l_vmPFC
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_betas")
l_vmpfc_betas<-read.csv('L_MPFC_-4_58_2_r5_betas.csv', stringsAsFactors=FALSE)
colnames(l_vmpfc_betas)[1]<-"ID"
SZ_med_l_vmpfC<-l_vmpfc_betas[-(which((l_vmpfc_betas$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_l_vmpfC<-l_vmpfc_betas[-(which((l_vmpfc_betas$ID %in% SZ_unmed)=="FALSE")),]

t.test(SZ_med_l_vmpfC$AX_CuCo_PrCo_Cu,SZ_unmed_l_vmpfC$AX_CuCo_PrCo_Cu)
#r_vmPFC
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_betas")
r_vmpfc_betas<-read.csv('FOX_R_MPFC_2_61_13_r5_betas.csv', stringsAsFactors=FALSE)
colnames(r_vmpfc_betas)[1]<-"ID"
SZ_med_r_vmpfC<-r_vmpfc_betas[-(which((r_vmpfc_betas$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_r_vmpfC<-r_vmpfc_betas[-(which((r_vmpfc_betas$ID %in% SZ_unmed)=="FALSE")),]

t.test(SZ_med_r_vmpfC$AX_CuCo_PrCo_Cu,SZ_unmed_r_vmpfC$AX_CuCo_PrCo_Cu)

#r_IPL
setwd("C:/Users/rphillipl/Box Sync/Proj_AX_LAPSE/Data_betas")
r_ipl_betas<-read.csv('FOX_R_IPL_53_-32_56_r5_betas.csv', stringsAsFactors=FALSE)
colnames(r_ipl_betas)[1]<-"ID"
SZ_med_r_ipl<-r_ipl_betas[-(which((r_ipl_betas$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_r_ipl<-r_ipl_betas[-(which((r_ipl_betas$ID %in% SZ_unmed)=="FALSE")),]

t.test(SZ_med_r_ipl$AX_CuCo_PrCo_Cu,SZ_unmed_r_ipl$AX_CuCo_PrCo_Cu)
#l_IPL
setwd("C:/Users/rphillipl/Box Sync/Proj_AX_LAPSE/Data_betas")
l_ipl_betas<-read.csv('FOX_L_IPL_-43_-40_55_r5_betas.csv', stringsAsFactors=FALSE)
colnames(l_ipl_betas)[1]<-"ID"
SZ_med_l_ipl<-l_ipl_betas[-(which((l_ipl_betas$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_l_ipl<-l_ipl_betas[-(which((l_ipl_betas$ID %in% SZ_unmed)=="FALSE")),]

t.test(SZ_med_l_ipl$AX_CuCo_PrCo_Cu,SZ_unmed_l_ipl$AX_CuCo_PrCo_Cu)

#Next: repeat for l_dlpfc, r_dlpfc, pcc, 

l_dlpfc_betas<-read.csv('FOX_L_DLPFC_-42_45_21_r5_betas.csv')
r_dlpfc_betas<-read.csv('FOX_R_DLPFC_42_47_15_r5_betas.csv')
l_vmpfc_betas<-read.csv('L_MPFC_-4_58_2_r5_betas.csv')
r_vmpfc_betas<-read.csv('')
pcc_betas<-read.csv('L_PCC_-10_-52_14_r5_betas.csv')
l_ips_betas<-read.csv('FOX_L_IPL_-43_-40_55_r5_betas.csv')
r_ips_betas<-read.csv('FOX_R_IPL_53_-32_56_r5_betas.csv')
