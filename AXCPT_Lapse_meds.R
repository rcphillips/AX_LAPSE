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


l_vmpfc_betas<-read.csv('L_MPFC_-4_58_2_r5_betas.csv', stringsAsFactors=FALSE)
colnames(l_vmpfc_betas)[1]<-"ID"
SZ_med_l_vmpfC<-l_vmpfc_betas[-(which((l_vmpfc_betas$ID %in% SZ_med)=="FALSE")),]
SZ_unmed_l_vmpfC<-l_vmpfc_betas[-(which((l_vmpfc_betas$ID %in% SZ_unmed)=="FALSE")),]

t.test(SZ_med_l_vmpfC$AX_CuCo_PrCo_Cu,SZ_unmed_l_vmpfC$AX_CuCo_PrCo_Cu)