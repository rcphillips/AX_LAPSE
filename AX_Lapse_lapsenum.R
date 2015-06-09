#AX_LAPSE lapsenumbers
#150608
#R.Phillips
#Previous script is on a different computer, so going to get
#lapse numbers for each group, quick and easy
#result: t test
###
#housekeeping
#load HC csv
#load SZ csv
#add grouping variable
#identify lapse column
#use GGplot to plot subject lapses by group
###
#housekeeping
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_lapses")
install.packages("ggplot2")
#load HC csv
HC_data<-read.csv("HC_lapses.csv",stringsAsFactors=FALSE)
#load SZ csv
SZ_data<-read.csv("SZ_lapses.csv",stringsAsFactors=FALSE)
#add grouping variable
HC_data<-cbind(HC_data, rep("HC",length(HC_data$ID)))
SZ_data<-cbind(SZ_data, rep("SZ",length(SZ_data$ID)))
#join lists
colnames(SZ_data)<-colnames(HC_data)
all_data<-rbind(HC_data,SZ_data)
all_data_trimmed<-data.frame(NULL)
#remove spaces from ID column
all_data$ID<-gsub(" ","",all_data$ID, fixed = TRUE)
#remove unused subjects
subjlist<-c("epc20","epc21","epc24","epc46",
            "epc48","epc54","epc66","epc76",
            "epc84","epc87","epc93","epc94","epc98",
            "epc99","epc107","epc112","epc116",
            "epc121","epc132","epc138",
            "epc174","epc189","epp85","epp66","epp63",
            "epp59","epp51","epp441","epp427","epp423",
            "epp414","epp394","epp388","epp385","epp367",
            "epp290","epp282","epp279","epp276","epp275",
            "epp273","epp252","epp241","epp234","epp217",
            "epp215","epp179","epp177","epp173","epp158",
            "epp149","epp141","epp13","epp05")
i=1

for (i in 1:length(all_data$ID)){
  if (all_data$ID[i] %in% subjlist){
    all_data_trimmed<-rbind(all_data_trimmed,all_data[i,])
  }
}
#Cool, 32 SZ, 23 HC.
#identify lapse column
colnames(all_data_trimmed)<-c("ID","Age","N_CueCorr_AX_err","N_CueCorr_BY_err","group")
#It's N_CueCorr_AX_err and N_CueCorr_BY_err
#use GGplot to plot subject lapses by group
library(ggplot2)
lapsenum_plot<-ggplot(all_data_trimmed, aes(x=group,y=N_CueCorr_AX_err))+geom_point()
lapsenum_plot
#get means and SDs for AX
means_AX<-c(mean(all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="HC")]),
         mean(all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="SZ")]))
sds_AX<-c(sd(all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="HC")]),
       sd(all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="SZ")]))
ttest_result_AX<-t.test((all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="HC")]),
         (all_data_trimmed$N_CueCorr_AX_err[which(all_data_trimmed$group=="SZ")]))
result_AX<-rbind(means_AX,sds_AX)
colnames(result_AX)<-c("HC","SZ")
rownames(result_AX)
result_AX
ttest_result_AX
#repeat for BY
means_BY<-c(mean(all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="HC")]),
         mean(all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="SZ")]))
sds_BY<-c(sd(all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="HC")]),
       sd(all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="SZ")]))
ttest_result_BY<-t.test((all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="HC")]),
                     (all_data_trimmed$N_CueCorr_BY_err[which(all_data_trimmed$group=="SZ")]))
result_BY<-rbind(means_BY,sds_BY)
colnames(result_BY)<-c("HC","SZ")
rownames(result_BY)
result_BY
ttest_result_BY

