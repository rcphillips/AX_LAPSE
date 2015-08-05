# BetaBarGraphs
# R.Phillips
# 02/19/15
###
# these files are produced using the Master_Beta script, in the common code repository
# (I think)
# that outputs .csvs, which can be read directly into R
###
#For HCs, get the L and R mpfc, as well as the user defined PCC activations.
#take the first average
#and the second average
#and plot them next to each other as a bar graph.
#Housekeeping
setwd("C:/Users/rphillips/Box Sync/Proj_AX_LAPSE/Data_betas")
#get csvs
l_dlpfc_betas<-read.csv('FOX_L_DLPFC_-42_45_21_r5_betas.csv')
r_dlpfc_betas<-read.csv('FOX_R_DLPFC_42_47_15_r5_betas.csv')
l_vmpfc_betas<-read.csv('L_MPFC_-4_58_2_r5_betas.csv')
r_vmpfc_betas<-read.csv('FOX_R_MPFC_2_61_13_r5_betas.csv')
pcc_betas<-read.csv('L_PCC_-10_-52_14_r5_betas.csv')
l_ips_betas<-read.csv('L_IPL_PCG_-42_-30_50_r5_betas.csv')
r_ips_betas<-read.csv('FOX_R_IPL_53_-32_56_r5_betas.csv')

HC_betas<-subset(l_dlpfc_betas, l_dlpfc_betas$class=="HC")
HC_betas<-HC_betas[1:23,]#gotta remove the excel calculations!
###
#Now, for two groups, we repeat the process above for the second group, then unite them
#in the graph below.
SZ_betas<-subset(l_dlpfc_betas, l_dlpfc_betas$class=="SZ")
SZ_betas<-SZ_betas[1:32,]#gotta remove the excel calculations!

###
#plot the resutlts
par(cex=1.0, mar=c(1,5,3,1))
myplot<-barplot(c(mean(HC_betas$AX_CuCo_PrCo_Cu), 
                  mean(HC_betas$AX_CuCo_PrIn_Cu)), 
                ylab="Beta Value (a.u.)", 
                ylim=c(-2,6),
                xlim=c(0,3),
                col=c("white","black"),
                cex.lab=,
                cex.axis=2
)
legend("topright", c("HC Correct","HC Lapse"), col=c("black","black"), bty="n", pch= 22, 
       pt.bg=c("white","black"), pt.cex=3.5, cex=2.5)
abline(h=0)
title(main = list("l_dlpfc in HC", font = 4, cex=5))
# Get standard deviation of each group
# The standard deviations are saved in a matrix of same size 
# as the matrix with midpoints, this is useful for plotting 
# the error bars
means<-c(mean(HC_betas$AX_CuCo_PrCo_Cu), 
         mean(HC_betas$AX_CuCo_PrIn_Cu))
stDers <- matrix(c((sd(HC_betas$AX_CuCo_PrCo_Cu)/sqrt(23)),
                   (sd(HC_betas$AX_CuCo_PrIn_Cu)/sqrt(23))))
# Plot the vertical lines of the error bars
# The vertical bars are plotted at the midpoints
segments(myplot, means - stDers, myplot, means + stDers, lwd=3)
# Now plot the horizontal bounds for the error bars
# 1. The lower bar
segments(myplot - 0.1, means - stDers, myplot + 0.1, means - stDers, lwd=3)
# 2. The upper bar
segments(myplot - 0.1, means + stDers, myplot + 0.1, means + stDers, lwd=3)

