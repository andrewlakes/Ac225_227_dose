library(reshape2)
library(plyr)
library(dplyr)
library(drc)
library(drfit)
library(ggplot2)
library(deSolve)
library(emdbook)
library(stats)
library(plotly)
library(gridExtra)
library(abind)
library(RColorBrewer)
library(tidyr)
library(GenKern)
library(xlsx)


columnnames = c("Days",	"Blood",	"Thymus",	"Heart",	"Lungs",	"Kidneys",	"Spleen",	"Liver",	"ART",	"Carcass",	"Tumor")

workbook = loadWorkbook("2018_6_20_import_to_R.xlsx")
sheetsnames = names(getSheets(workbook))

Days = read.xlsx("2018_6_20_import_to_R.xlsx",
                  sheetIndex = 1)

#Also add/subtract the average to each error


Average225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
                       sheetIndex = 2))

minus225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 3))

plus225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 4))

# Average227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
#           sheetIndex = 5))
# 
# minus227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
#           sheetIndex = 6))
# 
# plus227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
#           sheetIndex = 7))

#version with 0.5% the dose 227 of 225 
#since it's 200 nCi 225 and 20 nCi theoretical (actual is 
# divide 227 by 19.1x to get to 0.5%)
Average227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 5)/19.1)

minus227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 6)/19.1)

plus227 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 7)/19.1)

Over225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 8)/19.1)

minusover225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 9)/19.1)

plusover225 = cbind(Days,read.xlsx("2018_6_20_import_to_R.xlsx",
          sheetIndex = 10)/19.1)

colnames(Average225) = columnnames
colnames(Average227) = columnnames
colnames(Over225) = columnnames

colnames(minus225) = columnnames
colnames(plus225) = columnnames
colnames(minus227) = columnnames
colnames(plus227) = columnnames
colnames(minusover225) = columnnames
colnames(plusover225) = columnnames



#melt

mAverage225 = melt(Average225, id="Days")
colnames(mAverage225) = c("times", "Organs", "values")

mminus225 = melt(minus225, id="Days")
colnames(mminus225) = c("times", "Organs", "valuesminus")
mplus225 = melt(plus225, id="Days")
colnames(mplus225) = c("times", "Organs", "valuesplus")

mAverage227 = melt(Average227, id="Days")
colnames(mAverage227) = c("times", "Organs", "values")

mminus227 = melt(minus227, id="Days")
colnames(mminus227) = c("times", "Organs", "valuesminus")
mplus227 = melt(plus227, id="Days")
colnames(mplus227) = c("times", "Organs", "valuesplus")

mOver225 = melt(Over225, id="Days")
colnames(mOver225) = c("times", "Organs", "values")

mminusover225 = melt(minusover225, id="Days")
colnames(mminusover225) = c("times", "Organs", "valuesminus")
mplusover225 = melt(plusover225, id="Days")
colnames(mplusover225) = c("times", "Organs", "valuesplus")


#bind second data column to error sets

mAverage225error = cbind(mminus225,mplus225[3])
mAverage227error = cbind(mminus227,mplus227[3])
mOver225error = cbind(mminusover225,mplusover225[3])



plot225 = ggplot()+ 
  geom_line(data=mAverage225, aes(x=times, y=values, color=Organs), size=1.75, alpha=1)+
  geom_ribbon(data=mAverage225error, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.00000001,100,11)), limits=c(10^(-8),20))+
  theme_bw() +
  theme(legend.position="none",plot.margin = unit(c(0.25,0.75,0.25,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "200 nCi Ac-225 Dose (µGy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))
  #+
  #guides(shape=guide_legend(override.aes = list(size=3)))



plot227 = ggplot()+ 
  geom_line(data=mAverage227, aes(x=times, y=values, color=Organs), size=1.75, alpha=1)+
  geom_ribbon(data=mAverage227error, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.00000001,100,11)), limits=c(10^(-8),20))+
  theme_bw() +
  theme(legend.position="none", plot.margin = unit(c(0.25,0.75,0.25,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "1 nCi Ac-227 Dose (µGy)", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))#+
  #guides(shape=guide_legend(override.aes = list(size=3)))


plotover225 = ggplot()+ 
  geom_line(data=mOver225, aes(x=times, y=values, color=Organs), size=1.75, alpha=1)+
  geom_ribbon(data=mOver225error, aes(x=times, ymin=valuesminus,  ymax=valuesplus, fill = Organs), alpha = 0.2)+
  
  
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))+ 
  
  scale_x_log10(breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))+
  annotation_logticks(base = 10, sides = "bl", scaled = TRUE,
                      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
                      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL)+
  
  scale_y_log10(breaks=c(lseq(0.0001,100,7)))+
  theme_bw() +
  theme(legend.position="right", plot.margin = unit(c(0.25,0.75,0.25,0.75), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Time (days)", y = "Ratio Dose Ac-227/Ac-225", color="Organs")+
  theme(text = element_text(size=18, face = "bold"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"))+
  guides(shape=guide_legend(override.aes = list(size=3)))



grid.arrange(arrangeGrob(plot225, plot227, ncol=2), arrangeGrob(plotover225, ncol=1))
