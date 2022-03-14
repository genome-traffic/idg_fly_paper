library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(data.table)
# install.packages("forcats")
library(forcats)
library(multcomp)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#----------------------------------- Figure Fertility ----------------
fer<-read.table("fertility.csv", header=T, sep=",")
fer <- group_by(fer, genotype)

fer.aov <- aov(embryos.24h ~ genotype, data = fer)
# Summary of the analysis
summary(fer.aov)
TukeyHSD(fer.aov)

hr.aov <- aov(hr ~ genotype, data = fer)
# Summary of the analysis
summary(hr.aov)
TukeyHSD(hr.aov)


fer_sum <- dplyr::summarise(fer, mean=mean(embryos.24h), sd=sd(embryos.24h),n = n(), se = sd / sqrt(n))
fer_sum2 <- dplyr::summarise(fer, mean=mean(hr), sd=sd(hr),n = n(), se = sd / sqrt(n))


p3 <- ggplot(fer_sum, aes(x=genotype,y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(strip.text = element_text(size=12)) +
  theme(axis.text.x=element_text(size=12, angle=90,hjust=0.95,vjust=0.2)) +
  ylab("eggs/female/day") + xlab("")
plot(p3)

p4 <- ggplot(fer_sum2, aes(x=genotype,y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(strip.text = element_text(size=12)) +
  theme(axis.text.x=element_text(size=12, angle=90,hjust=0.95,vjust=0.2)) +
  ylab("hatching rate") + xlab("")
plot(p4)

