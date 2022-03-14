library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(data.table)
# install.packages("forcats")
library(forcats)
library(multcomp)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#----------------------------------- Figure 1 -----------------
f<-read.table("facs_data.csv", header=T, sep=",")
f <- group_by(f, construct, intron)

ft <- subset(f, intron == "synthetic intron")
anovares <- aov(fluorescence ~ construct, data = ft)
summary(anovares)
tukey.plot.aov<-aov(fluorescence ~ construct, data=ft)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
#plot(tukey.plot.test, las = 1)
print(tukey.plot.test)


fs <- summarise(f, mean=mean(fluorescence), sd=sd(fluorescence))
fs$o <- c(9,9,2,2,2,2,2,2,1,1,6,6,3,3,7,7,4,4,8,8,5,5)
# f <- with(f, f[order(o),])
#f$construct <- factor(f$construct, levels = f$construct[order(f$o)])

p <- ggplot(fs, aes(x=fct_reorder(construct, o),y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
  position=position_dodge(.9)) +
  facet_wrap(~ intron) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,250000) +
  theme(strip.text = element_text(size=18))
plot(p)



#------------------------------------ Figure S1 -----------------------------

fs<-read.table("facs_data_supp.csv", header=T, sep=",")
fs <- group_by(fs, construct, intron)
fs <- summarise(fs, mean=mean(fluorescence), sd=sd(fluorescence))
fs$o <- c(4,4,1,1,0,0,3,3)
#fs <- with(fs, fs[order(o),])
#fs$construct <- factor(fs$construct, levels = fs$construct[order(fs$o)])

p2 <- ggplot(fs, aes(x=fct_reorder(construct, o),y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~ intron) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,200000) +
  theme(strip.text = element_text(size=12)) +
  theme(axis.text.x=element_text(size=12, angle=90,hjust=0.95,vjust=0.2)) +
  ylab("Relative GFP fluorescence (au)") + xlab("")
  
plot(p2)

