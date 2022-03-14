library(ggplot2)
library(RColorBrewer)
library(Hmisc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# --- data input
c<-read.table("cages.csv", header=T, sep=",")

names(c)[1] <- "Generation"
names(c)[2] <- "Release"
names(c)[3] <- "Replicate"
names(c)[4] <- "Hom"
names(c)[5] <- "Het"
names(c)[6] <- "WT"
names(c)[7] <- "failed PCR"

c$n <- c$Hom + c$Het + c$WT
c$carrier <- c$Hom + c$Het
c$drive <- (c$carrier / c$n)
c$drive_af <- (c$Hom + c$Het/2)/(c$Hom + c$Het + c$WT)
c$wt_af <- (c$WT + c$Het/2)/(c$Hom + c$Het + c$WT)
c$total_af <- c$drive_af + c$wt_af

ctest <- subset(c, Generation >= 1)

p4 <- ggplot(c, aes(x=Generation, y=drive_af)) +
  geom_smooth(span = 1, method = "loess") +
  scale_shape_manual(values=c(15, 16, 17,18)) +
  geom_point(aes(group=Replicate, shape=as.factor(Replicate)))  +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  scale_x_continuous(breaks=0:10) + 
  facet_wrap(c$Release) +
  theme_bw()
  
plot(p4)





