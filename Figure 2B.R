library(ggplot2)
library(RColorBrewer)
library(plyr)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- data input
d<-read.table("homing.csv", header=T, sep=",")
names(d)[1] <- "Father_genotype"
d$Father_genotype <- gsub('TM6B', '+', d$Father_genotype)
d$Mother_genotype <- gsub('TM6B', '+', d$Mother_genotype)
names(d)[5] <- "Transmission"

d$cross <- paste(d$Father_genotype,"x",d$Mother_genotype)
d$size <- d$Progeny.Inheriting.Drive.Allele + d$Progeny.Inheriting.WT.Allele
#d$size <- is.na(d$size)
#d <- d[- grep("bam", d$cross),]

levelorder <- c('bam_dCFP/+ x W118','W118 x bam_dCFP/+','rcd-1r_dCFP/+ x W118','W118 x rcd-1r_dCFP/+','rcd-1r_d/+ x W118','W118 x rcd-1r_d/+')     

d.aov <- aov(Transmission ~ cross, data = d)
# Summary of the analysis
summary(d.aov)
TukeyHSD(d.aov)

d_sum <- ddply(d, c("cross"), summarise,
               mean = mean(Transmission),
               med = median(Transmission),
               sd   = sd(Transmission))

            
p <- ggplot(d, aes(x=factor(cross,level=levelorder), y=Transmission)) +
  geom_boxplot(fill="white") +
  geom_dotplot(binaxis='y', stackdir='center',binwidth=1)+
  geom_hline(yintercept=50,linetype="dashed", color = "red")+
  scale_fill_brewer(palette="Dark2") +
  coord_flip() +
  theme_bw()+
  theme(axis.text=element_text(size=12))

plot(p)

#--------- homozygous control crosses ----------

h<-read.table("homing_homo.csv", header=T, sep=",")
names(h)[1] <- "Father_genotype"
names(h)[5] <- "Transmission"
h$cross <- paste(h$Father_genotype,"x",h$Mother_genotype)



# p2 <- ggplot(h, aes(x=factor(cross), y=Transmission)) +
#   geom_boxplot(fill="white") +
#   geom_dotplot(binaxis='y', stackdir='center',binwidth=1)+
#   geom_hline(yintercept=50,linetype="dashed", color = "red")+
#   scale_fill_brewer(palette="Dark2") +
#   coord_flip() +
#   theme_bw()+
#   theme(axis.text=element_text(size=12))
# 
# plot(p2)
