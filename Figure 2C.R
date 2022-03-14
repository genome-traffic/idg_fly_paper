library(ggplot2)
library(RColorBrewer)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

overall <- read.table("indels.csv",sep=",", header=T)

p <- ggplot(overall, aes(x=Sample, y=Unmodified.)) +
  geom_boxplot(fill="white") +
  geom_dotplot(binaxis='y', stackdir='center',binwidth=1)+
  scale_fill_brewer(palette="Dark2") +
  geom_hline(yintercept=50,linetype="dashed", color = "red")+
  theme_bw()+
  theme(axis.text=element_text(size=12))

plot(p)

fs <- read.table("fs.csv",sep=",", header=T)

fss <- subset(fs, type=="frameshift")
fss <- subset(fss, sample!="WT")

p2 <- ggplot(fss, aes(x=sample, y=percent_fs)) +
  geom_boxplot(fill="white", outlier.size = 0.1) +
  geom_dotplot(binaxis='y', stackdir='center',binwidth=1, dotsize = 2)+
  scale_fill_brewer(palette="Dark2") +
  geom_hline(yintercept=(100/3)*2,linetype="dashed", color = "red")+
  theme_bw()+
  theme(axis.text=element_text(size=12))

plot(p2)




