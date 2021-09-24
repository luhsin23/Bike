# import data
hist.mean.a <- read.csv("mean_psi.csv")
hist.mean.r <- read.csv("mean_lambda.csv")
hist.sd.a <- read.csv("sd_psi.csv")
hist.sd.r <- read.csv("sd_lambda.csv")

hist(rnorm(1e4,hist.mean.a[3,],hist.sd.a[3,]),breaks = 100)
title <- matrix(0,18,1)

title[,1] <-c("Area.1 Rate.1",
              "Area.1 Rate.2",
              "Area.1 Rate.3",
              "Area.2 Rate.1",
              "Area.2 Rate.2",
              "Area.2 Rate.3",
              "Area.3 Rate.1",
              "Area.3 Rate.2",
              "Area.3 Rate.3",
              "Area.4 Rate.1",
              "Area.4 Rate.2",
              "Area.4 Rate.3",
              "Area.5 Rate.1",
              "Area.5 Rate.2",
              "Area.5 Rate.3",
              "Area.6 Rate.1",
              "Area.6 Rate.2",
              "Area.6 Rate.3"
) 

title.jpeg <- matrix(0,18,1)
title.jpeg[,1] <-c("Area.1 Rate.1.jpeg",
              "Area.1 Rate.2.jpeg",
              "Area.1 Rate.3.jpeg",
              "Area.2 Rate.1.jpeg",
              "Area.2 Rate.2.jpeg",
              "Area.2 Rate.3.jpeg",
              "Area.3 Rate.1.jpeg",
              "Area.3 Rate.2.jpeg",
              "Area.3 Rate.3.jpeg",
              "Area.4 Rate.1.jpeg",
              "Area.4 Rate.2.jpeg",
              "Area.4 Rate.3.jpeg",
              "Area.5 Rate.1.jpeg",
              "Area.5 Rate.2.jpeg",
              "Area.5 Rate.3.jpeg",
              "Area.6 Rate.1.jpeg",
              "Area.6 Rate.2.jpeg",
              "Area.6 Rate.3.jpeg"
) 

for(i in 1:18){
  jpeg(title.jpeg[i,], width = 640, height = 360)
  hist(rnorm(1e4,hist.mean.a[i,],hist.sd.a[i,]),breaks = 100, main = title[i,], xlab = "value of estimate")
  dev.off()
}

library(ggplot2)
library(ggtext)
library(mdthemes)
##Replot for the italic "i": 20200720
a = 'i'
for(i in 1:18){
  df = data.frame(x = rnorm(1e4, hist.mean.a[i,], hist.sd.a[i,]))
  g = ggplot(df, aes(x)) +
    ggtitle(paste("*i*"," = ",i)) + 
    geom_histogram(bins = 200,color="black", fill="white") +
    theme(plot.title =  element_markdown(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.title = element_blank(),
          axis.text=element_text(size=15))
  setEPS()
  postscript(file=paste("psi_h",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}

for(i in 1:18){
  df = data.frame(x = rnorm(1e4, hist.mean.r[i,], hist.sd.r[i,]))
  g = ggplot(df, aes(x)) +
    ggtitle(paste("*i*"," = ",i)) + 
    geom_histogram(bins = 200,color="black", fill="white") +
    theme(plot.title =  element_markdown(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.title = element_blank(),
          axis.text=element_text(size=15))
  setEPS()
  postscript(file=paste("lambda_h",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}

