# import data
hist.mean.a <- read.csv("mean_psi.csv")
hist.mean.r <- read.csv("mean_lambda.csv")
hist.sd.a <- read.csv("sd_psi.csv")
hist.sd.r <- read.csv("sd_lambda.csv")



library(ggplot2)
library(ggtext)
library(mdthemes)
library(commonmark)
##Replot for the italic "i": 20200720
a = 'i'
H = c(1:18)
for(i in 1:18){
  df = data.frame(x = rnorm(1e4, hist.mean.a[i,], hist.sd.a[i,]))
  g = ggplot(df, aes(x)) +
    ggtitle(substitute(paste(italic(i)," = ",x), list(x=i))) + 
    geom_histogram(bins = 200,color="black", fill="white") +
    theme(plot.title =  element_text(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.title = element_blank(),
          axis.text=element_text(size=15))
  setEPS()
  postscript(file=paste("a_h_20200721",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}

for(i in 1:18){
  df = data.frame(x = rnorm(1e4, hist.mean.r[i,], hist.sd.r[i,]))
  g = ggplot(df, aes(x)) +
    ggtitle(substitute(paste(italic(i)," = ",x), list(x=i))) + 
    geom_histogram(bins = 200,color="black", fill="white") +
    theme(plot.title =  element_text(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.title = element_blank(),
          axis.text=element_text(size=15))
  setEPS()
  postscript(file=paste("r_h_20200721",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}

a.result.0331 = read.csv("psi_result.csv")
r.result.0331 = read.csv("lambda_result.csv")
a_0331 = t(a.result.0331)
r_0331 = t(r.result.0331)

for(i in 1:18){
  df_a = data.frame(Iteration = 1:10000, Estimate = a_0331[-1,i])
  g = ggplot(df_a, aes(x=Iteration, y=Estimate)) +
    geom_path(size = 0.05) +
    ggtitle(substitute(paste(italic(i)," = ",x), list(x=i))) + 
    # theme(plot.title = element_text(vjust = 10)) +
    theme(plot.title =  element_text(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.text=element_text(size=16),
          axis.title=element_text(size=16))
  setEPS()
  postscript(file=paste("psi_trace_plot",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}

for(i in 1:18){
  df_r = data.frame(Iteration = 1:10000, Estimate = r_0331[-1,i])
  g = ggplot(df_r, aes(x=Iteration, y=Estimate)) +
    geom_path(size = 0.05) +
    ggtitle(substitute(paste(italic(i)," = ",x), list(x=i))) + 
    # theme(plot.title = element_text(vjust = 10)) +
    theme(plot.title =  element_text(vjust = -8, hjust =0.97,color="blue", size=15),
          axis.text=element_text(size=16),
          axis.title=element_text(size=16))
  setEPS()
  postscript(file=paste("lambda_trace_plot",i,".eps"),horiz=FALSE,onefile=FALSE,width=5.84,height=3.4) 
  plot(g)
  dev.off()
}
