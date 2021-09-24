library(ggplot2)
library(dplyr)
price = matrix( c(5, 10, 20, 5, 10, 20, 5, 10, 20, 5, 10, 20, 5, 10, 20, 5, 10, 20),  18,  1)
# e = matrix( exp( rnorm(1800, 0, 1)), 100, 18) #e: error term 1800個
s.1.2.9 = matrix(c(1,2,9),18,1)

mean.a = read.csv("mean_psi.csv")
mean.r = read.csv("mean_lambda.csv")



df = data.frame()
for(j in 1:18){ #Rental_Option

  # print(df)
  a = exp(mean.a[j, ])
  r = exp(mean.r[j,])
  other.p = price
  s = s.1.2.9[j, ]
  M = 2450  

  
  for(k in 1 : 5){ #iteration for each Rental_Option
    
    fun = function(x, alpha, gamma, p, p.s, e.1, e.2, other.p){
      p * ((( alpha * e.2 * p.s * p * (r * s * x + 1)) / ( a * e.1 * s * other.p )) - 1) / (gamma * p.s) +
        other.p * (((alpha * e.2 * p.s * p * (r * s * x + 1)) / ( a * e.1 * s * other.p)) - 1) / (gamma * p.s)
    }
    
    fun.p = function(p, x){
      sum = 0
      for(i in 1 : 18){
        sum = sum + fun(x, exp( mean.a[i, ]), exp(mean.r[i, ]), p, s.1.2.9[i, ], exp(rnorm(1)), exp(rnorm(1)), other.p[i, ])
      } 
      sum - M
    }
    
    fun.x = function(x){
      uniroot(fun.p, c(0, 10000000), x = x)[[1]]   # to solve for p
    }
    
    P = rep(0, 40) 
    
    for(i in 1 : 40){
      for(m in 1:5){
        P[i] = P[i] + fun.x(i) 
      }
      P[i] = P[i]/ 5
    }
    
    d = data.frame(1:40, P,j)
    df = rbind(df,d)
  }
  
}


df = read.csv("demand_20201011.csv")
for(b in 1:6){
  p = ggplot(df[(600*(b-1)+1):(600*b),], aes(x=X1.40, y=P, col = Rental_Option, linetype = Rental_Option)) + 
    geom_point(aes(shape=Rental_Option), size=0.6) +
    scale_shape_manual(values=c(4, 1, 17))+
    geom_smooth(size = 0.4)  +
    xlab("Quantity") + 
    ylab("Price") + 
    scale_x_continuous(limits = c(0,40),breaks = seq(0,40,2)) +
    ylim(-0.1,20) +
    # scale_color_brewer(palette="Dark2") + 
    scale_colour_manual(values=c("#900009","#999999", "#000000") )+ 
    # scale_linetype_manual(values=c("solid", "dashed","dotted"))+
    ggtitle(paste("area",b)) +
    theme(legend.position="bottom",
          legend.text = element_text(size = 9),
          plot.title = element_text(vjust = -9.5, hjust =0.95,color="blue", size=9),
  axis.text=element_text(size=8),
  axis.title=element_text(size=9))
  setEPS()
  postscript(file=paste("area_shape",b,".eps"),horiz=FALSE,onefile=FALSE,width=4.5,height=3.25) 
  plot(p)
  dev.off()
}

# p = ggplot(df[(600*(b-1)+1):(600*b),], aes(x=X1.40, y=P, col = Rental_Option) ) + 
#   geom_point(size=0.3)+ 
#   #geom_smooth(size = 0.4) +
#   xlab("Quantity") + 
#   ylab("Price") + 
#   scale_x_continuous(limits = c(0,40),breaks = seq(0,40,2)) +
#   ylim(-0.1,20) + 
#   scale_color_brewer(palette="Dark2") + 
#   # ggtitle(paste("area =",i)) +
#   theme(legend.position="bottom",
#         plot.title = element_text(vjust = -8, hjust =0.97,color="blue", size=15))
# plot(p)




