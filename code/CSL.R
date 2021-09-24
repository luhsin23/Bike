# Import file

mean.a.rent.am = read.csv2("mean_psi_am")
mean.r.rent.am = read.csv2("mean_lambda_am")


#value setting
{
price <- matrix(c(5,10,20,5,10,20,5,10,20,5,10,20,5,10,20,5,10,20), 18, 1)
n <- 1
a <- exp(mean.a.rent.am[n,])
r <- exp(mean.r.rent.am[n,])
other.p <- price
s <- s.1.2.9[n,]
M <- 2450
return <- 139
}

#plot
{
plot(1:60,P,ylim=c(0,40),xlim=c(0,80),xlab = "quantity", ylab = "price",cex=0.2,main="MRT Gongguan Sta.(Exit 2).3")
for(i in 1:20){
  fun <- function(x, alpha, gamma, p, p.s, e.1, e.2, other.p){
    p*(((alpha*e.2*p.s*p*(r*s*x+1))/(a*e.1*s*other.p))-1)/(gamma*p.s) +
      other.p*(((alpha*e.2*p.s*p*(r*s*x+1))/(a*e.1*s*other.p))-1)/(gamma*p.s)
  }
  fun.p <- function(p,x){
    sum <- 0
    for(i in 1:12){
      sum <- sum + fun(x,exp(mean.a[i,]),exp(mean.r[i,]), p, s.1.2.9[i,], exp(rnorm(1)), exp(rnorm(1)),other.p[i,])
    } 
    sum-M
  }
  fun.x <- function(x){
    uniroot(fun.p,c(0,1000),x=x)[[1]]
  }
  
  P <- rep(0,60)
  for(i in 1:60){
    P[i] <- fun.x(i)
  }
  par(new=T)
  plot(1:60,P,axes = FALSE,ylim=c(0,25),xlim=c(0,65),xlab = "", ylab = "",cex=0.2)
}
}



test <- array(0,dim = c(1000,4,5))
#1.st interval
{
dim=5
n=1
panelty <- 2
price[n,] <- 5
price[n,] <- price[n,]+panelty
a <- exp(mean.a.rent.am[n,]);a
r <- exp(mean.r.rent.am[n,]);r
s <- s.1.2.9[n,];s
p.5 <-price[n,];p.5
p.0 <- 0.5
}
write.table(Q,file="Q.csv",sep=",",row.names=F, na = "NA")

##p=5
for(i in 730:1000){
Q[i,n,dim] <- fun.p(p.5)
print(i)
}

##p=0
for(i in 1:1000){
Q[i,n+1,dim] <- fun.p(p.0+panelty)
print(i)
}

#2.nd interval
{
n=2
a <- exp(mean.a.rent.am[n,]);a
r <- exp(mean.r.rent.am[n,]);r
s <- s.1.2.9[n,];s
p.n<-price[n,];p.n

for(i in 943:1000){
Q[i,n+1,dim] <- fun.p(p.n)
  print(i)
}
}

#3.rd interval
{
panelty <- 0
n=3
a <- exp(mean.a.rent.am[n,]);a
r <- exp(mean.r.rent.am[n,]);r
s <- s.1.2.9[n,];s
p.n<-price[n,];p.n
 
for(i in 945:1000){
  Q[i,n+1,dim] <- fun.p(p.n+panelty)
  print(i)
}
}

#table
#20:80
{
Q.orig.20.80 <- array(0,dim=c(t,3,5))
for(i in 1:5){
Q.orig.20.80[c(1:200),1,i] <- Q[c(1:200),1,i]
Q.orig.20.80[c(201:1000),1,i] <- Q[c(201:1000),2,i];Q.orig.20.80[,2,i] <- Q[,3,i];Q.orig.20.80[,3,i] <- Q[,4,i]
}
orig.20.80 <- matrix(0,t,5)
for(i in 1:5){
  orig.20.80[,i] <- matrix(rowSums(Q.orig.20.80[,,i]),t,1)  
}
for (i in 1:5) {
  print(mean(current[,i]))
}

current <- return-orig.20.80


table.20.80 <- matrix(0,30,5)
for(k in 1:5){
for(n in 0:30){
  capacity <- 30
  capacity <- capacity-n
  count=0
for(i in 1:1000){
  if((capacity+current[i,k])>0){
    count <- count+1
  }
}
  table.20.80[n,k] <- count/1000
}
}
rn <- as.character(c(30:1));rn
rownames(table.20.80) <- c(rn)
colnames(table.20.80) <- c("5.0/0.0","5.5/0.5","6.0/1.0","6.5/1.5","7.0/2.0")
write.table(table.20.80,file="table.20.80.csv",sep=",",row.names=F, na = "NA")
}
#40:60
{
Q.orig.40.60 <- array(0,dim=c(t,3,5))
for(i in 1:5){
  Q.orig.40.60[c(1:400),1,i] <- Q[c(1:400),1,i]
  Q.orig.40.60[c(401:1000),1,i] <- Q[c(401:1000),2,i];Q.orig.40.60[,2,i] <- Q[,3,i];Q.orig.40.60[,3,i] <- Q[,4,i]
}
orig.40.60 <- matrix(0,t,5)
for(i in 1:5){
  orig.40.60[,i] <- matrix(rowSums(Q.orig.40.60[,,i]),t,1)  
}
for (i in 1:5) {
  print(mean(current[,i]))
}

current <- return-orig.40.60


table.40.60 <- matrix(0,30,5)
for(k in 1:5){
  for(n in 0:30){
    capacity <- 30
    capacity <- capacity-n
    count=0
    for(i in 1:1000){
      if((capacity+current[i,k])>0){
        count <- count+1
      }
    }
    table.40.60[n,k] <- count/1000
  }
}
rownames(table.40.60) <- c(rn)
colnames(table.40.60) <- c("5.0/0.0","5.5/0.5","6.0/1.0","6.5/1.5","7.0/2.0")
write.table(table.40.60,file="table.40.60.csv",sep=",",row.names=F, na = "NA")
}
#50:50
{
  Q.orig.50.50 <- array(0,dim=c(t,3,5))
  for(i in 1:5){
    Q.orig.50.50[c(1:500),1,i] <- Q[c(1:500),1,i]
    Q.orig.50.50[c(501:1000),1,i] <- Q[c(501:1000),2,i];Q.orig.50.50[,2,i] <- Q[,3,i];Q.orig.50.50[,3,i] <- Q[,4,i]
  }
  orig.50.50 <- matrix(0,t,5)
  for(i in 1:5){
    orig.50.50[,i] <- matrix(rowSums(Q.orig.50.50[,,i]),t,1)  
  }
  
  current <- return-orig.50.50
  
  
  table.50.50 <- matrix(0,30,5)
  for(k in 1:5){
    for(n in 0:30){
      capacity <- 30
      capacity <- capacity-n
      count=0
      for(i in 1:1000){
        if((capacity+current[i,k])>0){
          count <- count+1
        }
      }
      table.50.50[n,k] <- count/1000
    }
  }
  rownames(table.50.50) <- c(rn)
  colnames(table.50.50) <- c("5.0/0.0","5.5/0.5","6.0/1.0","6.5/1.5","7.0/2.0")
  write.table(table.50.50,file="table.50.50.csv",sep=",",row.names=F, na = "NA")
}
#60:40
{
  Q.orig.60.40 <- array(0,dim=c(t,3,5))
  for(i in 1:5){
    Q.orig.60.40[c(1:600),1,i] <- Q[c(1:600),1,i]
    Q.orig.60.40[c(601:1000),1,i] <- Q[c(601:1000),2,i];
    Q.orig.60.40[,2,i] <- Q[,3,i];
    Q.orig.60.40[,3,i] <- Q[,4,i]
  }
  orig.60.40 <- matrix(0,t,5)
  for(i in 1:5){
    orig.60.40[,i] <- matrix(rowSums(Q.orig.60.40[,,i]),t,1)  
  }
  
  current <- return-orig.60.40
  
  
  table.60.40 <- matrix(0,30,5)
  for(k in 1:5){
    for(n in 0:30){
      capacity <- 30
      capacity <- capacity-n
      count=0
      for(i in 1:1000){
        if((capacity+current[i,k])>0){
          count <- count+1
        }
      }
      table.60.40[n,k] <- count/1000
    }
  }
  rownames(table.60.40) <- c(rn)
  colnames(table.60.40) <- c("5.0/0.0","5.5/0.5","6.0/1.0","6.5/1.5","7.0/2.0")
  write.table(table.60.40,file="table.60.40.csv",sep=",",row.names=F, na = "NA")
}

#80:20
{
  Q.orig.80.20 <- array(0,dim=c(t,3,5))
  for(i in 1:5){
    Q.orig.80.20[c(1:800),1,i] <- Q[c(1:800),1,i]
    Q.orig.80.20[c(801:1000),1,i] <- Q[c(801:1000),2,i];Q.orig.80.20[,2,i] <- Q[,3,i];Q.orig.80.20[,3,i] <- Q[,4,i]
  }
  orig.80.20 <- matrix(0,t,5)
  for(i in 1:5){
    orig.80.20[,i] <- matrix(rowSums(Q.orig.80.20[,,i]),t,1)  
  }
  
  current <- return-orig.80.20
  
  
  table.80.20 <- matrix(0,30,5)
  for(k in 1:5){
    for(n in 0:30){
      capacity <- 30
      capacity <- capacity-n
      count=0
      for(i in 1:1000){
        if((capacity+current[i,k])>0){
          count <- count+1
        }
      }
      table.80.20[n,k] <- count/1000
    }
  }
  rownames(table.80.20) <- c(rn)
  colnames(table.80.20) <- c("5.0/0.0","5.5/0.5","6.0/1.0","6.5/1.5","7.0/2.0")
  write.table(table.80.20,file="table.80.20.csv",sep=",",row.names=F, na = "NA")
}






