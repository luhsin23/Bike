#data 
{
data.new <- read.csv("final.data.csv")
d <- data.new
d <- d[-(50001:50966),]
nrow(d)/500


#add@200
dat.1 <- matrix(NA, ncol = ncol(d), nrow = (nrow(d)/500))
for(i in 0:(nrow(d)/500)-1){
  dat.1[i+1,] <- colSums(d[250*i+1:250*(i+1),])+1
}

#package size
#@200
s.100 <- matrix(c(1,2,9,1,2,9,1,2,9,1,2,9,1,2,9,1,2,9), 18, 1) 


#price
p.1000 <- matrix(NA, nrow = (nrow(d)/200),ncol = ncol(d)) 
for(i in 0:(nrow(d)/200)-1){
  p.1000[i+1,] <- c(5,10,20,
                    5,10,20,
                    5,10,20,
                    5,10,20,
                    5,10,20,
                    5,10,20)
}

}
#set initial value
n <- 1
t <- 100
R=1e4

# R=2
varance.0 <- matrix(1,n,R+1)
# x.0 <- s.x.0 #matrix(0,n,1)
# a.0 <- s.a.0 #runif(1,0,0.04)

x.0 = matrix(0,n,1)
a.0 = runif(1,0,0.04)




lb <- matrix(0,t,n)
ub <- matrix(0,t,n)
error <- matrix(rnorm(n*t,0,1),t,n)
###step 1: Set initial values for a, r, M
r.new <- matrix(NA,n,R+1)
a <- matrix(log(1),n,R+1)     #alpha: 1 * 10,001 converge to the true alpha
a.result <- matrix(0,18,R+1)  
r <- matrix(log(1),n,R+1)
r.result <- matrix(0,18,R+1)
accept.1 <- matrix(0,n,R+1)
M <- 100

###step 2: Set initial values for z
{z <- array(NA,dim = c(t,n,R+1))
  for(i in 1:t){
    for(j in 1:n){
      z[i,j,] <- error[i,j]
    }
  } }

#metropolis hasting_r
{
  r.likelihood <- function(a,r){
    
    #generate lb
    for(i in 1:t){
      for(j in 1:n){
        lb[i,j] <- (log(a.0*p[i,j]*exp(r)/exp(a))-log(log((exp(r)*s[j]*x[i,j]+1)/(exp(r)*s[j]*(x[i,j]-1)+1))))
        if(lb[i,j] == "NaN"){print(x[i,j])}
      }
    }
    nrow(ub);ncol(ub)
    nrow((log(a.0*p[i,j]*exp(r)/exp(a))-log(log((exp(r)*s[j]*(x[i,j]+1)+1)/(exp(r)*s[j]*x[i,j]+1)))));ncol((log(a.0*p[i,j]*exp(r)/exp(a))-log(log((exp(r)*s[j]*(x[i,j]+1)+1)/(exp(r)*s[j]*x[i,j]+1)))))
    
    #generate ub
    for(i in 1:t){
      for(j in 1:n){
        ub[i,j] <- (log(a.0*p[i,j]*exp(r)/exp(a))-log(log((exp(r)*s[j]*(x[i,j]+1)+1)/(exp(r)*s[j]*x[i,j]+1))))
        if(ub[i,j] == "NaN"){print(x[i,j])}
      }
    }
    ints <- rep(0,n)
    for(i in 1:n){ 
      ints[i] = integrate(dnorm,lb[1,i],ub[1,i])[[1]] # log likelihood
    }
    llike = sum(log(ints))
    return(llike)}   
  
  r.prior <- function(r){
    rprior = dnorm(r, sd = 0.1, log = T)
    return(rprior)
  }
  
  proposalfunction <- function(r){
    r <- rnorm(1,mean = r, sd= 0.1)
    return(r)}
  
  r.posterior <- function(a,r){
    return (r.likelihood(a,r) + r.prior(r))
  }
}

for(pkg.size in 1:18){
  #data
  s <- s.100[pkg.size,] #matrix(0,n,1)
  x <- as.matrix(dat.1[1:t,pkg.size],1,t) #matrix(0,n,t)
  p <- as.matrix(p.1000[1:t,pkg.size],t,1) #matrix(0,t,n)
  
  
  #iteration
  for(k in 1:R){
    
    ###step 3: draw z
    
    #generate lb
    for(i in 1:t){
      for(j in 1:n){
        lb[i,j] <- (log(a.0*p[i,j]*exp(r[j,k])/exp(a[j,k]))-log(log((exp(r[j,k])*s[j]*x[i,j]+1)/(exp(r[j,k])*s[j]*(x[i,j]-1)+1))))
        if(lb[i,j] == "NaN"){
          print(r[j,k])
          
        }
      }
    }
    
    #generate ub
    for(i in 1:t){
      for(j in 1:n){
        ub[i,j] <- (log(a.0*p[i,j]*exp(r[j,k])/exp(a[j,k]))-log(log((exp(r[j,k])*s[j]*(x[i,j]+1)+1)/(exp(r[j,k])*s[j]*x[i,j]+1))))
        if(ub[i,j] == "NaN"){
          print(r[j,k])
          
        }
      }
    }
    
    
    
    sample <- matrix(NA,t,n)
    
    #(draw z:if z lies within the range of lb and ub, will accept the value)
    for(i in 1:t){
      for(j in 1:n){
        sample[i,j] <- rnorm(1, mean = a[j,k], sd = 1)
        while(sample[i,j]<lb[i,j] | sample[i,j]>ub[i,j]){
          sample[i,j] <- rnorm(1, mean = a[j,k], sd = 1)
        }
        z[i,j,k+1] <- sample[i,j]
        
      }}
    
    ###step 4: draw alpha
    for(i in 1:n){
      a[i,k+1] <- rnorm(1, mean = ((10/1+sum(z[,n,k]))/(1/1+t)), sd = ((1/1+t)^(-1)) )
    }
    
    ###step 5: draw r
    {
      
      r.new[,k] <- proposalfunction(r[,k])
      accept <- r.posterior(a[,k],r.new[,k]) / r.posterior(a[,k],r[,k])
      if (accept < runif(1,0,2)){
        r[,k+1] <- r.new[,k]
      }else{
        r[,k+1] <- r[,k]}
      
      
    }
    
    accept.1[,k+1] <- accept
    
    if(k%%1000 == 0){
      cat(k,"/",pkg.size,"\n")
      print(Sys.time())
    }
  }
  a.result[pkg.size,] <- a[1,]
  r.result[pkg.size,] <- r[1,]
}


#result 0330
{
  mean.a <- as.matrix(apply(a.result,1,mean),18,1)
  mean.r <- as.matrix(apply(r.result,1,mean),18,1)
  sd.a <- as.matrix(apply(a.result,1,sd),18,1)
  sd.r <- as.matrix(apply(r.result,1,sd),18,1)
  write.table(mean.a,file="mean_psi.csv",sep=",",row.names=F, na = "NA")
  write.table(mean.r,file="mean_lambda.csv",sep=",",row.names=F, na = "NA")
  write.table(sd.a,file="sd_psi.csv",sep=",",row.names=F, na = "NA")
  write.table(sd.r,file="sd_lambda.csv",sep=",",row.names=F, na = "NA")
  
  write.table(a.result,file="psi_result.csv",sep=",",row.names=F, na = "NA")
  write.table(r.result,file="lambda_result.csv",sep=",",row.names=F, na = "NA")
}

