#Settings for the derivation of likelihood

error <- matrix(rnorm(100,0,1),100,1)
psi <- matrix(1,100,1) # psi
lambda <- matrix(1,100,1) # lambda

s.all <- matrix(c(6,3,2,1),4,1)
s.6 <- matrix(6,100,1)
s.3 <- matrix(3,100,1)
s.2 <- matrix(2,100,1)
s.1 <- matrix(1,100,1)

utility <- function(alpha,gamma,e,s,x.1,x.0){
  alpha[1]*exp(e)/gamma*log(gamma*s*x.1+1)+alpha[2]*x.0
}

TL.6 <- sum(log(utility(psi,lambda,error,s.6,s.x.1,s.x.0)));TL.6
TL.3 <- sum(log(utility(psi,lambda,error,s.3,s.x.1,s.x.0)));TL.3
TL.2 <- sum(log(utility(psi,lambda,error,s.2,s.x.1,s.x.0)));TL.2
TL.1 <- sum(log(utility(psi,lambda,error,s.1,s.x.1,s.x.0)));TL.1

a <- matrix(0,100,4)
for(i in 1:4){
  a[,i] <- rep(mean.a.s[i,],100)
}


L.6 <- sum(log(utility(psi,lambda,error,s.6,s.x.1,s.x.0)));L.6
L.3 <- sum(log(utility(psi,lambda,error,s.3,s.x.1,s.x.0)));L.3
L.2 <- sum(log(utility(psi,lambda,error,s.2,s.x.1,s.x.0)));L.2
L.1 <- sum(log(utility(psi,lambda,error,s.1,s.x.1,s.x.0)));L.1
