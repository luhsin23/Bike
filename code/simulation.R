# To generate X0 and X1

a.1   = 1
s.a.0 = 0.02
r     = 1 
s     = matrix( 6, 1, 1)   # bundle size matrix
s.x.1 = matrix( 0, 100, 1) # inside good matrix
s.x.0 = matrix( 0, 100, 1) # outside good
p.1   = matrix( runif( 100, 1, 3), 100, 1) #price
n     = matrix( rnorm( 100, 0, 1), 100, 1) 
M     = 100

# while loop: check if there is enough budget
# for loop: determine for each data whether the indide good or outside good has higher utility
total = 0

for(j in 1 : 1000){
  a.1   = 1
  s.a.0 = 0.02
  r     = 1 
  s     = matrix( 6, 1, 1)   # bundle size
  s.x.1 = matrix( 0, 100, 1) # inside good
  s.x.0 = matrix( 0, 100, 1) # outside good
  p.1   = matrix( runif( 100, 1, 3), 100, 1) # price
  n     = matrix( rnorm( 100, 0, 1), 100, 1) 
  M     = 100
  for( i in 1 : 100){
    while( M - ( p.1[ i, ] * s.x.1[i, ] + s.x.0[ i, ]) >= 0 && s.x.1[i, ]>=0){
      if((a.1 * exp(n[ i, ])) / r * log(r * s * ( s.x.1[ i, ] + 1) + 1) - (a.1 * exp( n[i, ])) / r * log(r * s * s.x.1[i, ] + 1) > s.a.0 ){
        s.x.1[i, ] = s.x.1[i, ] + 1
      }
      else{
        s.x.0[i, ] = s.x.0[i, ] + 1
      }
    }
    if(s.x.1[i, ]>=1) {s.x.1[i, ] = s.x.1[i, ] - 1}
  }
  total = total + sum((s.x.0[,1]==0))
}
total/1000

s.x = cbind(s.x.0, s.x.1)
sum((s.x[,1]==0)) #(x0=0)
sum((s.x[,1]!=0)) #(x0>0)



write.table( s.x.0,   file = "s.x.0.csv",   sep=",", row.names = F, na = "NA")
write.table( s.x.1,   file = "s.x.1.csv",   sep=",", row.names = F, na = "NA")
write.table( s.x,   file = "s.x.csv",   sep=",", row.names = F, na = "NA")


for(i in 1 : 100){
  print(s.x.1[i, ] * p.1[i, ] + s.x.0[i, ])
}
sum(s.x.0==0)

# matplot( a[ , 1 : 8000], type = "l")
# matplot( r[ , 1 : 8000], type = "l")
