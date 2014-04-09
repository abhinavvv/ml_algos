f <- function(x){
  12*x - 3*x^4 - 2*x^6
}
df <- function(x){ 
  eval(D(expression( 12*x - 3*x^4 - 2*x^6),"x"))
}
x=seq(-1,2,length=50)
par(mfcol=c(1,2))
plot(x,f(x),type='l',ylim=c(-2,10),xlim=c(-2,2),ylab="f(x)",xlab="x")
points(0.843,7.85) 
plot(x,df(x),type='l',ylab="f'(x)",xlab="x",ylim=c(-2,10),xlim=c(-2,2))

#Bi-section Algorithm
x_lower <-0.2
x_upper <- 1.2
trace <- NULL

#to calculate mid
x_mid <- (x_lower + x_upper)/2

#the algorithm
while(x_upper - x_lower >= 0.00000001){
  initial_col <- cbind(df(x_mid),x_lower,x_upper)
  if(df(x_mid) >=0){x_lower <-x_mid }
  if(df(x_mid) <=0){x_upper <- x_mid}
  x_mid <- (x_lower + x_upper)/2
  column <- cbind(initial_col,x_mid,f(x_mid))
  trace <-rbind(trace,column)
}
#e = 0.00000001

