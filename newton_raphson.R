#data
x <- seq(0.5,1.5,length=50)

#function f(x)
f <- function(x){
  x^3 + 2*x - 4
}

#plotting f(x)
plot(x,f(x),type='l',ylim=c(-3,2))

#function df(x)
df <- function(x){
  eval(D(expression( x^3 + 2*x - 4),"x"))
}

#function ddf(x)
ddf <- function(x){
  eval(D(D(expression( x^3 + 2*x - 4),"x"),"x"))
}

i  <- 1
t <- c(1.4)
par(mfrow=c(2,2))

#newton-raphson's algo
while(i < 5){
  
  #plotting the progress
  plot(x,f(x),type='l',ylim=c(-3,2))
  points(t, f(t), col="blue", bg="blue", pch=1)
  text(t[i]-0.2, f(t[i])+0.2,
       paste("(",round(t[i],digits=2),",",round(f(t[i]),digits=2),")"))
  
  abline(v=t, col="green"); abline(h=0)
  
  #newton-raphson equation
  t <-c(t,t[i] - (f(t[i])/df(t[i])))
  i <- i+1
}

#finding the zeros 
arrows(1.18,2.05,1.18,0.05,length=0.25,angle=10)
