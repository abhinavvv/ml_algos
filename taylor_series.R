#data
x<- seq(-3,3,length=60)

#function definition f(x)
f <- function(x){
   x^3 - 12*x + 1
}

#for higher order derivatives, but never used
diff <- function(expression,x,n){
  if(n==1) return(eval(D(expression,"x")))
  else diff(D(expression,"x"),n-1)
}

#to compute df(x)/dx 
df <- function(x,n=1){
  if(n < 1) stop("'order' must be >= 1") 
  if(n == 1) eval(D(expression(x^3 - 12*x + 1),"x"))
  else
    diff(D(expression(x^3 - 12*x + 1),"x"),x,n-1)  
}

#calculating taylor approximation
taylor_term=function(x1,y1,n=1){
  y1 + eval(df(x1,n))*(x-x1)^n/factorial(n)
}


#plot for f(x)
plot(x,f(x),type='l',xlim=c(-3,3),ylim=c(-30,30),ylab="f(x)")
grid()
par(new=TRUE)

#plotting the taylor approximation 
plot(x,taylor_term(-1,f(-1)),type='l',
     xlim=c(-3,3),ylim=c(-30,30),xlab="",ylab="")

#plotting the tangential point
points(-1,f(-1),cex=.5,col="red")
points(-1,f(-1),cex=1.5,col="red")

#for arrow and text
arrows(1,15,x1=-3,y1=f(-1),length =0.5,angle=10,col="red")
text(1.5,15,label="(-1,12)")


#plot for tangential approximation point for f'(x)
plot(x,df(x),type='l',xlim=c(-3,3),ylim=c(-30,30),ylab="f'(x)")
grid()
par(new=TRUE)

#plotting the taylor approximation
plot(x,taylor_term(-1,df(-1)),type='l',
     xlim=c(-3,3),ylim=c(-30,30),xlab="",ylab="")

#plotting the tangential points
points(-1,df(-1))
points(-1,df(-1),cex=.5,col="red")
points(-1,df(-1),cex=1.5,col="red")

#arrows and text
arrows(1,15,x1=-1,y1=df(-1),length =0.5,angle=10,col="red")
text(1.5,15,label="(-1,9)")

plot(x,df(x,2),type='l',xlim=c(-3,3),ylim=c(-30,30),ylab="f''(x)")
grid()