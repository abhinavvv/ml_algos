
class_conditional <- function(y,x){
	cm <- data.frame(y,x)
	names(cm) <- c("class","x")
	t <- table(cm)
	s <- apply(t,1,sum)
	t <- t/s
	return(t)
}


library(mlbench)
data(HouseVotes84,data=mlbench)
data <- na.omit(HouseVotes84)
x <- data[,-1]
cc <- list()
y <- data$V1
for(i in 1:dim(x)[2]){
	cc[[ i ]] <- class_conditional(data$Class,x[,i])
}

prior <- table(y)/sum(table(y))
cc <- lapply(cc,log)
  prior <- log(prior)
 
  prob_matrix <- matrix(0, nr=dim(x)[1], nc=length(prior))
  colnames(prob_matrix) <- names(prior)
  
  for (i in 1:dim(x)[1]) {
    for (j in 1:length(prior)) {
      prob_matrix[i,j] <- prior[j]
      for (k in 1:dim(x)[2]) {
        prob_matrix[i,j] <- prob_matrix[i,j] + cc[[ k ]][j,x[i,k]]
      }
    }
  }
  prob_matrix <- prob_matrix - apply(prob_matrix,1,mean)
  prob_matrix <- exp(prob_matrix)
  levels(y)[ apply(prob_matrix, 1, function (a) { which(a==max(a))[1] }) ]


