find_threshold <- function(data){
	sse <- list()
	for(i in 1:length(data$x)){
		sse_left <- NULL
		sse_right <- NULL
		for(j in 1:i){
			sse_left <- (data$y[j] - 
				mean(data$y[1:i]))^2 
		}

		for(k in i+1:length(data$y)){
			sse_right <- (data$y[k] - 
				mean(data$y[i+1:length(data$y)]))^2
		}

		sse <- append(sse,(sse_left+sse_right))

	}
	return( c(match(min(sse)),min(sse)) )
}



tree_creation <- function(data,sse<- list(),i <- 1){
	i <- i+1
	values <- find_threshold(data)
	left_array <- data[1:values[2],]
	right_array <- data[values[2]+1:length(data),]
	append(sse,values[1])
	if(length(left_array$x) == 1 || length(right_array)== 1 ||  
		abs(sse[i-1] - sse[1]) == 0.0001){
		break
	}
	tree_creation(left_array)
	tree_creation(right_array)

}

install.package("tree")
library(tree)
library(rpart)
head(car.test.frame)
x <- car.test.frame$Mileage
y <- car.test.frame$Weight
data <- data.frame(x,y)
tree_creation(data)

