delta <- function(y_class, y_eg){
	if(y_class == y_eg) return(1)
	else return(0)
}

mean <- function(data,class_value){
	sum_x <- 0 
	sum_y <- 0
	for(i in 1:length(data[,1])){
		if(data$class[i] == class_value){
			sum_x = sum_x+data$X[i]
		}
		
		sum_y = sum_y + delta(data$class[i],class_value)
		
	}
	return(sum_x/sum_y)

}

var <- function(data,class_value,mean){
	sum_x <- 0 
	sum_y <- 0
	for(i in 1:length(data[,1])){
		if(data$class[i] == class_value){
			sum_x = sum_x+(data$X[i]-mean)^2
		}
		sum_y = sum_y + delta(data$class[i],class_value)
		
	}

	return(sum_x/sum_y)
}

p <- function(x,mean_1,var_1,mean_2,var_2){
	if(0.5*(1/(sqrt(2*pi*var_1)))*exp(-0.5*((x-mean_1)/sqrt(var_1))^2)  >= 
		0.5*(1/(sqrt(2*pi*var_2)))*exp(-0.5*((x-mean_2)/sqrt(var_2))^2)) return(1)
	else return(2)
}

predictions <- function(x,mean_1,var_1,mean_2,var_2){
	labels <- c()
	for(i in 1:length(x)){
		labels <- append(labels,p(x[i],mean_1,var_1,mean_2,var_2))
	}
	return(labels)
}

class1_data <- data.frame(rnorm(100, 2,1),1)
class2_data <- data.frame(rnorm(100,6,1),2)
names(class1_data) <- c("X","class")
names(class2_data) <- c("X","class")
data <- rbind(class1_data,class2_data)
mean_1 <- mean(data,1)
mean_2 <- mean(data,2)
sd_1 <- var(data,1,mean_1)
sd_2 <- var(data,2,mean_2)
predictions(data$X,mean_1,sd_1,mean_2,sd_2)
table(predictions(data$X,mean_1,sd_1,mean_2,sd_2))



#using package e107
model <- naiveBayes(class ~ ., data = data)

#prediction
predict(model, data[1:10,1], type = "raw")

#validation
pred <- predict(model, data[,1])

#plotting correctly classified and incorrectly classified items
table(pred, data$class)



