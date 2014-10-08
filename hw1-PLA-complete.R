## Load the library
library(ggplot2)

## Function to generate the data
data.generate = function(N = 10, ext = 1){ 
	# Generate the points.
	x1 = runif(N, -ext, ext)
	x2 = runif(N, -ext, ext)
	
	# Draw a random line in the area.
	point = runif(2, -ext, ext)
	point2 = runif(2, -ext, ext)
	slope = (point2[2] - point[2]) / (point2[1] - point[1])
	intercept = point[2] - slope * point[1]
	
	# Assign the dependent values.
	y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
	
	# Return the values.
	data = data.frame(x1,x2,y)
	return(list(data = data,slope = slope, intercept = intercept))
}	

## PLA
PLA = function(generated, max.iter = 10000){
	X = as.matrix(cbind(1, generated$data[c(1,2)]))
	w = c(0,0,0)
	counter = 0
	while(sum(sign(X %*% w) == generated$data$y) != nrow(X)){
		idx = sample(which(sign(X %*% w) !=generated$data$y),1)
		w = w + generated$data$y[idx] * X[idx,]
		counter = counter + 1
		if(counter >= max.iter) break()
	}
	return(list(w = w, counter = counter))
}

## Decision Boundary
d.b = function(w){
	dbx = runif(2, -1, 1)
	dby = (-w[1]-w[2]*dbx) / w[3]
	db.slope = (dby[2] - dby[1])/(dbx[2] - dbx[1])
	db.intercept = dby[2] - dbx[2]*db.slope
	return(list(db.slope = db.slope, db.intercept = db.intercept))
}

## Out-of-sample error rate
disagree = function(intercept, slope, db.intercept, db.slope, ext = 1, accuracy.level = 2){
	accuracy = 10 ^ accuracy.level
	x1 = x2 = 0:(accuracy) * 2 * ext / accuracy - ext
	disagreement = 0
	for(i in x1){
	 for(j in x2){
		if((i * slope + intercept > j & i * db.slope + db.intercept <= j)|(i * slope + intercept <= j & i * db.slope + db.intercept > j)) disagreement = disagreement + 1
	 }
	}
	error.rate = disagreement / accuracy^2
	return(error.rate)
}

## One example with N = 50
generated = data.generate(50) 
result = PLA(generated)
DB = d.b(result$w)
error.rate = disagree(generated$intercept, generated$slope, DB$db.intercept, DB$db.slope)
qplot(x1,x2,col= as.factor(y), data = generated$data) + geom_abline(intercept = generated$intercept, slope = generated$slope) + geom_abline(intercept = DB$db.intercept, slope = DB$db.slope, col = "blue")
print(paste("Number of iterations: ", result$counter, "  Out-of-sample error rate: ", error.rate, sep = ""))

## Problem 7 - 10
solve = function(N, run){
	actual.run = 0
	acc.error.rate = 0
	acc.counter = 0
	for(i in 1:run){
		generated = data.generate(N) 
		result = PLA(generated)
		DB = d.b(result$w)
		error.rate = disagree(generated$intercept, generated$slope, DB$db.intercept, DB$db.slope)
		if(result$counter >=10000) next()
		actual.run = actual.run + 1
		acc.error.rate = acc.error.rate + error.rate
		acc.counter = acc.counter + result$counter
	}
	mean.error.rate = acc.error.rate / actual.run
	mean.counter = acc.counter / actual.run
	return(list(mean.error.rate = mean.error.rate, mean.counter = mean.counter))
}

## Run 1000 times with N = 10
solve(10, 1000)

## Run 1000 times with N = 100
solve(100, 1000)
