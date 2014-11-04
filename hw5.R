## Problem 5 - 7
p5_7 = function(u = 1, v = 1 ,eta = 0.1, goal = 1e-14, max.iter = 0, method){
	E_u_v = function() (u*exp(v) - 2*v*exp(-u))^2
	gradient_u = function() 2*(exp(v)+2*v*exp(-u))*(u*exp(v)-2*v*exp(-u))
	gradient_v = function() 2*(u*exp(v)-2*exp(-u))*(u*exp(v)-2*v*exp(-u))
	iter.count = 0
	if(method == "GD"){
		while(E_u_v() >= goal){
			gradient.u = gradient_u()
			gradient.v = gradient_v()
			u = u - eta*gradient.u
			v = v - eta*gradient.v
			iter.count = iter.count + 1
			if(iter.count >= max.iter & max.iter != 0) break()
			}
	} else if(method == "CD"){
		while(E_u_v() >= goal){
			u = u - eta*gradient_u()
			v = v - eta*gradient_v()
			iter.count = iter.count + 1
			if(iter.count >= max.iter & max.iter != 0) break()
			}
	} else {
		stop("Invalid method.")
	}
	return(list(u = u, v = v, iter.count = iter.count, E.u.v = E_u_v()))
}

p5_6result = p5_7(method = "GD")
p5_6result

p7_result = p5_7(method = "CD", max.iter = 15)
p7_result


## Function to generate the data
data.generate = function(N = 100, ext = 1){ 
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

## Stochastic Logistic Regression
SLR = function(generated, max.iter = 10000, eta = 0.01, min.update = 0.01){
	X = as.matrix(cbind(1, generated$data[c(1,2)]))
	y = generated$data$y
	w = c(0,0,0)
	w.old = w
	counter = 0
	repeat{
		for(i in sample(nrow(X))){
			gradient = - (y[i] * X[i,]) / (1 + exp(y[i] * X[i,] %*% w))
			w = w - eta * gradient
		}
		counter = counter + 1
		if(sqrt(sum((w-w.old)^2)) <= min.update) break()
		w.old = w
	}
	return(list(w = w, counter = counter))
}

## Estimate out-of-sample Cross Entropy Error
disagree = function(intercept, slope, w, ext = 1, accuracy.level = 2){
	accuracy = 10 ^ accuracy.level
	x1 = x2 = 0:(accuracy) * 2 * ext / accuracy - ext
	X = as.matrix(cbind(1, x1, x2))
	y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
	sum(log(1 + exp(- y * X %*% w))) / nrow(X)
}

## Problem 8 & 9
epoch.acc = 0
CEE.acc = 0
for(i in 1:100){
	generated = data.generate()
	result = SLR(generated)
	epoch = result$counter
	epoch.acc = epoch.acc + epoch
	CEE = disagree(generated$intercept, generated$slope, result$w)
	CEE.acc = CEE.acc + CEE
}
CEE.acc/100
epoch.acc/100
