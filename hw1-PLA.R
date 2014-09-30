## Load the library
library(ggplot2)

## Function to generate the data
data.generate = function(n = 10, ext = 1){ 
	# Generate the points.
	x1 = runif(n, -ext, ext)
	x2 = runif(n, -ext, ext)
	
	# Draw a random line in the area.
	point = runif(2, -ext, ext)
	slope = tan(runif(1, 0, 2 * pi))
	intercept = point[2] - slope * point[1]
		###slope = runif(1)
		###intercept = runif(1, -ext, ext)
	
	# Assign the dependent values.
	y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
	
	# Return the values.
	data = data.frame(x1,x2,y)
	return(list(data = data,slope = slope, intercept = intercept))
}	

## Generates the data; passing the parameters with desired numbers can change the points generated as well as the range. (n = 100, ext = 2)
generated = data.generate()

## Plot the data.
qplot(x1,x2,col= as.factor(y), data = generated$data) + geom_abline(intercept = generated$intercept, slope = generated$slope)

## Initializing PLA
independent = as.matrix(cbind(1, generated$data[c(1,2)]))
w = c(0,0,0)

## PLA
