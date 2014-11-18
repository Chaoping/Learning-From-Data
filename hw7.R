## Read in the Data
data.in = read.table("http://work.caltech.edu/data/in.dta", col.names = c("x1","x2","y"))
data.out = read.table("http://work.caltech.edu/data/out.dta", col.names = c("x1","x2","y"))

## Split data
data.train = data.in[1:25,]
data.val = data.in[26:35,]

## Functions to map X to Z.
##############################################
##	Wrong implementations.		:(			##
##############################################
## phi0 = function(X) numeric(nrow(X))+1	##
## phi1 = function(X) X[,1]					##
## phi2 = function(X) X[,2]					##
## phi3 = function(X) X[,1]^2				##
## phi4 = function(X) X[,2]^2				##	
## phi5 = function(X) X[,1]*X[,2]			##
## phi6 = function(X) abs(X[,1]-X[,2])		##
## phi7 = function(X) abs(X[,1]+X[,2])		##
##############################################
phi0 = function(X) numeric(nrow(X))+1
phi1 = function(X) cbind(1, X[,1])
phi2 = function(X) cbind(1, X[,1], X[,2])
phi3 = function(X) cbind(1, X[,1], X[,2],X[,1]^2)
phi4 = function(X) cbind(1, X[,1], X[,2],X[,1]^2,X[,2]^2)
phi5 = function(X) cbind(1, X[,1], X[,2],X[,1]^2,X[,2]^2,X[,1]*X[,2])
phi6 = function(X) cbind(1, X[,1], X[,2],X[,1]^2,X[,2]^2,X[,1]*X[,2],abs(X[,1]-X[,2]))
phi7 = function(X) cbind(1, X[,1], X[,2],X[,1]^2,X[,2]^2,X[,1]*X[,2],abs(X[,1]-X[,2]),abs(X[,1]+X[,2]))

## Linear Regression
LR = function(X, y){
	w = solve(t(X) %*% X) %*% t(X) %*% y
	return(w)
}

## LR Classifier
LRC = function(X, w) sign(X %*% w)

## Classification Error
CE = function(pred.y, y) 1 - mean(pred.y == y)

## Problem 1 & 2
for(k in 0:7){
	phi = get(paste("phi",k,sep = ""))
	Z.train = phi(data.train) 
	Z.val = phi(data.val)
	Z.test = phi(data.out)
	w = LR(Z.train, data.train[,3])
	E.val = CE(LRC(Z.val, w), data.val[,3])
	E.out = CE(LRC(Z.test, w), data.out[,3])
	cat("For k = ",k,", E.val = ",E.val,", E.out = ",E.out,"\n",sep = "")
}

## Problem 3 - 5
data.train = data.in[26:35,]
data.val = data.in[1:25,]
for(k in 0:7){
	phi = get(paste("phi",k,sep = ""))
	Z.train = phi(data.train) 
	Z.val = phi(data.val)
	Z.test = phi(data.out)
	w = LR(Z.train, data.train[,3])
	E.val = CE(LRC(Z.val, w), data.val[,3])
	E.out = CE(LRC(Z.test, w), data.out[,3])
	cat("For k = ",k,", E.val = ",E.val,", E.out = ",E.out,"\n",sep = "")
}

## Problem 6
e1.acc = e2.acc = emin.acc = 0
for(i in 1:100000){
	e1 = runif(1,0,1)
	e2 = runif(1,0,1)
	emin = min(c(e1,e2))
	e1.acc = e1.acc+e1
	e2.acc = e2.acc+e2
	emin.acc = emin.acc + emin
}
e1.acc/100000
e2.acc/100000
emin.acc/10000


## Problem 8 - 10
## Function to generate the data
data.generate = function(N = 10, ext = 1){ 
	repeat{
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
		
		# Check if both sides have at least one point
		if(abs(sum(y)) != sum(abs(y))) break()
	}
		# Return the values.
		data = data.frame(x1,x2,y)
		return(list(data = data,slope = slope, intercept = intercept))
}

## PLA function:
PLA = function(generated, max.iter = 10000){
	X = as.matrix(cbind(1, generated$data[c(1,2)]))
	w = c(0,0,0)
	counter = 0
	
	# Iterate to update the weights until converge.
	while(sum(sign(X %*% w) == generated$data$y) != nrow(X)){
		idx = sample(which(sign(X %*% w) !=generated$data$y),1)
		w = w + generated$data$y[idx] * X[idx,]
		counter = counter + 1
		
		# Break the loop if it fails to converge.
		if(counter >= max.iter) break()
	}
	
	# Return the weights as well as the number of iterations.
	return(list(w = w, counter = counter)) 
}

## SVM
library(LowRankQP)
QC = function(X, y) (y %*% t(y)) * (X %*% t(X))
SVM = function(generated){
	X = as.matrix(generated$data[c(1,2)])
	y = generated$data$y
	Vmat = QC(X, y)
	dvec = numeric(length(y)) - 1
	Amat = t(y)
	bvec = 0
	uvec = rep(10000,length(y))
	solution = LowRankQP(Vmat, dvec, Amat, bvec, uvec, "LU")
	alpha = zapsmall(solution$alpha, 6)
	############################
	## w = t(alpha * y) %*% X ##
	############################
	SV.index = which(alpha != 0)
	w = t(X[SV.index,]) %*% (alpha[SV.index] * y[SV.index])
	b = 1/y[SV.index[1]] - X[SV.index[1],] %*% w
	return(list(w = w, b = as.numeric(b), N.SV = length(SV.index)))
}


## Problem 8 - 10
P810 = function(N, run = 1000){
	Eout.PLA.acc = numeric(run)
	Eout.SVM.acc = numeric(run)
	SVM.Wins = numeric(run)
	N.SV.acc = numeric(run)
	for(i in 1:run){
		generated = data.generate(N)
		PLA.result = PLA(generated)
		SVM.result = SVM(generated)
		test.x1 = runif(20000, -1, 1)
		test.x2 = runif(20000, -1, 1)
		test.y = as.numeric(test.x1 * generated$slope + generated$intercept > test.x2) * 2 - 1
		pred.PLA = sign(cbind(1, test.x1, test.x2) %*% PLA.result$w)
		pred.SVM = sign(cbind(test.x1, test.x2) %*% SVM.result$w + SVM.result$b)
		Eout.PLA = sum(pred.PLA != test.y) / 20000
		Eout.SVM = sum(pred.SVM != test.y) / 20000
		Eout.PLA.acc[i] = Eout.PLA
		Eout.SVM.acc[i] = Eout.SVM
		if(Eout.SVM < Eout.PLA) SVM.Wins[i] = 1
		N.SV.acc[i] = SVM.result$N.SV
	}
	Eout.PLA.mean = mean(Eout.PLA.acc)
	Eout.SVM.mean = mean(Eout.SVM.acc)
	SVM.Win.Rate = mean(SVM.Wins)
	N.SV.mean = mean(N.SV.acc)
	return(list(Eout.PLA.mean = Eout.PLA.mean, Eout.SVM.mean = Eout.SVM.mean, SVM.Win.Rate = SVM.Win.Rate, N.SV.mean = N.SV.mean))
}

N10.result = P810(10)
N100.result = P810(100)
N10.result
N100.result