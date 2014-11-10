## Read in the Data
train = read.table("http://work.caltech.edu/data/in.dta", col.names = c("x1","x2","y"))
test = read.table("http://work.caltech.edu/data/out.dta", col.names = c("x1","x2","y"))

## Map X to Z
Phi = function(X) cbind(1, X[,1], X[,2],X[,1]^2,X[,2]^2,X[,1]*X[,2],abs(X[,1]-X[,2]),abs(X[,1]+X[,2]))
Z.train = Phi(train)
Z.test = Phi(test)

## Linear Regression
LR = function(X, y, lambda = 0){
	w = solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
	return(w)
}

## LR Classifier
LRC = function(X, w) sign(X %*% w)

## Classification Error
CE = function(pred.y, y) 1 - mean(pred.y == y)

## Problem 2
w = LR(Z.train, train[,3])
CE(LRC(Z.train, w), train[,3]) # in-sample error
CE(LRC(Z.test, w), test[,3]) # out-of-sample error

## Problem 3 - 6 
P36 = function(k, toprint = TRUE) {
	w = LR(Z.train, train[,3], 10^k)
	E.in = CE(LRC(Z.train, w), train[,3])
	E.out = CE(LRC(Z.test, w), test[,3])
	if(toprint == TRUE){
		cat("For k = ",k,": E.in = ",E.in, "; E.out = ", E.out, ".\n",sep = "")
		invisible(E.out)
	} else {
		return(E.out)
	}
}

P36(-3) # Problem 3
P36(3) # Problem 4
sapply(-2:2,P36) # Problem 5
sapply(-10:10, toprint = FALSE, P36) # Problem 6


## Problem 8 - 9
nweights = function(nnodes){
	nnodes = c(10,nnodes,2)
	weights.acc = 0
	for(i in 1:(length(nnodes) - 1)){
		weights.acc = weights.acc + nnodes[i] * (nnodes[i+1] - 1)
	}
	weights.acc
}

#########################
## nweights(rep(2,18)) ## These two structures of hidden units give the smallest and greatest answer. I actually tried them out, so no real analytic ways :(
## nweights(c(22,14))  ##
#########################



