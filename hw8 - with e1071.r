
## Get and preprocess the data
features.train = read.table("http://www.amlbook.com/data/zip/features.train")
features.test = read.table("http://www.amlbook.com/data/zip/features.test")
train.X = features.train[c(2,3)]
train.y = features.train[1]
test.X = features.test[c(2,3)]
test.y = features.test[1]

## A Versus B classification
OVN = function(A, B, kernel, Q, C, toprint = TRUE, ...){
	if(B == "all"){
		train.y = as.numeric((train.y == A) * 2 - 1)
		test.y = as.numeric((test.y == A) * 2 - 1)
	} else if(B %in% 0:9) {
		subindex.train = train.y == A | train.y == B
		subindex.test = test.y == A | test.y == B
		train.X = train.X[subindex.train,]
		train.y = train.y[subindex.train]
		test.X = test.X[subindex.test,]
		test.y = test.y[subindex.test]
		train.y = as.numeric((train.y == A) * 2 - 1)
		test.y = as.numeric((test.y == A) * 2 - 1)		
	}
	require(e1071)
	model = svm(x = train.X, y = as.integer(train.y), kernel = kernel, degree = Q, cost = C, coef0 = 1, gamma = 1,scale = FALSE, ...)
	E.in = sum(predict(model) != train.y) / length(train.y)
	E.out = sum(predict(model, newdata = test.X) != test.y) / length(test.y)
	N.SV = length(model$index)
	if(toprint == TRUE){
		cat("\n", A," versus ",B," classifier using ",kernel," kernel with Q = ", Q,", C = ",C,":\n	E.in = ", 
			E.in,"\n E.out = ",E.out,"\n Number of SV's = ",N.SV, "\n",sep = ""
			)
	}
	invisible(list(E.in = E.in, E.out = E.out, N.SV = N.SV))
}

## Problem 2 - 4
for(A in (0:4)* 2){
	OVN(A, "all", type = "C-classification", kernel = "polynomial", Q = 2, C = 0.01 )
}
for(A in (0:4)* 2 + 1){
	OVN(A, "all", type = "C-classification", kernel = "polynomial", Q = 2, C = 0.01 )
}

## Problem 5
for(C in c(0.001, 0.01, 0.1, 1)){
	OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 2, C = C)
}


## Problem 6
#[a]
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 2, C = 0.0001)
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 5, C = 0.0001)

#[b]
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 2, C = 0.001)
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 5, C = 0.001)

#[c]
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 2, C = 0.01)
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 5, C = 0.01)

#[d]
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 2, C = 1)
OVN(1, 5, type = "C-classification", kernel = "polynomial", Q = 5, C = 1)

## Cross Validation on C
CV = function(A, B, Cset, k, runs = 100){
	if(B == "all"){
		train.y = as.numeric((train.y == A) * 2 - 1)
		test.y = as.numeric((test.y == A) * 2 - 1)
	} else if(B %in% 0:9) {
		subindex.train = train.y == A | train.y == B
		train.X = train.X[subindex.train,]
		train.y = train.y[subindex.train]
		train.y = as.numeric((train.y == A) * 2 - 1)	
	}
	y = as.integer(train.y)
	X = train.X
	N = length(y)
	E.cv.all.runs = matrix(0, runs, length(Cset))
	for(i in 1:runs){
		shuffle = sample(N)
		X.shuffled = X[shuffle,]
		y.shuffled = y[shuffle]
		E.cv = matrix(0,k,length(Cset))
		for(fold in 1:k){
			cv.index = round((fold - 1) * N/k):round(fold*N/k)
			X.minus = X.shuffled[-cv.index,]
			y.minus = y.shuffled[-cv.index]
			X.val = X.shuffled[cv.index,]
			y.val = y.shuffled[cv.index]
			E.cv.fold = numeric(length(Cset))
			for(C.index in 1:length(Cset)){
				model = svm(x = X.minus, y = y.minus, kernel = "polynomial", degree = 2, cost = Cset[C.index], coef0 = 1, gamma = 1,scale = FALSE, type = "C-classification")
				E.cv.fold[C.index] = sum(predict(model, newdata = X.val) != y.val)/length(y.val)
			}
			E.cv[k,] = E.cv.fold	
		}
		E.cv.all.runs[i,] = colMeans(E.cv)
	}
	colnames(E.cv.all.runs) = Cset
	selected = Cset[apply(E.cv.all.runs, 1, which.min)]
	return(list(E.cv.all.runs = E.cv.all.runs, selected = selected))
}

## Problem 7 & 8
result = CV(1,5, c(0.0001,0.001,0.01,0.1,1), 10)
table(result$selected)
colMeans(result$E.cv.all.runs)

## Problem 9 & 10
for(C in c(0.01, 1, 100, 1e4, 1e6)){
	OVN(1, 5, type = "C-classification", kernel = "radial", Q = 2, C = C)
}