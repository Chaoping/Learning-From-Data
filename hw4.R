growth = function(dvc, N, approximate = TRUE){
	if(dvc >= N){
		return(2^N)
	}
	if(approximate == TRUE){
		return(N^dvc)
	} else {
		H = 0
		for(i in 0:dvc) H = H + choose(N, i)
		return(H)
	}
}

Omega = function(dvc, N, delta = 0.05, bound = "VC"){
	if(bound == "VC"){
		return(sqrt(8/N * log(4 * growth(dvc, 2 * N)/delta)))
	}else if(bound == "RP"){
		return(sqrt(2 * log(2 * N * growth(dvc, N)) / N) + sqrt(2/N * log(1/delta)) + 1/N)
	}else if(bound == "Parrondo"){
		return(sqrt(1/N*log(6*growth(dvc, 2 * N)/delta)+1/N^2)+1/N)
	}else if(bound == "Devroye"){
		return(sqrt((-2*(log(4) + 2*dvc*log(N) - log(delta))+N*(log(4) + 2*dvc*log(N) - log(delta))+2)/(N-2)^2)/sqrt(2)+1/(N-2))
	}else {
		stop("Invalid bound type. Supported types are \"VC\", \"RP\", \"Parrando\" and \"Devroye\".")
	}
}

#Problem 1
N.vector = c(400000,420000,440000,460000,480000)
lapply(N.vector, dvc = 10, delta = 0.05, FUN = Omega)

#Problem 2
bound.vector = c("VC","RP","Parrondo","Devroye")
lapply(bound.vector, N = 10000, dvc = 50, delta = 0.05, FUN = Omega)

#Problem 3
bound.vector = c("VC","RP","Parrondo","Devroye")
lapply(bound.vector, N = 5, dvc = 50, delta = 0.05, FUN = Omega)