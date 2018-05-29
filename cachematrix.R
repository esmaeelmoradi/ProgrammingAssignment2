##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(myMatrix = matrix()){
	#set a null value for the inverse value
	inv.matrix <- NULL
	
	#set the value of myMatrix and reset the inverse value
	set <- function(A){
		myMatrix <<- A
		inv.matrix <<- NULL
	}
	
	#check the matrix
	get <- function() myMatrix
	
	#set the inverse value
	setInv <- function(inverse) inv.matrix <<- inverse
	
	#check the inverse value
	getInv <- function() inv.matrix
	
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


##This function computes the inverse of the special "matrix"
cacheSolve <- function(myMatrix, ...){
	#checking the existing inverse value
	inv.matrix <- myMatrix$getInv()
	
	if(!is.null(inv.matrix)){
		message("getting cached data")
		return(inv.matrix)
	}
	
	mtrx <- myMatrix$get()
	
	#the matrix should be assigned in some time
	if(is.na(mtrx)){
		message("The matrix inside myMatrix has not been set yet!")
		return()
	}
	#matrix needs to be inversable
	else if(nrow(mtrx) != ncol(mtrx) || det(mtrx) == 0){
		message("The matrix is not inversable!")
		return()
	}
	#calculate the inverse and set the value for that
	else{
		inv.matrix <- solve(mtrx)
		myMatrix$setInv(inv.matrix)
		return(inv.matrix)
	}
}
