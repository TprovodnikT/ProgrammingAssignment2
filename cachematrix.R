## Put comments here that give an overall description of what your
## functions do

## This functions find inversion of the matrix, if it is possible, save it into cache and restore it

## Write a short comment describing this function
## function makeCacheMatrix have tools to save matrix, get it, get cached inverse
## matrix and set it
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mat) {
		x <<- mat
		inv <<- NULL
	}
	get <- function() x
	setInversion <- function(inverseMat) {
	    inv <<- inverseMat
	    inv <<- inverseMat
	}
	getInversion <- function() {
	    inv
	} 
	list(set = set, get = get,
		setInversion = setInversion,
		getInversion = getInversion)
}


## Write a short comment describing this function
## function cacheSolve checks, if we already have cached inverse matrix, and if
## no, than it tries to find it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInversion()
		if(!is.null(inv)) {
			message("getting cached inversion")
			return(inv)
		}
		data <- x$get()
		isMatrixSuitable <- isMatrixSuitableForInversion(x) 
		if(isMatrixSuitable == TRUE) {
			inv <- solve(data, ...)
			x$setInversion(inv)
			inv
		} else {
		    isMatrixSuitable	
		}
}

## This function checks, if our object is a  matrix and if it
## is suitable for inversion.
## If no, then it return messages and does not raise error

isMatrixSuitableForInversion <- function(x) {
    data <- x$get()
	if(!is.matrix(data)) {
		return ("Your object is not a matrix")
	}
	if(nrow(data) != ncol(data)) {
	    return ("Matrix must be square")
	}
	if(det(data) == 0) {
	    return ("Matrix determinant is 0")
	}
    TRUE
}
