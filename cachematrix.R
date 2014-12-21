## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	setMatrix <- function(new_matrix) {
		x <<- new_matrix
		i <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(
		setMatrix=setMatrix,
		getMatrix=getMatrix,
		setInverse=setInverse,
		getInverse=getInverse
	)		
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
