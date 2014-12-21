## The below functions are used to create a special object
## that stores a matrix and caches its inverse.


## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing the following four functions:
##     setMatrix (sets the value of the matrix)
##     getMatrix (gets the value of the matrix)
##     setInverse (sets the value of the matrix's inverse)
##     getInverse (gets the value of the martix's inverse)
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


## The cacheSolve function returns the inverse of the special "matrix" that is created
## with the above function. It first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it computes the
## inverse and stores it in the cache via the setInverse function.
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
