## This assignment is to write a pair of functions that cache the inverse 
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y 
	inv <<- NULL	
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
	message("Getting cached data")
	return(inv)	
	}
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

## Testing the functions
mt <- matrix(1:4, 2, 2)
mt2 <- makeCacheMatrix(mt)
cacheSolve(mt2)
##    [,1]   [,2]
##[1,] -2     1.5
##[2,]  1    -0.5

