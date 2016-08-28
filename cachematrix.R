## Catching the Inverse of a Matrix
## The functions bellow pretend to cache the inverse of a given
## square matrix (supposed invertible) in order to avoid repeated
## computations.
## The scheme followed is similar to the example shown in the
## information of the assignment using a vector.

## This function generates a 'special' matrix to cache its inverse.
## It include 4 functions to set its value and its inverse and to get
## its entries and its inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
	invmat <- NULL
	set <- function(y) {
		x <<- y
		invmat <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) invmat <<- solve
	getInverse <- function() invmat
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


## We want to return the inverse matrix of 'x'. Now we are ready to check if the inverse has already been calculated, through the function above. If so, it's returned from the cache.

cacheSolve <- function(x, ...) {
        invmat <- x$getInverse()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(invmat)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
