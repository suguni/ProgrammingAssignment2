## Matrix inversion is usually a costly computation.
## Following functions create special matrix object and cache
## inversed matrix.

## Create a speccial "matrix" object that can cahe its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(iv) inverse <<- iv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	
	if (!is.null(inverse)) {
		message('getting cached data')
		return(inverse)
	}
	
	data <- x$get()
	identity <- diag(1, dim(data))
	inverse <- solve(data, identity, ...)
	x$setinverse(inverse)
	inverse
}

