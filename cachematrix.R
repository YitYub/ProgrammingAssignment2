## Functions makeCacheMatrix and cacheSolve are used in combinations to compute inverse of matrices
## Cached inverse of matrices will not be recalculated



## Returns an object with a matrix and a list of functions to modify the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	# Set a new matrix and reset the inverse
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	# Set and Get functions
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Takes an object from makeCachedMatrix and return the inverse of a matrix
## The inverse is either calculated or returned from cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # If inverse is not null, get previously  calculated inverse
    if (!is.null(inv)){
    	message("getting cached data")
    	return(inv)
    }
    # Else calculate new inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv 	## Return a matrix that is the inverse of 'x'
}
