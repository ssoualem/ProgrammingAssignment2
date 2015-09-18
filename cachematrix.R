## Functions used to cache the inverse of a matrix to speed up processing time

## Returns a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) inv <<- inverse
	
	getInverse <- function() inv
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Retrieves the cached matrix inverse if it exists. If not the inverse is computed and cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	mat <- x$get()
	
	inv <- solve(mat)
	x$setInverse(inv)
	inv
}
