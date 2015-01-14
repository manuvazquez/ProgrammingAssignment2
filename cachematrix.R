## Functions to cache/compute the inverse of a matrix

## A function containing a matrix along with its inverse if previously computed

makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL
	
	set <- function(A){
		x <<- A
		inverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(givenInverse) inverse <<- givenInverse
	
	getInverse <- function() inverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## A function to compute the inverse of the matrix "object" defined above

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	inverse <- x$getInverse()
	
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	
	data <- x$get()
	inverse <- solve(data,...)
	x$setInverse(inverse)
	inverse
}

# test
# A <- c(1.31000415,-0.07867442,-0.26672719,1.12703183)
# dim(A) <- c(2,2)
# 
# cachedA <- makeCacheMatrix(A)
# inv <- cacheSolve(cachedA)