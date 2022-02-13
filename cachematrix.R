## The function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(mtrx = matrix()) {
	slv <- NULL

	set <- function(m) {
		mtrx <<- m
		slv <<- NULL
	}

	get <- function() mtrx
	setSolve <- function(s) slv <<- s
	getSolve <- function() slv
	list(set = set,
	     get = get,
	     setSolve = setSolve,
	     getSolve = getSolve)
}


## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(cmtrx, ...) {
	slv <- cmtrx$getSolve()

	if (!is.null(slv)) {
		message("getting cached data")
		return(slv)
	}

	data <- cmtrx$get()
	slv <- solve(data, ...)
	cmtrx$setSolve(slv)
	slv
}
