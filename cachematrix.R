## Caching inverse of a matrix (potentially time-consuming
## caluclation, so result is cached for repeat use).

## Creates a cache matrix object to store results of previous
## calculations of inverse. Object consists of 4 accessor functions
## for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) m <<- solve
	getSolve <- function() m
	list(set = set, 
		 get = get, 
		 setSolve = setSolve, 
		 getSolve = getSolve)
}


## Returns a matrix inverse - either from cache if computed earlier,
## or computes it implicitly and caches for later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ## Check if available from cache
        m <- x$getSolve()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        ## Compute as it's not available from cache
        data <- x$get()
        m <- solve(data,...)
        x$setSolve(m)
        m
}
