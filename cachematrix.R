## Two functions that together compute, cache, and retrieve the
## inverse of a function

## makeCacheMatrix is a function that sets and gets the value of a
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) im <<- solve
	getmatrix <- function() im
	list(set=set, get=get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}


## cacheSolve checks to see if the inverse of the matrix created by
## makeCacheMatrix has already been computed. If it has, it gets 
## this from the cache. If not, it computes it and caches it.

cacheSolve <- function(x=matrix(), ...) {
        im <- x$getmatrix()
        if(!is.null(im)) {
        	message("getting cached data")
        	return(im)
        }
        matrix <- x$get()
        im <- solve(matrix, ...)
        x$setmatrix(im)
        im
}
