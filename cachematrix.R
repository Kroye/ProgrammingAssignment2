## Assignment 2 -- Karoly Sumegi
## The makeCacheMatrix creates a special matrix which is a list
## containing set and get of the matrix
## containing set and get of the inverse of the matrix
## The cacheSolve will solve the matrix inversion if it is not cached already
## using the solve function

## makeCacheMatrix to set and get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
## the solve could have been written in an else
## so it we don't have to operate with the return skip
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <-x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
