## Put comments here that give an overall description of what your
## functions do

## These functions build a matrix in a special form that is suitable for 
## calculation and caching the inverse of the matrix. The functions are 
## makeCacheMatrix and cacheSolve, which build the matrix and calculate the
## inverse, respectively.

## Write a short comment describing this function

## makeCacheMatrix creates a special 'matrix' that contains a list of functions that:
## a) Set the value of the matrix
## b) Get the value of the matrix
## c) Set the value of the inverse
## d) Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(locInverse) inverse <- locInverse
	getInverse <- function() inverse
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve returns the inverse of a 'matrix' if it has already been
## calculated; otherwise, it calculates the inverse.
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message('getting cached data')
		return(inverse)
	}
	tempMatrix <- x$get()
	inverse <- solve(tempMatrix, ...)
	x$setInverse(inverse)
	inverse
}
