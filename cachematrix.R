## Put comments here that give an overall description of what your
## functions do

## These functions build a matrix in a special form that is suitable for 
## calculating and caching the inverse of the matrix. The functions are 
## makeCacheMatrix and cacheSolve, which build the matrix and calculate the
## inverse, respectively.

## Write a short comment describing this function

## makeCacheMatrix creates a special 'matrix' that contains a list of functions that:
## a) Set the value of the matrix
## b) Get the value of the matrix
## c) Set the value of the inverse
## d) Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	
	# Start an empty matrix to hold the inverse value
	inverse <- NULL

	# This function sets the value of a new matrix, 
	# and clears the previous cached inverse matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# This function gets you the value of the stored matrix
	get <- function() x

	# This function sets the value of the inverse matrix in the cache	
	setInverse <- function(externalInverse) inverse <- externalInverse

	# This function gets the inverse matrix stored in the cache
	getInverse <- function() inverse

	# Return a list with functions to set and get the matrix
	# and its inverse, respectively.
	list(set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve returns the inverse of a 'matrix' if it has already been
## calculated; otherwise, it calculates the inverse.
cacheSolve <- function(x) {

	# Check if the inverse is stored in the cache.
	# If so, return the cached inverse.
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message('getting cached data')
		return(inverse)
	}
	
	# In case the inverse is not cached, let's calculate the inverse.
	# Get the cached matrix in x.
	tempMatrix <- x$get()

	# Calculate the inverse matrix
	inverse <- solve(tempMatrix)

	# Store inverse matrix in the cache so it is not calculated again.
	x$setInverse(inverse)

	#Return the calculated inverse matrix
	inverse
}
