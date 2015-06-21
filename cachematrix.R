# This function will create a matrix object that cache its inverse.
# It will create a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
# 
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse)
	getinverse <- function() inv
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the matrix created by makeCacheMatrix
# function.
#
# If the inverse has already been calculated, i.e, the matrix has not changed,
# then the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
