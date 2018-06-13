# cachematrix.R -->  This function will creates a "matrix" object that can cache its inverse in the same object
# By Bert-Jan Stroop 

makeCacheMatrix <- function(x = matrix()) {
	## innitiate a variable which will hold the cached inverse:
	inverse <- NULL 
	
	
	## define a set function to assign value of matrix from the higher parent space to the local space. In case of a new matrix reset the inverse
	set <- function(y) {                   
        x <<- y
        inverse <<- NULL 
    }
	
	## get function
	get <- function() x
	
	## define a set function to assign inverse
	setinv <- function(inv) inverse <<- inv
	
	## get function 
	getinv <- function() inverse
	
	## we want to refer to the data with the $ parameter, so we need a list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cachematrix.R -->  This function will use a "matrix" object, and return it's inverse, either from cached inverse, or from solved inverse.
# By Bert-Jan Stroop 

cacheSolve <- function(x, ...) {
	## try to load the cached inverse matrix
	inverse <- x$getinv()
	
	## if it does exist, then we can return it. 
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	## else we need to calculate it.
	data <- x$get()
	inverse <- solve(data, ...)

	## and store it to the cache for a next time
	x$setinv(inverse)
	
	## after storing the value can be returned
	inverse
}
