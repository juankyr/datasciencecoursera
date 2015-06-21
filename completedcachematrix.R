## Below are two functions that are used to create a special object that stores a matrix and 
## caches its inverse.

## The first function, makeCacheMatrix creates a special matrix, 
## a list containing a function to:
## 1)set value of matrix (set_matrix)
## 2)get value of matrix (get_matrix)
## 3)set value of inverse (set_inverse)
## 4)get value of inverse (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 					## Initially assign NULL to inverse
	set_matrix <- function(y) {			
		x <<- y 					## Set the matrix x
		inverse <<- NULL
	}
	get_matrix <- function() x 				## Return matrix x
	set_inverse <- function(solve) inverse <<- solve 	## Cache the value of the inverse 
	get_inverse <- function() inverse 			## Return inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## The following function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the 'set_inverse' function.

cacheSolve <- function(x, ...) {				## Return a matrix that is the inverse of x
	inverse <- x$get_inverse()				## Get inverse
	if(!is.null(inverse)) {					## Check for the presence of inverse
		message("getting cached data")			## Display message
		return(inverse)
	}
	data <- x$get_matrix()					## Get Matrix
	inverse <- solve(data, ...)				## Compute inverse
	x$set_inverse(inverse)					## Cache the inverse
	inverse 						## Return the inverse
}
