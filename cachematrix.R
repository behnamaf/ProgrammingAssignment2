## These functions are useful for bulding a special matrix which can be used
## for quickly returning its inverse from cache


## Creates a special data structure for Matrix which can be used for 
## caching inverse values

makeCacheMatrix <- function(x = matrix()) {
	  inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inverse <<- inv
        getInv <- function() inverse
        list(set = set, 
		 get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function returns the inverse of a matrix
## if the inverse is already calculated, it is returned from cache
## otherwise, it will be calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("Getting cached data ...")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
