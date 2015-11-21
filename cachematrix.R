## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        
        # Define a local 
        getInverse <- function() i
        
        list(set = set,
             get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. 
##
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        # L
        i <- x$getInverse()
        
        # If 'i' is not Null, then this means that it has been found in the cache.
        if(!is.null(i)) {
                message ("getting cached data")
                return (i)
        }
        data <- x$get()
        
        ## Call the 'solve' function to calculate the matrix inverse
        i <- solve(data, ...)
        
        ## Set the inverse matrix 
        x$setInverse(i)
        
        # Return a matrix that is the inverse of 'x'. This is given by the
        # variable 'i'
        i
}
