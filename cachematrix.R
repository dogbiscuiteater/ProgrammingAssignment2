## Two functions are defined here, makeCacheMatrix and cacheSolve.
## The makeCacheMatrix function creates a special type of matrix that can 
## cache its inverse, while the cacheSolve function looks for an inverse of 
## a given square invertible matrix.
## 
## Example usage
##
## x <- matrix(c(1,2,3,4, 4, 5,1,3,5), nrow=3, ncol=3)
## z <- cacheSolve(makeCacheMatrix(x))
## 
## The variable 'z' returned here will be the inverse of 'x'.
## 



## The makeCacheMatrix function takes a matrix as a parameter.
## 
## The function returns the special "matrix" that can cache its inverse. 
## This special "matrix" is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse.
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Define functions for getting and setting the matrix and inverse.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        
        ## Return the list containing functions for getting and setting matrix 
        ## and inverse.
        list(set = set,
             get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}


## This function computes the inverse of a matrix, taking the list returned by
## the makeCacheMatrix function as its parameter
##
## It returns the the inverse of the matrix, retrieving it from the cache if it
## exists or calculating the value and caching it if it has not yet been cached.
##
## Note that this function will only work properly if the matrix stored in
## parameter 'x' is square and invertible.
##
cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        
        ## If 'i' is not NULL, then it has been found in the cache.
        if(!is.null(i)) {
                message ("getting cached data")
                return (i)
        }
        data <- x$get()
        
        ## Call the 'solve' function to calculate the matrix inverse.
        i <- solve(data, ...)
        
        
        ## Cache the inverse matrix
        x$setInverse(i)
        
        ## Return a matrix that is the inverse of 'x'. 
        i
}
