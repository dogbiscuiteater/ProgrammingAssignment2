## Two functions are defined here, makeCacheMatrix and cacheSolve.
## The makeCacheMatrix function creates a special type of matrix that can 
## cache its inverse, while the cacheSolve function looks for an inverse of 
## a given matrix.
## 
## Example usage
##
## x <- matrix(c(1,2,3,4, 4, 5,1,3,5), nrow=3, ncol=3)
## z <- cacheSolve(makeCacheMatrix(x))
## 
## The variable 'z' returned here will be the inverse of 'x'.
## 



## The makeCacheMatrix function takes a standard matrix as a parameter, which may be created 
## with the matrix function (as described in the comment above.)
## 
## The function returns the special "matrix" that can cache 
## its inverse. This special "matrix" is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse.
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
        
        ## Return the special "matrix", which is really a list containing 
        ## the functions for getting and setting matrix and inverse.
        list(set = set,
             get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}


## This function computes the inverse of a special "matrix" returned by the
## makeCacheMatrix function. It takes a special "matrix" as a parameter, which is
## really a list as described in the comment to makeCacheMatrix() above.
##
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. Otherwise, it 
## calculates the matrix inverse using the solve function and puts it into the 
## cache.
##
## It returns the the inverse of the matrix.
##
## Note that this function will only work properly if the matrix stored in
## parameter 'x' is invertible.
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
        
        
        x$setInverse(i)
        
        ## Return a matrix that is the inverse of 'x'. 
        i
}
