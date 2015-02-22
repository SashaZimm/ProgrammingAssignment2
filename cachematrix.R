## The following 2 functions are responsible for:

## 1) Creating an object for a matrix, with an inbuilt cache for its calculated inverse.
## 2) Using the object to either return the cached inverse, or to calculate the inverse,
##    update the inverse in the cache, and then return the inverse.


## Creates the object for a matrix that can cache its inverse, once calculated. 

makeCacheMatrix <- function(x = matrix()) {

    # Cache for the inverse
    i <- NULL
    
    # Sets the matrix for the object and the cached inverse to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Returns the matrix
    get <- function() x
    
    # Set the cached inverse
    setinverse <- function(inverse) i <<- inverse
    
    # Return the cached inverse (can be null)
    getinverse <- function() i
    
    # Return the matrix object with the caching ability
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of a matrix, and sets in the cache if not present.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the inverse from the cache (can be null)
    i <- x$getinverse()
    
    # If found in cache print message and return inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Inverse is not found in the cache, proceed to calculate
    matrix <- x$get()
    i <- solve(matrix, ...)
    
    # Put the calculated inverse in the cache 
    x$setinverse(i)
    
    # Return the inverse
    i
}