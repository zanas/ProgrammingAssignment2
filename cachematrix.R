####################################################################
# This file contains two functions that will be used to compute    #
# the inverse of a matrix then cache it into a specific structure  #
# called 'CacheMatrix'                                             #
####################################################################

makeCacheMatrix <- function(m = matrix()) {
    # Creates the CacheMatrix structure that will hold a matrix and
    # its inverse, and provide four functions to manipulate them.
    #
    # Args:
    #   m: The matrix we want to cache its inverse.
    #
    # Returns:
    #   A CacheMatrix structure that holds m and its inverse
    
    inverse <- NULL
    
    # Function that sets the matrix to invert
    set <- function(matrixToInvert) {
        m <<- matrixToInvert
        inverse <<- NULL
    }
    
    # Function that returns the matrix to invert
    get <- function() m
    
    # Function that sets the inverted matrix
    setInverse <- function(inv) inverse <<- inv
    
    # Function that returns the inverted matrix
    getInverse <- function() inverse
    
    # The structure "CacheMatrix" which link a matrix and its inverse 
    # through the above functions
    CacheMatrix <- list(set = set, get = get,
                        setInverse = setInverse,
                        getInverse = getInverse)
    
    CacheMatrix
}

cacheSolve <- function(cacheMatrix, ...) {
    # Computes the inverse of the matrix embedded in the cacheMatrix structure.
    #
    # Args:
    #   cacheMatrix: A Structure that contains the matrix to be inverted and that
    #   will hold the resulting inverted matrix.
    #
    # Returns:
    #   The inverse of the matrix.
    
    # First, search of a previously cached inverse
    inverse <- cacheMatrix$getInverse()
    
    # If there is one, then return it 
    if(!is.null(inverse)) {
        message("getting the cached inverse")
        return(inverse)
    }
    
    # Otherwise, get the matrix to be inverted
    matrixToInvert <- cacheMatrix$get()
    
    # Invert it
    inverse <- solve(matrixToInvert)
    
    # Then cache it
    cacheMatrix$setInverse(inverse)
    
    # And finally return it
    inverse
}
