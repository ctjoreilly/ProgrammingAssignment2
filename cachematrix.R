# This file contains a collection of functions (including a testing function) 
# that provide an implementation of Programming Assignment 2 described at:
# https://github.com/rdpeng/ProgrammingAssignment2
#
# Basic usage example is:
# > cachedMatrix          <- makeCacheMatrix(matrix(c(7,4,9,3),nrow=2,ncol=2))
# > inverseOfCachedMatrix <- cacheSolve(cachedMatrix)
#
# To run predefined tests:
# > testCacheMatrix()
#
# Note: Function comment blocks are written based on Google's R Style Guide:
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#functiondocumentation

makeCacheMatrix <- function(x = matrix()) {
    # Make a list of functions for caching and working with a matrix.
    #
    # Args:
    #   x: a matrix to be cached (for assignment assumed to be invertible).
    #
    # Returns:
    #   A list containing 4 functions for interacting with the cached matrix:
    #     list$get       : Get the cached matrix.
    #     list$set       : Set the matrix to be cached. 
    #                      Will also set the cached inverse associated with 
    #                      the cached matrix to NULL.
    #     list$getinverse: Get the inverse associated with the cached matrix
    #                      via 'setinverse'
    #     list$setinverse: set the inverse to be associated with the cached 
    #                      matrix.
    cachedInverse <- NULL
    # Define functions for setting and getting the cached matrix 'x'.
    get <- function() { 
        x 
    }
    set <- function(y) {
        x             <<- y
        cachedInverse <<- NULL  # will need to re-compute in cacheSolve()
    }
    # Define functions for setting and getting the cached matrix's inverse.
    getinverse <- function() { 
        cachedInverse 
    }
    setinverse <- function(inverse) { 
        cachedInverse <<- inverse 
    }
    # Return the list of functions for working with the cached matrix.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {   
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
    #
    # Args:
    #   x  : a cached matrix constructed from a call to makeCacheMatrix().
    #   ...: additional arguments passed to solve() in addition to the 
    #        cached matrix contained in x.
    #
    # Returns:
    #   A matrix that is the inverse of the cached matrix contained in 'x'.

    # Attempt to get the cacched inverse of the cached matrix
    inv <- x$getinverse()
    if(!is.null(inv)) {
        # Indicate to the user cached value is being used
        message("getting cached data") 
    } else {
        # Cached inverse does not exist, therefore compute it
        # and cache it for future calls.
        matrix <- x$get()
        inv    <- solve(matrix, ...)
        x$setinverse(inv)
    }
    # Return the inverse of the cached matrix.
    inv
}

testCacheMatrix <- function() {
    # Implements reproducible tests
    #
    # Args:
    #   None.
    #
    # Returns:
    #   None.
    print("Test 1: Will NOT print 'getting cached data")
    cm <- makeCacheMatrix(matrix(c(7,4,9,3),nrow=2,ncol=2))
    print("Inverse = ") 
    print(cacheSolve(cm))
    print("Test 2: Will print 'getting cached data")
    print("Inverse = ") 
    print(cacheSolve(cm))
}
