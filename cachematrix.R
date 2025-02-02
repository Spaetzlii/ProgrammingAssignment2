# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse 
# of a matrix.

# Write the following functions:
#   - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#     If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.


## This is a factory function that creates a "cacheable" object that holds both the matrix data (x) and the cached result (cache).
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL 
    
    set <- function(y) {
        x <<- y  
        cache <<- NULL 
    }
    
    get <- function() x
    
    setMatrix <- function(inversed_matrix) cache <<- inversed_matrix
    
    getMatrix <- function() cache
    
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## This function checks if the inversed matrix is already cached. If it is, it returns the cached value. 
## If it isn't, it computes the inverse matrix, caches it, and then returns the result.

cacheSolve <- function(x, ...) {
    inversed_matrix <- x$getMatrix()
    if (!is.null(inversed_matrix)) {  
        message("Getting cached inversed matrix")
        return(inversed_matrix) 
    }
    
    matrix <- x$get() 
    inversed_matrix <- solve(matrix)  
    x$setMatrix(inversed_matrix) 
    inversed_matrix
}
