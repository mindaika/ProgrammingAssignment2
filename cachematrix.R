## I will do no such thing.
##

## Takes a matrix "x" and creates a blank matrix of the same dimensions
## in which the inverse will be stored, as well as caching the original
## to determine equality of input.

makeCacheMatrix <- function(x = matrix()) {
    if (dim(x)[1] != dim(x)[2]) {
        print("The input matrix must be square.")
        return
    }
    if (exists("cacheOriginal") && matequal(x, cacheOriginal)) {
        cacheMatrix
        return
    } else {
        cacheOriginal <<- x
        cacheMatrix <<- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
    }
    cacheMatrix
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    if (exists("cacheMatrix") &&
        !is.na(cacheMatrix[1]) &&
        exists("cacheOriginal") && matequal(x, cacheOriginal)) {
        print("Returning cached value")
    } else {
        print("Caching...")
        makeCacheMatrix(x)
        cacheMatrix <<- solve(x)
    }
    cacheMatrix
}


## Checks equality of matrices. 
matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}