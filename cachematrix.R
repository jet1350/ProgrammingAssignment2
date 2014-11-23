## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The functions here will provide the caching mechanism
## to the inverse of a matrix, and avoid to compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Create a list containing a function to
    ## 1,set the value of the matrix
    ## 2,get the value of the matrix
    ## 3,set the value of inverse
    ## 4,get the value of inverse
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data of the inverse")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
