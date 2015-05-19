## These functions compute and caches the inverse of a square matrix.

## Create a matrix object that stores its inverse in the cache.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## Compute the inverse of the matrix by calculation or from cache
## and return the inverse of the matrix.
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix,...)
    x$setmatrix(m)
    m
}

