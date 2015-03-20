## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
## This function saves the matrix to cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
  }

## This function get the matrix from cache
    get <- function() x

## This function saves the inverse matrix to cache
    setinverse <- function(inverse) inv <<- inverse

## This function get the inverse matrix from cache
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {

## check if the inverse of 'x' already saved to cache
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
  
## If the inverse matrix does not exist in calculate it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
