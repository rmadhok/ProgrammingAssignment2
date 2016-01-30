## Taking the inverse of a matrix is typically fast, but not
## if you have do it repeatedly. It is more efficient to 
## save the inverse in a cache, and access it when needed
## instead of computing it each time

## makeCacheMatrix does the following:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of the inverse 
# 4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computed the inverse of the "special matrix" created
## by makeCacheMatrix. If the inverse has already been computed (and
## the matrix has not changed), it skips the computation. Otherwise,
## it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
