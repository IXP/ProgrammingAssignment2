## 
## functions do

## Creates a 'matrix' that can cache the inverse of a matrix. But, you need to call cacheSolve to actually get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invrs) inv <<- invrs
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## You can pass a 'matrix' of type 'makeCacheMatrix' into cacheSolve, which will either calculate and return the inverse of the matrix, or return the cached inverse of the matrix. 



cacheSolve <- function(x, ...) {
        inv <- x$getinverse
        if(!isnull(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}
