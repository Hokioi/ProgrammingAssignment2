## The following two functions calculate and cache the inverse of a matrix.
## Once the inverted matrix has been calculated, subsequent calls of these functions retrieve 
## it from the cache, rather than recalculating it.


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It creates an enclosed environment with variables:
## y = your matrix
## m = an empty variable, or the inverse of y if it has been calculated by cacheSolve
## It returns 4 functions into the global environment as a list:
## set (which sets the value of the matrix)
## get (which retrieves the value of the matrix)
## setinverse (which stores the value of the inverted matrix)
## getinverse (which retrieves the value of the inverted matrix)

##makeCacheMatrix assumes that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
  
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
  
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), cacheSolve 
## will retrieve the inverse from the cache instead of recalculating it.

## cacheSolve assumes that the matrix supplied via is always invertible.

cacheSolve <- function(x, ...) {

    m <- x$getinverse()

    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...) 
    x$setinverse(m)
    m
}
