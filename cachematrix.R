## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix "object", which is a list of functions to set and get the 
## values of the matrix itself and its inverse (assumes matrix is square!). 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(nx) {
        x <<- nx
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Returns the inverse of a matrix "object" ('x'), created with the 
## 'makeCacheMatrix' function. If the inverse has been previously calculated 
## and threfore cached inside the matrix 'x', the cached value is returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    return(inv)
}

