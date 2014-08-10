## Put comments here that give an overall description of what your
## functions do

## 
## makeCacheMatrix is a function that takes a matrix x
## and stores its inverse when setinverse is supplied
## the value for the inverse
## it then makes it available with getinverse
## (which returns NULL until then)
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
## cacheSolve(x,...) takes a cachematrix x, 
## either finds its inverse in the cache
## or calls solve(x) and *stores* the inverse in the cache,
## then (either way) returns the inverse of x
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverion
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        } else {
            matrix_x <- x$get()
            inv <- solve(matrix_x)
            x$setinverse(inv)
            inv
        }
}
