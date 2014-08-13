## 
## makeCacheMatrix is a function that returns a list object with 4 functions
## to enable you to store a matrix x
## and also store (cache) its inverse 
## once the value for the inverse is supplied via setinverse(inv).
## It then makes the inverse available with getinverse()
## (which returns NULL until then).
##
## set(x) : store the matrix x, and initialize the inverse to NULL
##      to indicate not yet known
## get() returns the matrix x
## setinverse(inv) stores the supplied value of the inverse
## getinverse()
##        returns the inverse if that is already stored, else
##        returns NULL 
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
        inv <- x$getinverse()
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
