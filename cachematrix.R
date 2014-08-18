
makeCacheMatrix <- function(x = matrix()) {
    # 
    # Returns a list object with 4 functions
    #   to enable you to store a matrix x
    #   and also store (cache) its inverse 
    #   once the value for the inverse is supplied via setinverse(inv).
    #   It then makes the inverse available with getinverse()
    #   (which returns NULL until then).
    #
    # Args:
    #   x: A normal matrix.
    #
    # Returns:
    #   A list of four functions.  The list object can effectively be
    #      treated as a "wrapper" to the underlying matrix x, caching its inverse.
    #  The four functions in the returned list are:
    #    set(x) : store the matrix x, and initialize the inverse to NULL
    #      to indicate not yet known
    #    get()  : returns the matrix x
    #    setinverse(inv) : stores the supplied value of the inverse
    #    getinverse()    : returns the inverse if that is already stored, 
    #      else returns NULL 
    #
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

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    # either by finding its inverse in the cache
    # or by calling solve(x) and *storing* the inverse in the cache,
    # then (either way) returning the inverse of x
    #
    # Args:
    #   x: A 'cacheMatrix' object that consists of 4 function 
    #       to get and set the underlying matrix x 
    #       and its inverse for caching purposes
    # 
    # Returns:
    #   a matrix that is the inverse of 'x'
    #
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        inv
    } else {
        matrix_x <- x$get()
        inv <- solve(matrix_x)
        x$setinverse(inv)
        inv
    }
}
