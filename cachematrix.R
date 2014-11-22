## Matrix functions for maintaining a cached version of the inverse of the 
## matrix.  Given an R matrix object, makeCacheMatrix() creates the 
## cache matrix wrapper.  cacheSolve() takes a cache matrix wrapper and
## returns the result of solve() on the original matrix, using a cached
## version, if available.

## Use makeCacheMatrix to create a cache matrix wrapper object given an 
## underlying matrix object.
makeCacheMatrix <- function(matrix = matrix()) {
  ## The lazily-populated cached inverse field, NULL if not populated yet
  cachedInverse <- NULL
  
  ## Set a new underlying matrix value
  set <- function(newMatrix) {
    matrix <<- newMatrix
    cachedInverse <<- NULL
  }
  
  ## Return the underlying matrix
  get <- function() matrix
  
  ## Store the cached inverse value
  setinverse <- function(inverse) cachedInverse <<- inverse
  
  ## Return the cached inverse value
  getinverse <- function() cachedInverse

  ## Return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Use cacheSolve to return the inverse of the given cache matrix object, 
## which was created with makeCacheMatrix.  Will return a native matrix, 
## using a cached version, if available.  The first call to cacheSolve will 
## perform the full solve computation and cache the result.  Subsequent calls 
## to cacheSolve will use the cached value of the inverse.
cacheSolve <- function(x, ...) {
  ## Try getting a cached version
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## Cache hit - return the cached value
    message("getting cached data")
    return(inverse)
  }
  
  ## Cache miss - retrieve the underlying matrix and compute inverse 
  ## with solve()
  matrix <- x$get()
  inverse <- solve(matrix)
  
  ## Cache the inverse
  x$setinverse(inverse)
  inverse
}
