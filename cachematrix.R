## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_x <<- inverse
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
        }
    inv_x <- solve(x$get())
    x$setinv(inv_x)
    return(inv_x)
}
