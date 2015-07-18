## Function to cache a matrix inversion
## Example calling sequence
##    mat_x <- makeCacheMatrix(matrix(c(2:5),nrow=2,ncol=2))
##    cacheSolve(mat_x) ## First time, calc inverse
##    cacheSolve(mat_x) ## after first time, returns cached value


## Initializes the functions for the given
## matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  #
  # return the list of functions and names
  #
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## First time function is invoked, the inverse is NULL (not populated)
## So function calculates.  For all subsequesnt invocations, the cached inverse is returned, saving computation
## Assumes all matrices are invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
