## Functions allow for repeated calculation of a matrix inverse
## to be avoided by calculating the inverse once and caching the value


## Function caches the inverse of a matrix
## Nested list of functions allow:
##  1. the value of matrix to be reset
##  2. retrieval of the matrix
##  3. setting the value of the matrix inverse
##  4. retrieval of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
      x <<- y
      x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) x_inv <<- inverse
  getinv <- function() x_inv
  list(set = set, get = get,
  setinv = setinv,
  getinv = getinv)
}


## Function gets the inverse of the matrix input if it has already been calculated
##  Otherwise, calculates matrix inverse and stores value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
      message("getting cached data")
      return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinv(x_inv)
  x_inv
}
