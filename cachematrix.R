## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Input: x as a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_aux <- NULL
  set <- function(y) {
    x <<- y
    inv_aux <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_aux <<- inverse
  getinv <- function() inv_aux
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Input x as a matrix
# Output y as inverted matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_aux <- x$getinv()
  if(!is.null(inv_aux)) {
    message("get cache result.")
    return(inv_aux)
  }
  data <- x$get()
  inv_aux <- solve(data, ...)
  x$setinv(inv_aux)
  message("set cache result.")
  inv_aux
}

m <- matrix(rnorm(9),3,3)
m_inv <- makeCacheMatrix(m)
cacheSolve(m_inv)
