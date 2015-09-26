## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix has a function to set the value of the Matrix
##get the value of the Matrix, set the value of the inverse of the matrix and finally get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function returns the inverse of the matrix.1st step to check if the inverse is already calculated.
##if yes,then get the result and return, otherwise compute set the value in the cache via the set inverse function.

ccacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("fetching cache data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
