## Function creates special matrix that can hold it's own inverse
## once it has been calculated. The inverse has only to be
## recalculated if the original matrix is changed.
## It is assumed that the matrix supplied is always invertible
makeCacheMatrix <- function(x=matrix()) {
  # initialize the inverse with NULL to check if already calculated
  i <- NULL
  # Define get and set-function for matrix and its inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() (x)
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  # return "special matrix" that is able to remember it's inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Function calculates Inverse of special matrix.
## If Inverse has already been calculated, it returns
## the cached result.
## @param x   a "special matrix"
## @return    inverse of matrix
cacheSolve <- function(x,...) {
  # retrive inverse matrix from "special matrix"
  i <- x$getinverse()
  # check whether whether inverse is set
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # calculate inverse if not set
  data <- x$get()
  i <- solve(data,...)
  # add inverse to "special matrix"
  x$setinverse(i)
  # return calculated inverse
  i
}
