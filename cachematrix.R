## This function generates a matrix that can be inversed

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mi <<- inverse
  getInverse <- function() mi 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function calculates the inverse of the matrix generated with makeCacheMatrix

cacheSolve <- function(x, ...) {
  mi <- x$getInverse()
  if(!is.null(mi)){
    message("getting cached data")
    return(mi)
  }
  mat <- x$get()
  mi <- solve(mat,...)
  x$setInverse(mi)
  mi
}
