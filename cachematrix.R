##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <-NULL <-function(y){
    x<<- y
    invr <<- NULL
  }
  get <<- function() {x}
  setInverse <- function(inverse) {invr <<- inverse}
  getInverse <- function() {invr}
  list(set=set,get=get, setInverse=setInverse, getInverse=getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix ,If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
  invr <-x$getInverse()
  if(!is.null(invr)){                         ##it checks if its not null  and returns inverse after caching
    message("getting cached data")
    return(invr)
  }
  matrx <- x$get()
  invr <- solve(matrx,...)
  x$setInverse(invr)
  invr
}