## This file contains 2 functions that create a matrix, cache its inverse and 
## retrieve the cached inverse, if available, instead of computing it

## Create matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getmean <- function() i
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Retrieve cached inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
