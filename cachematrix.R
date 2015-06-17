## Create a special matrix with an inverse that can be cached, then
## compute this inverse. If the inverse has already been computed
## it will be pulled from a cache.

## Coverts a matrix 'x' to a special list that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
## Return a list that can have its inverse cached
{
  s <- NULL
  set <- function(y)
  {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of 'x'. If the inverse of 'x' has already
## computed, the result will be pulled from the cache.

cacheSolve <- function(x, ...) 
## Return a matrix that is the inverse of 'x'
{
  s <- x$getInverse()
  if(!is.null(s))
  {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}
