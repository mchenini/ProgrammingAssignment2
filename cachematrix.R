## Ths function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("get the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv

}

## > Example Run
## > M <- matrix(c(2,-3, -1, 1),2,2)
## > M
##       [,1] [,2]
##  [1,]    2   -1
##  [2,]   -3    1
## >
## > M1 <- makeCacheMatrix(M)
## >
## > cacheSolve(M1)
##      [,1] [,2]
## [1,]   -1   -1
## [2,]   -3   -2
## > M1$get()
##      [,1] [,2]
## [1,]    2   -1
## [2,]   -3    1
## >
## > ## No cache in the first run
## >
## > cacheSolve(M1)
## get the cached data
##      [,1] [,2]
## [1,]   -1   -1
## [2,]   -3   -2
## > ## A second run will retrieve from the cache
## > cacheSolve(M1)
## get the cached data
##      [,1] [,2]
## [1,]   -1   -1
## [2,]   -3   -2
