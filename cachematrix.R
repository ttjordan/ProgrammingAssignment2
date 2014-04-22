## The functions makeCacheMatrix and cacheSolve are used to cache computing of
## matrix inverse. The first time the inverse is requested, builtin function solve()
## is used to get the inverse and the value is stored in a cache. In the following
## requests the value of inverse is retrieved from the cache.

## This function takes as an argument a matrix and returns a list of 
## four functions: get(), set(), setinverse(), getinverse(). 
## Its output is passed as an argument to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the value of the inverse of the matrix has been already 
## computed. If it has, it returns it, otherwise it uses solve() to compute
## the inverse and then stores it for the future requests.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Sample test
# a<- makeCacheMatrix(matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3))
# b <- cacheSolve(a)
