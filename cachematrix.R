makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solveMatrix) m <<- solveMatrix
    getsolve <- function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# test data
test <- matrix(sample(1:9), 3, 3)
# sent to cache
mc <- makeCacheMatrix(test)
cacheSolve(mc) # data are calculated
cacheSolve(mc) # data are taken from cache
