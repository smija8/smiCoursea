# Creates an object that can cache data
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # return input data
    get <- function() x
    
    # save calculated data to cache
    setsolve <- function(solveMatrix) m <<- solveMatrix
    
    # return cache data
    getsolve <- function() m
    
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
# Return inverse matrix of "x"
# First call: data are calculated and cached
# Next calls: data are taken from the cache
# Cache represents function makeCacheMatrix
cacheSolve <- function(x, ...) {
  results <- x$getsolve()
  if(!is.null(results)) {
    message("getting cached data")
    return(results)
  }
  results <- solve(x$get(), ...)
  x$setsolve(results)
  results
}

# test data
test <- matrix(sample(1:9), 3, 3)
# sent to cache
mc <- makeCacheMatrix(test)

cacheSolve(mc) 
# result:
# [1,]  0.07142857 -0.4714286  0.4857143
# ...

cacheSolve(mc)
# result:
# getting cached data
# [1,]  0.07142857 -0.4714286  0.4857143
# ...