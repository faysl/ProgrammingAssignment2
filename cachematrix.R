
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Following functions are used to create special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse as quoted in the example.

makeCacheMatrix <- function(alfa = matrix()) {
  inv <- NULL
  set <- function(bravo) {
    alfa <<- bravo
    inv <<- NULL
  }
  get <- function() alfa
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,  get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolution <- function(alfa, ...) {
  ## Return a matrix that is the inverse of 'alfa'
  inv <- alfa$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- alfa$get()
  inv <- solve(mat, ...)
  alfa$setInverse(inv)
  inv
}
