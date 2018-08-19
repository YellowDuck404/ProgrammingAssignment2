## Caching the Inverse of a Matrix
## 
## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.
## 
## Example of usage:
## 
## >m <- matrix(data=c(4,2,7,6), nrow=2, ncol=2)
## >mCache <- makeCacheMatrix(m)
## >cacheSolve(mCache)
##        [,1]    [,2]
## [1,]   0.6    -0.7
## [2,]  -0.2     0.4
## 
## >cacheSolve(mCache)
## getting cached data
## [,1]    [,2]
## [1,]   0.6    -0.7
## [2,]  -0.2     0.4

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse.
## 
## Args:
##   x: matrix
## 
## Returns:
##   Matrix with set,get, setInverse and getInverse functions.

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    if (!identical(oldMatrix,y)) {
      x <<- y
      inverseMatrix <<- NULL 
      message("non identical matrix")
    }
  }
  
  get <- function() x
  
  setInverse <- function(inverseMatrixIn) inverseMatrix <<- inverseMatrixIn
  
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
## Args:
##   x: square invertiable matrix, returned by makeCacheMatrix
##   ...: extra arguments
## 
## Returns:
##   The inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  # Return cached matrix if already computed
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # Compute inverse of the matrix
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  
  # Cache inverse of the matrix
  x$setInverse(inverseMatrix)
  
  # Return inverse of the matrix
  return(inverseMatrix)
}
