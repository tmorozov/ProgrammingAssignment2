## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## Those are pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## It returns the list with function for 
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse (setinverse)
## 4. get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  ## s - holds inverse of matrix
  s <- NULL
  ## set function stores matrix value in x
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## get function returns matrix value
  get <- function() x
  ## setinverse - stores inverse value
  setinverse <- function(inverse) s <<- inverse
  ## getinverse - returns cahced inverse value
  getinverse <- function() s
  
  ## return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the inverse from the cache is returned.
cacheSolve <- function(x, ...) {
  ## read cached inverse value
  s <- x$getinverse()
  ## check if it's not empty
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## if no chaced inverse value preset - calculate it
  data <- x$get()
  ## solve function is used for calculation of inverse
  s <- solve(data, ...)
  ## save calculated inverse value
  x$setinverse(s)
  ## return inverse value
  s
}
