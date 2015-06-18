## Below are two functions that are used to create a
## special object that stores a numeric matrix and caches its inverse.
## These functions are useful to avoid recompuation of the (costly)
## inverse matrix function

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing functions to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix 
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The 'cacheSolve' function below calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setInverse`
## function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## check the cache first, and return cached matrix if it exists
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## otherwise compute it
  data <- x$get()
  ## call solve for inverse matrix
  im <- solve(data, ...)
  ## save it in the cache
  x$setInverse(im)
  im
}

##
## A simple test function, input is a square invertable matrix
##
test <- function(x, ...) {
  ## make the "special" matrix
  matrix = makeCacheMatrix(x)
  ## first call does the actual computation and saves it
  print(cacheSolve(matrix))
  ## second call uses the cache -- & prints its message
  print(cacheSolve(matrix))
}
