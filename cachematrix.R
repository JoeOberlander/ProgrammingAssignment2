## This is the second programming assignment for the
## Coursera course entitled "R Programming"

## The assignment was to write two functions, 
## makeCacheMatrix and cacheSolve

## The makeCacheMatrix functionis designed to create a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invermat <- NULL
  set <- function(y) {
    x <<- y
    invermat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invermat <<- solve
  getinverse <- function() invermat
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function is designed to compute 
## the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  invermat <- x$getinverse()
  if(!is.null(invermat)) {
    return(invermat)
  }
  data <- x$get()
  invermat <- solve(data, ...)
  x$setinverse(invermat)
  invermat
}makeCacheMatrix