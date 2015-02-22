## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## make cachematrix creates matrixs in cache
##1. set the value of the matrix
## 2.gets the value of the matric
##3.set the  value of the inverse
##4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse= getinverse)
}


## Write a short comment describing this function

## cacheSolve computes inverse of the matrix created with the makecachematrix function
## if looks at whther there is this data in the cache, if present it gets inverse from 
##cache , if not then it calculates the inverse matrix and stores the value in the cache 
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

