## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value of the vector (m<-NULL)
## get the value of the vector (definition of "get")
## set the value of the inverse (function "solve")
## get the value of the inverse (definition of "getinv")

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## The function "cacheSolve()" first checks if the inverse matrix has
## already been calculated. If it hasn't, it calculates with "solve()".

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

