## Here are a couple of functions that will:
## 1) create a matrix that can cache its inverse, and
## 2) return the inverse of that matrix.

## For these functions, we assume that the matrix supplied is always invertible.


## makeCacheMatrix creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  
  m <- NULL   
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
      ## set initial value of matrix
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the inversion for the matrix created by the
## makeCacheMatrix function above. If the inverse matrix has already
## been calculated, this function simply retreives the result.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
## If the inverse matrix 'm' already exists, notify the user and retrive it from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

## If the inverse matrix 'm' hasn't been calculated, get matrix 'x' and call it 'data'
  data <- x$get()

## Calculate the inverse of 'data' with solve() and call it 'm'
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## If m is NULL, 
}
