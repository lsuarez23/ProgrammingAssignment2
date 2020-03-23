## Put comments here that give an overall description of what your
## functions do

## The next lines are functions to cache and compute the inverse of a matrix. 
## makeCacheMatrix is a function that caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
         x <<- y
         m <<- NULL
         }
 
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
  
       list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)      
          
}

## CacheSolve is the function that calculates the inverse of the cached matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
        
}
