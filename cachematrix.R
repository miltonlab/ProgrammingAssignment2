## Two comlementary functions to create a cached version of a matrix object
## Please note than cacheSolve receibes a makeCacheMatrix object, 
## instead a matrix object

## Create a cached version to x matrix
## Also one makeCacheMatrix is a wrapper for x matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(n) {
    x <<- n
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calcute ang return the solve cached version of solve 
## funtion to a makeCacheMatrix objet

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}
