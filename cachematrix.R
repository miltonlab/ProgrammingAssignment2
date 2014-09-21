## Two comlementary functions to create a cached version of a matrix object
## Please note than cacheSolve receibes a makeCacheMatrix object, 
## instead a matrix object

## Create a cached version to x matrix
## Also one makeCacheMatrix is a wrapper for x matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Asign the realy matrix and blank the inverse solve
  set <- function(n) {
    x <<- n
    inv <<- NULL
  }
  # Return the realy matrix
  get <- function() x
  # Change the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  # Return the inverse matrix
  getinverse <- function() inv
  # Return a dataframe with all functions in this object
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calcute and return the solve cached version of solve 
## funtion to a makeCacheMatrix objet

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # If already calculated inverse matrix only return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Else calclate inverse matrix with realy matrix and set it in makeCacheMatix object
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  # Return the new inverse matrix calculated
  inv  
}
