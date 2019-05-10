## First creates a cache matrix setting the matrix, getting the matrix, then both setting and getting the inverse.
## following same general flow of the means examples.


makeCacheMatrix <- function(x = matrix()) {
  I <- NULL  	
  set <- function(y) { 		
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I 
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##first check to see if inverse has already been calculated, therefore if the matrix hasn't changed it will get the inverse from the cache.
## If not already solved gets data from the cache list "get".
## solve command solves the matrix, and then sets the inverse of I and displays it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if (!is.null(I)) {
    message("grabbing cache inverse")
    return(I)
  }
  data <- x$get()
  I <-solve(data, ...)
  x$setinverse(I)
  I
}
