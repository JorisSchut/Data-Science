## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #defines caching variable
  m <- NULL
  
  set <- function(y) {
    #caches data in global environment
    x <<- y
    m <<- NULL
  }
  
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setdolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    #checks if cached data is present. If present returns this data.
    message("getting cached data")
    return(m)
  }
  
  #solves the inverse of the matrix and sets the new values
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
