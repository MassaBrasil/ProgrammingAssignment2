## Put comments here that give an overall description of what your
## functions do
## test masa , TEST 2ND TIME 
## Write a short comment describing this function
## 
## makeCacheMatrix is function to create the cache of a matrix
## 

makeCacheMatrix <- function(x = matrix()) {
  ## flag to indicate if it is inverse or not (NULL)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
  
}


## Write a short comment describing this function
## 
## cacheSolve is function to  cache a matrix
##   x is a matrix()
##   ... dotdotdot

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
  
}
