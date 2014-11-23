## makeCacheMatrix : 
## This function creates a special "matrix" object that can cache its inverse.
##

##  makeCacheMatrix is a function that receives a variable of class matrix()
## test masa , TEST 2ND TIME 

## 
## makeCacheMatrix is function to create the cache of a matrix
## 

makeCacheMatrix <- function(x = matrix() ) {

  ## inv is the inverse matrix, initially is set the cache inv to NULL
  
  inv <- NULL
  ## set receives function variable (a matrix) and caches matrix into  mt
  ## also caches NULL into inverse matrix inv
  set <- function( matrix ) {
    mt <<- matrix
    inv <<- NULL
  }
  
  ## get returns cached matrix mt
  get <- function() {
    
    mt
  }
  
  ## set in the cache inv (invese matrix) the inverse matrix received
  setInverso <- function(inverso ) {
    inv <<- inverso 
  }
  
  ## get returns the cached inverse matrix inv
  getInverso <- function() {
    inv
  }
  
  ## for listing the methods of function makeCacheMatrix...
  
  list(set = set, get = get,
      setInverso = setInverso,
      getInverso = getInverso)
}

## cacheSolve : 
## This function receives parameter of matrix, process solve function of R and returns the inverse of input matrix cached .
## solve() is executed only in case of inverse still does not exist.
## this is verified checking if there is its correspondent inverse in cache for the  matrix from parameter
## in case its inverse already exists), then this function  cacheSolve should process the solve() function, set the cache  and return the inverse that cache


cacheSolve <- function(mtx = matrix(), ...) {

  ## retrieves matrix inverse cached
  invmtx <- matrix()
  invmtx <- mtx$getInverso()
  
  ## in case inverse still does not exist, mtx is expected to be NULL
  ## in case it is null, this means that inverse already exists cached - thus in this case just need to return the invmtx
  if(!is.null(invmtx)) {
    message("inverse matrix already exists ")
    return(invmtx)
  }
  
  ## if executino of function reached this point
  ## inverse does not exist and still needs to be created and cached
  ## first step is to retrieve the original matrix from cache
  orimtx <- mtx$get()
  
  ## the syntax solve(orimtx) is executed is as below (multiply matrix )
  invmtx <- solve(orimtx) %*% orimtx
  
  ## invmtx <- solve(orimtx) %*% orimtx this makes return the identity matrix (replaced by above)
  
  ## caching the invmtx calculated 
  mtx$setInverso(invmtx)
  
  ## return the inverse matrix (invmtx)
  invmtx
}

## steps for testing :
## 1) creating (instancing) object of function makeCacheMatrix()
## >testing <- makeCacheMatrix()
##
## 2) using function set of instance testing to set matrix : 
## >testing$set(matrix(1:4,2,2))
## 
## 3) using function set of instance testing to set matrix : 
## >cacheSolve(testing)
## 
## it returns output as below:
##
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 
