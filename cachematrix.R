## Matrix inversion is a computation-intensive and there are benefits to caching the inverse rather than computing it again
## The two functions below try to optimize the solving for the matrix inverse by first checking it in the cache
 
## makeCacheMatrix function creates a special matrix object that can cache its inverse
 
makeCacheMatrix <- function(x = matrix()) {
  ## set the initial value of the matrix inverse to NULL
  matrixinverse <- NULL                    
  set <- function(y) {                     
    x <<- y
    matrixinverse <<- NULL             
  }
  ## get the value of the inverse
  get <- function() x                           
  #calculate the inverse of a matrix by the solve function
  setinverse <- function(solve) matrixinverse <<- solve
  # get the inverse    
  getinverse <- function() matrixinverse       
  ## return      
  list(set = set, get = get,                   
       setinverse = setinverse,
       getinverse = getinverse)
}
 
## cacheSolve function computes the inverse of the special matrix returned by the makeCacheMatrix function
## if the inverse has already been calculated (and the matrix has not changed) than the inverse is retrieved from the cache
 
cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()
  #get the inverse if exists
  if(!is.null(matrixinverse)) {                 
    message("getting the cached inverse of the matrix")
    return(matrixinverse)
  }
  #if there is no cached inverse, we need to calculate it
  data <- x$get()                              
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
