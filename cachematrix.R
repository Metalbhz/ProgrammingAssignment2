## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  
  set <- function (y) {
    x <<- y
    inv <- NULL
  }
  
  get <- function () x;
  
  setInv <- function (inversa) inv <<- inversa
  
  getInv <- function() inv
  
  list (set = set, get = get, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 
  m <- x$getInv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  mI <- solve (data)
  
  x$setInv (mI)
  
  mI
}
