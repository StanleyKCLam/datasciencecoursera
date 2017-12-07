## This a almost a copy of makeVector
## This is a function to return a list of behaviour functions

makeCacheMatrix <- function(x = matrix()) {
  # First, create an object "inv" in the parent environment
  #     to store the inverse of "x", but initially set to NULL
  inv <- NULL
  
  # Now define the 4 getter & setter functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(z) inv <<- z
  getinv <- function() inv
  
  #Now return the list which contains the above 4 functions
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function is to return the inverse matrix

cacheSolve <- function(x, ...) {
  # First, check if the parent environment variable "inv" is NULL or not
  # If not NULL, return "inv" as the cached Inverse
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # If "inv" is NULL, calculate the Inverse using solve() function & store in "inv"
  # and so "inv" is not NULL in the next read of "inv"
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
