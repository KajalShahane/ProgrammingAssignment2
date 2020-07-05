CacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInv <- function(inverse) j <<- inverse
  getInv <- function() j 
  list(set = set, get = get, 
  setInv = setInv, 
  getInv = getInv)
}

cacheSolve <- function(x, ...) {
  j <- x$getInv()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInv(j)
  j
}