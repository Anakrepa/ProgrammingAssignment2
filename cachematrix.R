## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  
        inv <- x$getInv()
  if(!is.null(inv)) {                ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
    message("getting cached data")   ## should retrieve the inverse from the cache.
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)   ## solve function gives the inverse of the data.
  x$setInv(inv)
  inv                          ## Return a matrix that is the inverse of 'x'
        
}
