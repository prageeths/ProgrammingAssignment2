#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  c_inverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) c_inverse <<- inverse
  getInverse <- function() c_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  invFunc <- x$getinverse()
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(invFunc)) {
    message("retrieving cached data.")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data)
  x$setinverse(invFunc)
  invFunc
}