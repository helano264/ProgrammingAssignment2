## Pair of functions that calculate the inverse of a matrix or just cache it if
## it has been previously computed

## Create a "vector" that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## Calculate the inverse of a matrix, or just cache it if it's already been 
## calculated

cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set.inv(inv)
  inv      ## Return a matrix that is the inverse of 'x'
}
