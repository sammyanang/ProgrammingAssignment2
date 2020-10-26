## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_of_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_of_matrix <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse) {
    inverse_of_matrix <<- inverse
  }
  getinverse <- function() {
    inverse_of_matrix
  }
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_of_matrix <- x$getinverse()
  if (!is.null(inverse_of_matrix)) {
    message("getting cached data")
    return(inverse_of_matrix)
  }
  data <- x$get()
  inverse_of_matrix <- solve(data, ...)
  x$setinverse(inverse_of_matrix)
  inverse_of_matrix
}
