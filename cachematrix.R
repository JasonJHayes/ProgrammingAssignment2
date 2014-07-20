## These functions allow the matrix inverse to be retrieved from cache when the matrix has not changed.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Return an object that holds a matrix and its' inverse
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { inv <<- inverse }
  getinverse <- function() { inv }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## Testing

## mat = matrix(data=c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE)
## mat2 = makeCacheMatrix(mat)
## cacheSolve(mat2)
## cacheSolve(mat2)