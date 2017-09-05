## makeCachematrix: return a list of functions for
##  setting/accessing data of a matrix and its inverse
## cacheSolve: return the inverse of a matrix using cache if available

## create a 'matrix' which is really a list contaning a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## return the inverse of 'matrix' x created with 
##  'makeCacheMatrix', accessing cached inverse if available
##  otherwise computing inverse with 'solve'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message('getting cached inverse')
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
