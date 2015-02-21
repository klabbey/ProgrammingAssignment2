## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is used to set and get a Matrix
## cacheSolve is used to return the inverse of the matrix. If calculated before the inverse is returned from the cache

## Write a short comment describing this function
##makeCacheMatrix is used to make a new matrix. Please create a new matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- ymake
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrixInv) m <<- matrixInv
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
## The function cacheSolve returns a cached version of the inverse of the matrix if already calculated.
## For a new matrix, the inverse is calulaed using the solve() function
##Please pass the matrix that you had originally created using the makeCacheMatrix funtion to cacheSolve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
}
