## Put comments here that give an overall description of what your
## functions do
##hg232 assignment

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  # inverse the matrix
  invMatrix <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  # get the matrix
  get <- function() x
  
  # set the inverse
  invMatrixSet <- function(inverse) invMatrix <<- inverse
  # get the inverse
  invMatrixGet <- function() invMatrix
  
  # return the matrix 
  list(set = set, get = get, invMatrixSet = invMatrixSet, invMatrixGet = invMatrixGet)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  invMatrix <- x$invMatrixGet()
  
  # if the inverse is already calculated:: return inverse
  if (!is.null(invMatrix)) {
    message("Getting Cached Inverse Matrix")
    return(invMatrix)
  }
  
  # the inverse isn't yet calculated:: calculate inverse
  data <- x$get()
  invMatrix <- solve(data, ...)
  
  # cache the inverse
  x$invMatrixSet(invMatrix)
  
  # return a matrix that is the inverse of 'x'
  invMatrix
  
}
