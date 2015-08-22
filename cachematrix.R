## Matrix Inverse Calculation Function

## makeCacheMatrix creates a list of four functions on the input to the function
## The first sets the matrix, the second gets the matrix, the third sets the calculated inverse, and the fourth gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If no inverse exists, this function calculates the inverse of the matrix using solve, and then uses the setinverse function
## from above to set the inverse of the matrix. It then prints out the inverse.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting Inverse")
    return(m)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
