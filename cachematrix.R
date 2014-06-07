## cachematrix.R contains the functions necessary to perform
## cached computations of the inverse of a matrix

## makeCacheMatrix  Creates a list of set/get functions
## for matrix inverse computations
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## Define the "set" function
  set <- function(y){
          x <<- y
          m <<- NULL
  }
  
  ## Define the "get" function
  get <- function() x
  
  ## Define the "setinv" function
  setinv <- function(solve) m <<- solve
  
  ## Define the "getinv" function
  getinv <- function() m
  
  ## Finally, create the list of set/get functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve calculates the inverse of the matrix
## object created by makeCacheMatrix or returns the
## cached value of a previous computation
cacheSolve <- function(x, ...) {

  ## Attempt to get the cached data
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }

  ## Could not get the cached data. Compute inv(x)
  xinv <- solve(x$get(),...)
  x$setinv(xinv)
  xinv

}
