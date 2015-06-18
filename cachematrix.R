  ## below functions calculate the inverse of matrix and store it using
  ## cacheSolve function which uses a vector returned from makeCacheMatrix function
  
  
  
  ## makeCacheMatrix function simply:
  ## 1. Sets and gets the value of input matrix via set and get functions
  ## 2. Sets and gets the value of inverse of matrix via setinv and getinv functions
    
  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv )
  }
  ## cacheSolve functions checks whether the matrix input is new or not and if it
  ## is old retrieves the inverse from the memory or else computes the new inverse 
  ## of the input matrix
  
  cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i  
    ##  Return a matrix that is the inverse of 'x'
  }
  
