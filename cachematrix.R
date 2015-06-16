## Functions makeCacheMatrix and cacheSolve store the results
## of matrix-inversion calculations, so that these calculations
## only need to be performed once.  After the inverse is calculated
## once, it is stored in a function list so that it is retrieved
## (rather than calculated) as long as the original matrix
## remains unchanged.

## makeCacheMatrix stores a matrix as a list of 4 functions:
## Function set() to initialize the matrix by storing
##      the full matrix within the list
## Function setinverse() to calculate the inverse and store it
## Function get() to return the value of the matrix stored
##      within the list
## Function getinverse() to return an inverse that has been 
##      calculated previously
## Note: if the value of the matrix is changed (using set()),
##       the stored value of the inverse is erased and will not
##       be re-calculated until the inverse requested.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #Initialize the inverse as NULL (uncalculated)
  ## set() assigns a new value to the matrix and resets
  ## the value of the inverse to NULL (indicating uncalculated)
  set <- function(z) {
    x <<- z      
    inv <<- NULL # NULL indicates inverse is uncalculated
  }
  ## get() simply returns the matrix saved within the list
  get <- function() {
    x
  }
  ## setinverse assigns the value of the argument (a new inverse
  ## calculated within cacheSolve() ) to inv
  setinverse <- function(newinverse) {
    inv <<- newinverse
  }
  ##  getinverse returns the inverse, which is NULL if not yet
  ##  calculated, or is the actual inverse if it has already
  ##  been assigned by the setinverse() function
  getinverse <- function() {
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## cacheSolve solves for the inverse of a matrix while taking 
## advantage of the stored values made possible by makeCacheMatrix.
## If the inverse has been previously calculated, it uses 
## getinverse() to retrieve it; but if it has not yet been 
## calculated, it caches the inverse using setinverse().
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #Try to retrieve cached inverse
  if(!is.null(inv)) {
    message("cached value found")
    return(inv)  # Return the cached value
  }
  ## If no cached value exists, calculate the inverse and cache the result:
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}