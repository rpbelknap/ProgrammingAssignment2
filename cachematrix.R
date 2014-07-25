## a pair of functions for caching the inversion of matrices

## Returns a list of functions that set and get values for the supplied matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Variable to hold the inverse
  inv <- NULL
  
  ## Define the setters and getters
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv<<- i
  getInverse <- function() inv
  
  ## Create a list consisting of the functions and return it
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
  
}


## Return a matrix that is the inverse of x.  Checks x to see if the inverse
## has already been calculated. If not, calculates the inverse and stores it 
## with x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## See if the inverse has already been calucated
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    ## Return the cached inverse
    return(inv)
  }
  
  ## Calculate the inverse and attach it to 'x'
  d<-x$get()
  s<-solve(d)
  x$setInverse(s)
  s
  
}
