## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Similar to the reference example makeVector()
## this function create a list with functions applied to the
## matrix passed as parameter (mtx) to later, via the cacheSolve 
## function calculate the inverse matrix of mtx.
## The only real change was to explicitly set the input parameter 
## as a Matrix class. 

makeCacheMatrix <- function(mtx = matrix()) {
  inv_m <- NULL
  setmtx <- function(y) {
    mtx <<- y
    inv_m <<- NULL
  }
  getmtx <- function() mtx
  setinvmtx <- function(get_inv) inv_m <<- get_inv
  getinvmtx <- function() inv_m
  list(setmtx = setmtx, getmtx = getmtx,
       setinvmtx = setinvmtx,
       getinvmtx = getinvmtx)
}


## Write a short comment describing this function
## Similar to the reference example cachemean()
## this functions calculates the inverse matrix of the mtx
## matrix only once, and it caches the result, so if the function is 
## called again and the origin matrix hasn't changed the result
## is retrieved from the cached variable.
## The input of the function is the list that was the result of
## the call to makeCacheMatrix().

cacheSolve <- function(x = list(), ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvmtx()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmtx()
  inv_m <- solve(data, ...)
  x$setinvmtx(inv_m)
  inv_m
}
