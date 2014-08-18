## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# D T Nguyen version created 08/18/2014

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL # set inverse matrix to NULL if it doesn't exist 
    
  # set(): function to push the matrix and its inverse to cache    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  
  # get():  get data value (in this case the matrix) of matrix object x 
    get <- function() x 
  
  # setInv(inv_matrix): set value of the inverse matrix of x to cache
    setInv <- function(inv_matrix) inv <<- inv_matrix
  
  # getInv: get value of the cached inverse matrix
    getInv <- function() inv
    
  # the list of functions to be returned 
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  # 'x' is assumed to be an object created with makeCacheMatrix()
  
  # recall the value of the inverse matrix if it exists
    inv <- x$getInv() 
    
  # if the inverse matrix was already calculated and cached, return the cached value  
    if(!is.null(m)) {
      message("getting cached data")
      return(m) 
    }
  
  # if the inverse matrix hasn't been calculated, get the matrix data
    data <- x$get()
  
  # then calculate the inverse
    inv <- solve(data, ...)
  
  # finally, cache the calculated inverse matrix for future use
    x$setInv(inv)
  
  # and return the calculated inverse matrix inv
    inv
}
