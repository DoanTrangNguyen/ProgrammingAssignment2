## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# D T Nguyen version created 08/18/2014

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL # set inverse matrix to NULL if it doesn't exist 
    
  # set(): store the matrix data and NULL for matrix inverse   
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
    if(!is.null(inv) ){
      message("getting cached data")
      return(inv) 
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

###       Function test case example:
# x <- matrix(data = 1:4, nrow = 2, ncol = 2) 
# solve(x) gives:      [,1] [,2]
              # [1,]   -2    1.5
              # [2,]    1   -0.5
# create "matrix" object with data from x: 
          #  obj_x <- makeCacheMatrix(x) 
          #  obj_x$getInv() returned NULL 
# first call of cacheSolve for obj_x, inverse matrix was calculated:
          #  > cacheSolve(obj_x)
        #     [,1] [,2]
        #[1,]   -2  1.5
        #[2,]    1 -0.5
# second call if cacheSolve for obj_x, value of inverse matrix was gotten from cached data:
          # > cacheSolve(obj_x)
          #  getting cached data
          #     [,1] [,2]
          #[1,]   -2  1.5
          #[2,]    1 -0.5

