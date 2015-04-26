## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
##  generates and returns a list with 4 functions:
##          set - m is changed globally to input matrix, sets invm to NULL and returns it
##          get - returns matrix m
##          setinvm - modifies invm globaly, sets invm to invMatrix 
##          getinvm - returns inverse matrix saved in invm

makeCacheMatrix <- function(m = matrix()) {
 
      invm <- NULL
      
      set <- function(y) {
            m <<- y
            invm <<- NULL
      }
      
      get <- function() m
      
      setinvm <- function(invMatrix) {
            invm <<- invMatrix
      }
      
      getinvm <- function() invm
      
      list(set = set, get = get,
           setinvm = setinvm,
           getinvm = getinvm)    

}


## cacheSolve:
## needs a list with 4 elements as input
## 4 elements need to be functions named get,set,getinvm,setinvm
## returns inverse of matrix - either reads it out of 
##     cache it it is was calculated already or calculates inverse matrix

cacheSolve <- function(funlist, ...) {
        ## Return a matrix that is the inverse of "matrix"
      invm <- funlist$getinvm()
      if(!is.null(invm)) {
            message("getting cached data")
            return(invm)
      }
      matrix <- funlist$get()
      invm <- solve(matrix)
      funlist$setinvm(invm)
      invm
}
