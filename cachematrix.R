## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## This function creates a matrix object that caches it's inverse
  
  inv <- NULL                                        ## initialize inv value as NULL, will hold mx inverse value
  set <- function(y){                                ## define the set function to assign new
    x <<- y                                          ## value of mx in parent environment
    inv <<- NULL                                     ## resets inv to NULL if there is a new mx
  }

  get <- function() x                                ## get function returns value of mx argument
  setInverse <- function(inverse) inv <<- inverse    ## assigns value of inverse in parent environment
  getInverse <- function() inv                       ## gets the value of inv where called
  list(set=set,get=get,
       setInverse=setInverse,                        ## use a list to refer to functions with $ operator
       getInverse=getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## This function computes the inverse of the matric returned by makeCacheMatrix above
  ## if the matrix has not already been calculated or changed then cacheSolve will retrieve the inverse from the cache
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}










