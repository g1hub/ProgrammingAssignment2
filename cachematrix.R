## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  get<-function(){x} # this function returns the value of the original matrix
  
  setinv<-function(solve){inv<<-solve}
  
  getinv <-function(){inv}
  
  list(get=get,
       setinv=setinv,
       getinv=getinv)
  
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix,...)
  x$setinv(inv)
  inv
}

