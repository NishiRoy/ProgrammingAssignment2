## Solution to the programming Assignment:"Caching the inverse of a matrix"

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is an inversible matrix
  m<-NULL
  set<- function(y){
    x<<-y 
    ## <<- is used to assign a value to an object in an environment different from the current environment.
    m<<-NULL
  }
  get<- function() x
  ## Setting the inverse of the matrix
  setInverse<- function(solve) m<<-solve
  ## Getting the inverse of the matrix
  getInverse<- function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function creates the inverse of the Matrix created by makeCacheMatrix(). If the 
## inverse is already cached then it just returns the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  
  ## check if the inverse has already been calculated
  if(!is.null(m))
  {
    message("Cached Data")
    return(m)
  }
  
  ## else calculate the inverse
  data<-x$get()
  m<-solve(data,...)
  
  ## set the value of inverse in cache
  x$setInverse(m)
  
  return(m)
}
