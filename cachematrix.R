## Functions for Programming Assignment 2, designed to cache a matrix inverse

## This function creates a special element, a list, that contains a matrix and its inverse

makeCacheMatrix<-function(m=matrix()){
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function(){
    m
  } 
  setinverse <- function(inverse){
    i<<-inverse
  }
  getinverse <- function(){
    i
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function returns the inverse of a matrix from cache if it is cached

cacheSolve<-function(m,...){
  i <- m$getinverse()
  if(!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}