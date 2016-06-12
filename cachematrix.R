## Assignment: Programming Assignment 2: Lexical Scoping
## written by Chunhui Chen

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<- function(y){
      x<<-y
      m<<-NULL
  }
  get = function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse<- function() inv
  list(set = set, get = get,setInverse= setInverse,getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

##Test Run
##test_matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
##test_matrix1$get()
##    [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##test_matrix1$getInverse()
##NULL
##cacheSolve(test_matrix1)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

