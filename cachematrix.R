## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix, create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y){
    x<<- y 
    invs <<- NULL
  }
  get<- function()x
  setInverse <- function (inverse)invs<<-inverse
  getInverse <- function ()invs
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## Write a short comment describing this function
  ##CacheSolve calculate de inverse of the matrix obtained from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()
  if(!is.null(invs)){
    message ("getting cache data")
    return(invs)
  }
  dat<- x$get()
  invs <- solve (dat, ...)
  x$setInverse(invs)
  invs
}
testmatrix<-makeCacheMatrix(matrix(c(4,3,5,6,1,11,8,3,2),nrow=3,ncol=3))
cacheSolve(testmatrix)

