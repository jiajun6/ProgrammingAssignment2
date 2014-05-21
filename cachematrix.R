## Creat a  object stores a matrix and cache its inverse
## This script contains two functions "makeCacheMatrix" and "cacheSolve"
## "makeCacheMatrix" creats a matrix which cache its inverse
## "cacheSolve" computes the invesreof matrix created by "makeCacheMatrix"

## FUnciton "makeCacheMatrix" creats a matrix which can cache its inverse, which is a list contaning
## a funciton to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse of the matrix
## 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix<-NULL
  set<-function(y){
    x<<-y
    inverseMatrix<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inverseMatrix<<-inverse
  getinverse<-function() inverseMatrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function "cachesolve" computes the inverse of the special "matrix" returned
## by function "makeCacheMatrix" above. If the inverse has already been calculated
## (and the matrix has not changed), then "cachesolve" should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix<-x$getinverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    retrun(inverseMatrix)
  }
  data<-x$get()
  inverseMatrix<-solve(data,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
