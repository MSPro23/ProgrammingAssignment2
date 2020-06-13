## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setmatrix<-function(inverse) i<<-inverse
  getmatrix<- function() i
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getmatrix()
  if(is.null(i)){
    message("getting the inverse")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setmatrix(i)
  i
}
