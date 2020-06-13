## Two functions are used to store the values and compute the inverse if not
## present

## function makeCacheMatrix is used to create a list of functions which is used
## to set and get the values of data and its inverse

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


## function cacheSolve checks for whether the data has stored an inverse or else
## computes it and then stores it

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
