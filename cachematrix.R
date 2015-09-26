##These two functions are used to create a "matrix" object and then cache its 
##inverse so it can be retrieved later from cache memory if needed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  ##set the value of the matrix
  set<- function(y){
    x<<-y
    m<<- NULL
  }
  ##get the value of the matrix
  get<- function() x
  ##sets the  value of the inverse of the matrix
  setinverse<- function(inverse) m<<- inverse
  ##gets the value of the inverse of inverse
  getinverse<- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  ##checks to see if mean has been calculated
  if(!is.null(m)){
    message("getting cached data")
    #returns inverse from cache 
    return(m)
  }
  data<- x$get()
  ##calculates inverse of the matrix
  m<- solve(data, ...)
  x<- x$setinverse(m)
  ##returns the calculated inverse
  print(m)
        ## Return a matrix that is the inverse of 'x'
}
