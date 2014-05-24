## makeCacheMatrix takes a matrix and makes a list with four elements.
## cacheSolve takes this list as argument and returns the inverse matrix
## if the inverse has allready been calculated once
##cacheSolve returns the cached inverse


## makeCacheMatrix takes a matrix and makes a list with four elements.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
    
  }
  get<-function() x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function() m
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  m<-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
