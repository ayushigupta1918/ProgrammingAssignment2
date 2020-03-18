## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  initialMatrix <- NULL  #to store the previous matrix
  set <- function(y){
    initialMatrix <<- x  #update the previous matrix to x before updating x
    x <<- y
    r <<- NULL
  }
  
  get <- function() x
  setReverse <- function(reverse) {
    r <<- reverse
  }
  getInitialMatrix <- function() initialMatrix 
  setInitialMatrix <- function(im) initialMatrix <<- im
  
  getReverse <- function() r
  list(set = set, get = get, setReverse = setReverse, getReverse = getReverse, getInitialMatrix = getInitialMatrix, setInitialMatrix = setInitialMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  r <- x$getReverse()
  previous <- x$getInitialMatrix() #getting previous matrix 
  current <- x$get() #getting current matrix
  
  #if r is set and previous matrix and current matrix are same then use cached data
  if(!is.null(r) && is.matrix(previous) && is.matrix(current) && (dim(previous) == dim(current)) && all(previous == current)) {
    #set previous to current
    message("getting cached data")
    return(r)
  }
  
  #if different, then update previous to current
  x$setInitialMatrix(current)
  r <- solve(current)
  
  #update reverse
  x$setReverse(r)
  r

}
