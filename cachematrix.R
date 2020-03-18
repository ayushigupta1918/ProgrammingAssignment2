## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  initialMatrix <- NULL
  set <- function(y){
    initialMatrix <<- x
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
  message("printing r")
  print(r)
  previous <- x$getInitialMatrix()
  message("printing previous")
  print(previous)
  current <- x$get()
  message("printing current")
  print(current)
  if(!is.null(r) && is.matrix(previous) && is.matrix(current) && (dim(previous) == dim(current)) && all(previous == current)) {
    #set previous to current
    x$setInitialMatrix(current)
    message("getting cached data")
    return(r)
  }
  x$setInitialMatrix(current)
  r <- solve(current)
  x$setReverse(r)
  r

}
