## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# mat <- matrix(, nrow = 15, ncol = n.columns)
# for(column in 1:n.columns){
#   mat[, column] <- vector
#v}

makeCacheMatrix <- function(x = matrix()) {
#  x <- matrix(x,nrow=2,ncol=2)
  i <- NULL
  set <- function(y) {
    x <<- matrix(y,nrow=2,ncol=2)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solv) i <<- solve
  getinverse <- function() i
  matrix(c(set = set, get = get, setinverse = setinverse, getinverse = getinverse),
         nrow=2,ncol=2)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
