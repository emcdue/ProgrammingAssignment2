# The two functions cache the inverse of a matrix instead of
# calculating it repeatedly


# The makeCacheMatrix function create a special matrix, based on
# the 'x' input matrix, that can chace its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# The cacheSolve function return the inverse of the matrix
# 'x' that is the result of the makeCacheMatrix function. 
# It also checks if the inverse has been already calculated:
# in this case it prints the message "getting cached data" 
# and it returns the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
