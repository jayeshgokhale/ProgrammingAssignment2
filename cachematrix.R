## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function builds a list which holds the functions to set or get (contd)
#the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function calls the getinverse function of the list and if (contd)
# the inverse is already cached then prints it else it computes (contd)
# real time and stores it in the same list
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

#Testing with a sample matrix
A <- matrix( c(5, 1, 0,3,-1, 2,4, 0,-1),3,3)
B <- makeCacheMatrix(A)
cacheSolve(B)
cacheSolve(B)
A %*% cacheSolve(B)

