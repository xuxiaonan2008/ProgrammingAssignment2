## The following two functions creates a speicial matrix and then calculates the inverse of the matrix. The second function first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


## The function "makeCacheMatrix" creates a special matrix that can cache its inverse.
## It is a list containing a function to
## set(): set the value of the matrix
## get(): get the value of the matrix
## setinverse() set the value of the inverse
## getinverse(): get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" should retrieve the inverse from the "makeCacheMatrix".

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
