## [Put comments here that describe what your functions do]

# create function "makeCacheMatrix"
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# create function "cacheSolve"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


## Test functions

my_matrix <-makeCacheMatrix(matrix(2:5,3,3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$set(matrix(c(41,32,11,74),2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)

