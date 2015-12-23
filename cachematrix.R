
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize I to store the inversion of matrix
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL # initialize I to null
  }
  ## Get the matrix in the program
  get <- function() {
    m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    i
  }
  ## Back a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# get the inversed matrix from object x
cacheMatrix <- function(x=matrix(), ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Compute the inverse via matrix multiplication
  data <-x$get()
  m <- solve(data, ...)
  x$setInverse(m) ## Set the inverse to the object
  m
}

#use the following input to test the code
#mat <- matrix(data = c(3,2,5,6), nrow = 2, ncol = 2)
#mat2 <- makeCacheMatrix(mat)
#cacheMatrix (mat2)

