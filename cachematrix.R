# Programming Assignment 2: Lexical Scoping
makeCacheMatrix <- function( mat = matrix() ) {
  
  ## Initialize I to store the inversion of matrix
  i <- NULL
  set <- function( matrix ) {
    mat <<- matrix
    i <<- NULL # initialize I to null
  }
  ## Get the matrix in the program
  get <- function() {
    mat
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
  mat <- x$getInverse()
  if( !is.null(mat) ) {
    message("getting cached data")
    return(mat)
  }
  ## Compute the inverse via matrix multiplication
  data <-x$get()
  mat <- solve(data, ...)
  x$setInverse(mat) ## Set the inverse to the object
  mat
}

#use the following input to test the code
#mat <- matrix(data = c(3,2,5,6), nrow = 2, ncol = 2)
#mat2 <- makeCacheMatrix(mat)
#cacheMatrix (mat2)

