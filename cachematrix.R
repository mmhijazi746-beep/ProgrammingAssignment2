## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creating a matrix to cashe its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL                            ## initialize the inverse property
  
  set <- function( matrix ) {          ## to set the matrix
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() m                  ## to get the matrix
    
  setInverse <- function(inverse) {     ## to set the inverse matrix
    i <<- inverse
  }
  
  getInverse <- function() i            ## to get the inverse matrix

  list(set = set, get = get,            ## return a list of the methods
       setInverse = setInverse,
       getInverse = getInverse)
}



## function to compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
     m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()               ## get the matrix from data
  
  m <- solve(data) %*% data     ## calculate the inverse
  
  x$setInverse(m)               ## setting the inverse to the object
  
  m                             ## Return the matrix
  
}
