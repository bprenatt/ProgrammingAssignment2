## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  inverseMatrix <- NULL
  get <- function(){
    m
  }
  set <- function( i ) {
    m <<- i
    inverseMatrix <<- NULL
  }
  getInverse <- function(){
    inverseMatrix
  }
  setInverse <- function( inverse ){
    inverseMatrix <<- inverse
  }
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function( m, ... ) {
  inverseMatrix <- m$getInverse()
  if ( is.null( inverseMatrix ) ) {
    inverseMatrix <- solve( m$get() )
    m$setInverse( inverseMatrix )
  }
  inverseMatrix
}