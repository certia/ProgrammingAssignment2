## 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    i <<- NULL
    
  }
  
  get <- function() x
  
  setmatrix <- function(solve) i <<- solve
  
  getmatrix <- function() i
  
  list(set = set, get = get,
       
       setmatrix = setmatrix,
       
       getmatrix = getmatrix)
  
}

## cacheSolve - Computes inverse and cache if not already cached. Retrieve from cache if already there
cacheSolve <- function(x=matrix(),...) {
  
  i <- x$getmatrix()
  
  if(!is.null(i)) { 
    
    message("getting cached data")
    
    return(i)
    
  }
  
  matrix <- x$get()
  
  i <-solve(matrix, ...)
  
  x$setmatrix(i)
  
  i
  
}
