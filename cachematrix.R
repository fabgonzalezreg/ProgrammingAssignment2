## The objective of these functions are calculate Matrix inversion. You can cached the result so the excution work faster. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## It's assigned Null to variable m
    m <- NULL
  ## It's assigned function 'set' with two variables whose enviroments is different
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  ## Function get() which return the matrix.
  get = function() x
  ## Function setMatInv which fixed inverse matrix
  setMatrInv = function(inverse) m <<- inverse 
  ## Function setMatInv which return inverse matrix
  getMatrInv = function() m
  list(set=set, get=get, setMatrInv=setMatrInv, getMatrInv=getMatrInv)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the inverse matrix   
  m = x$getMatrInv()
  
  # if the matrix inverse exists, the result is informed,  
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # if the result is not cached, then it gets the inverse matrix.
  mat.data = x$get()
  m = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setMatrInv(m)
  
  return(m)
}
