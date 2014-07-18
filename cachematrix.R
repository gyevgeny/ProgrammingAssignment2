# Reduce repeating matrix solve computation with followng code:
#
#  m <- makeCacheMatrix(matrix( c(4,3,3,2), nrow=2 ))
#  cacheSolve(m)
#
#  m$get() is original matrix. 

# Create an object which hold matrix and inverted cache of it.  
makeCacheMatrix <- function(x = matrix()) {
    cached_solve <- NULL
    set <- function(y) {
      x <<- y
      cached_solve <<- NULL
    }
    get <- function() x
    setsolve <- function(v) cached_solve <<- v
    getsolve <- function() cached_solve
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

#Cache result of solve in the makeCacheMatrix object, return solve version of the matrix.
cacheSolve <- function(x, ...) {
  if (is.null(x$getsolve()))
    x$setsolve( solve(x$get()) )
    
  x$getsolve()
}