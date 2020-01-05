## These functions can be used to make calculations 
##with the inverse of a matrix, without repeatidly 
##re-calculating this inverse matrix



##the functions for setting (storing) and getting (retrieving) the inverse of the target matrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }
  



## this function checks wether the solved matrix is 
##already stored in cache. if it is, it retrieves it. 
##if it doesn't, it solves the target matrix and 
##stores the result in cache.

cacheSolve <- function(x, ...) {
       
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
