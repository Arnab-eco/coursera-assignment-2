########################################################################
##  This function takes the input matrix and create a special matrix  ##
##  and cache the inverse of the same                                 ##
########################################################################
makeCacheMatrix <- function(m = matrix(rnorm(25), 5, 5)) {
  cache <- NULL               # set the initial cache
  m.set <- function(y) {
    m <<- y                     # special assign
    cache <<- NULL
  }
  m.get <- function() m
  m.setsolve <- function(solve) cache <<- solve
  m.getsolve <- function() cache
  list(set = m.set, get = m.get,
       setsolve = m.setsolve,
       getsolve = m.getsolve)
}

######################################################################
##  This function returns the inverse of the original input matrix  ##
######################################################################
cacheSolve <- function(m, ...) {
  cache <- m$getsolve()
  if(!is.null(cache)) {
    return(cache)
  }
  data <- m$get()
  cache <- solve(data, ...)
  m$setsolve(cache)
  cache
}

#################################################################
mymat <- matrix(rnorm(36), 6, 6)            #####           #####
m1 <- makeCacheMatrix(mymat)                #       TEST        #
cacheSolve(m1)                              #####           #####
#################################################################

