##
## Function : makeCacheMatrix (m)
##
## Purpose : Function containing logic to save and retrieve the inverse of a matrix from cache.
##
## Syntax : getsol(x) : return a copy of the inverse of x from cache or NULL if none exists.
##        : setsol(x) : save a copy of the inverse of x in cache.
##
## Returns: getsol() : return a copy from cache if it exists or NULL if not.
##          setsol() : save a copy of the solution in cache and return the result.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get, 
       setsol = setsol, 
       getsol=getsol)
}


## 
##  Function : cacheSolve (m)
##
##  Purpose : Function to return the inverse of matrix m from and store it in cache if 
##            it does not already exist in cache.
##
##  Syntax  : cacheSolve(m) where m is the matrix of which the inverse is required.
##
##  Returns : The inverse of the matrix m retrieved from cache. If a copy does not exists in
##            cache, a copy is stored in cache and the result returned.
##
cacheSolve <- function(x, ...) {
  m <- x$getsol()
  if (!is.null(m)){
    return (m)
    m <- solve(m)
    x$setsol(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
}
