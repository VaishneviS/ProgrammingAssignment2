## makeCacheMatrix has 4 functions:
## set<-sets any new matrix to x in the parent environment(i.e. parent function)
##     nullifies inverse matrix if new matrix is assigned
## get<-gets the matrix assigned to x
##setmatrix<-gets the inverse matrix and caches it
##getmatrix<-retrieves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix= getmatrix)
  }
  
}


## cacheSolve calculates the new inverse matrix 
#if cache is null for a given x

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
