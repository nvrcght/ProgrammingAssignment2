## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## his function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ##create a matrix
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      ## create inverse of a matrix
      setinverse <- function(solve) m <<- solve
      ## getter - to access inverse of a matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      ## if inverse was already calculated - get data from cache
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
