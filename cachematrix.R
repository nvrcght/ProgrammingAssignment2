
## This function creates a special "matrix" object that can cache its inverse
##create a matrix
makeCacheMatrix <- function(x = matrix()) { 
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
      ## if inverse was already calculated - get data from cache and return it 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ##if the inverse hasn't been calculated - get data and solve the inverse then return the inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
