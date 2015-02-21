# Author : bganapathy

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <-  function(x = matrix())
{
  
  invmat <- NULL
  set <- function(y)
  {
    x  <<- y
    invmat  <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) 
    invmat <<-inverse
  getinverse <- function() invmat
  list(set= set,get = get,setinverse=setinverse, 
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieves the inverse from the cache.

cacheSolve <- function(x,...)
{
  m <- x$getinverse()
  print(m)
  if (!is.null(m))
  {
    message("getting cached inversed matrix")
    return(m)
  } 
  else 
  {
    m <-solve(x$get())
    x$setinverse(m)
    return(m)
  }
}
