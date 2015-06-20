makeCacheMatrix <- function(x = matrix(), z = matrix()) {
  i <- NULL
  set1<- function(y) {
    x <<- y
    i <<- NULL
  }
  set2<- function(t) {
    z <<- t
    i <<- NULL
  }
  get1<- function() x
  get2 <- function() z
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  matrix( set1= set1,set2 = set2,
          get1 = get1, get2 = get2,
          setmean = setmean,
          getmean = getmean)
}
cacheSolve <- function(x, ...)  {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data1<- x$get1()
  data2<- z$get2()
  i <- solve(x, z, tol, LINPACK = FALSE, data1, data2)
  x$setinverse(i)
  i
}