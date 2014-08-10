## makeCacheMatrix and cacheSolve provide an inexpensive interface to store 
## matrices and their inverse. The computationally expensive solve()-function is
## only called if it has not been called on the matrix previously, i.e. the 
## inverse matrix is cached.



## makeCacheMatrix stores a matrix together with its inverse
## It posesses functions set() and setinverse() to update the matrix and the inverse, respectively,
## and functions get() and getinverse() to retrieve the matrix and the inverse, respectively.
## When the matrix is updated with set(), the inverse matrix is deleted to avoid 
## retrieving a previously cached matrix.
makeCacheMatrix <- function(x = matrix()) {
  theInverseMatrix <- NULL # initially, the inverse matrix is not defined
  
  set <- function(y) {
    x <<- y
    # upon update of the matrix, the possibly cached inverse needs to be deleted
    theInverseMatrix <<- NULL   
  }
  get <- function() x
  
  setinverse <- function(xm1 = matrix()) theInverseMatrix <<- xm1
  getinverse <- function() theInverseMatrix
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
      )
}


## Return a matrix that is the inverse of 'x'
## If possible, it uses the cached inverse matrix to avoid calling
## solve()
cacheSolve <- function(x, ...) {
   inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  ##No cached data, need to actually compute inverse
  theMatrix <- x$get()
  ## Get inverse matrix, call the actual numerical routine to find the inverse
  inverseMatrix <- solve(theMatrix)
  ## Update the stored inverse matrix
  x$setinverse(inverseMatrix)
  inverseMatrix
  
}


## Test functionality on a simple example
## Create matrix object
m1<-makeCacheMatrix( cbind( c(0,1,0), c(0,0,1),c(1,0,0)))
## The matrix is stored in m1
m1$get()
## The inverse does not yet exist
m1$getinverse()
## Compute the inverse
cacheSolve(m1)
## Now the inverse is stored
m1$getinverse()
## Update matrix
m1$set(cbind(c(0,2,0),c(0,0,2),c(2,0,0)))
## The inverse was deleted
m1$getinverse()
