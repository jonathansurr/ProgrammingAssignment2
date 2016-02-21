makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
           xinv <<- NULL
         }
      get <- function() x
       setinv <- function(solve) xinv <<- solve
       getinv <- function() xinv
       list(set = set, get = get,
                       setinv = setinv,
                       getinv = getinv)
     }  

  
   ## x$get() will show the matrix, x$getinv will produce NULL unless used cacheSolve below, x$set(...) creates new matrix, x&setinv makes new inverse
   
   cacheSolve <- function(x, ...) {
       xinv <- x$getinv()
       if(!is.null(xinv)) {
           message("getting cached data")
           xinv
         }
      data <- x$get()
       xinv <- solve(data, ...)
       x$setinv(xinv)
       xinv
     }
      ## checks to see if xinv is not NULL and if it isnt prints 'getting cached data' and shows xinv, otherwise calculated inverse and sets it to xinv
     
