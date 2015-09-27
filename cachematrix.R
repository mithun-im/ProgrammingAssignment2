
makeCacheMatrix <- function(x = matrix()) {      # makeCacheMatrix function is used to create a special "matrix"
  i <- NULL
  set <- function(y) {                           # set function is used to set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                            # get function is used to get the value of the matrix
  setinverse <- function(inverse) i <<- inverse  # setinverse function is used to set the value of the inverse
  getinverse <- function() i                     # getinverse function is used to get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {    # cacheSolve function is used to return a matrix that is the inverse of 
                                    # invertible matrix 'x'
  i <- x$getinverse()               # 
  if(!is.null(i)) {                 # checking whether the inverse of matrix exists or not
    message("getting cached data")  
    return(i)                       # if it exists, the inverse is taken from cache and no further computation 
                                    # is carried out.
  }
  data <- x$get()                   # if the inverse of the matrix is not in the cache, get function
                                    # is used to get the value of the matrix
  
  i <- solve(data, ...)             # finding the inverse of the matrix using solve function
  x$setinverse(i)                   
  i
}
