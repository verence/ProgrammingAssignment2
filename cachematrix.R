## Compute the inverse of a matrix is a time-consuming process for large matrix.
## So the folowing 2 functions provid a basic infrastructure to store an 
## and reuse the computed inverse of a matrix.

## create a list object with 4 functions:
## 1.  set ... store the matrix in different environment from current environment,
##             reset the cached result everytime when storing the matrix  
## 2.  get ... load the result
## 3.  setinverse ... store the result in different environment from current environment
## 4.  getinverse ... load the result
##
## Parameter x ... should be a n times n invertable matrix. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function try to load the inverse of a matrix from cache.
## If the inverse is not jet calculated and stored it will be computed and 
## stored.
##
## Parameter x .. have to have at least the 4 functions created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  # compute the inverse of the matrix stored in data
  m <- solve(data)
  x$setinverse(m)
  m
}


# TESTCALL
# create Matrix
m <- matrix(data = c(2,1,5,3), nrow = 2, ncol = 2)
# create CacheMatrix
c <- makeCacheMatrix(m)
# solve inverse of the matrix or get inverse form cache  
cacheSolve(c)

