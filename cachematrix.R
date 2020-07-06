## The following function is used to create an object to 
## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ##First we will initialize the inverse setting
  j <- NULL
  ## SEtting the Matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  #Getting the Matrix using the following function: (returning matrix x)
  get <- function()x
  #Setting the inverse
  setInverse <- function(inverse) j <<- inverse
  #Getting the inverse of Matrix using the following function: 
  #(returning inverse of matrix)
  getInverse <- function() j 
  ##REturning a list of methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The following function will computer the inverse of the matrix. If it already
## Exists then it will be retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("cache data present")
    return(j)
  }
  #Getting matrix from object
  mat <- x$get()
  j <- solve(mat,...)
  #Setting the inverse
  x$setInverse(j)
  # REturning the matrix
  j
}
