

## Matrix Inversion can be a time taking computation,
## the functions below are created to cache the inversed
## matrix and provide stored/cached inverse matrix
## If the inverse has not been calculated before, the 
## cacheSolve function computes the Inverse, if the 
## Inverse exists the cacheSolve returns the cached 
## inverse with text stating the Inverse is retrieved 
## from cache 

## makeCacheMatrix, takes a matrix as an input (squared matrix)
## and creates getters & setters for accessing the matrix 
## information (Matrix itself or Inverse )

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y = matrix()){
            x <<-- y
            i <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This fuction takes an object of makeCacheMatrix as input
## It checks if the Matrix Inversion has been performed on the 
## object, if the result exists, it returns the cached Inversed 
## Matrix, else it commutes the Inverse & stores it back into the
## cache 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
       i <- x$getinverse()
       
       if(!is.null(i)){
             message("getting cached matrix inverse")
             return(i)
       }
       data <- x$get()
       i <- solve(data)
       x$setinverse(i)
       i
}