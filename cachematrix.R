## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function allows the matrix to set and get value of matrix
## then set and get the the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  
  
  
## Write a short comment describing this function

## This function checks if the inverse is already calculated and skips it if it has.
## If not, it calculates and stores the value in the cache through the setinverse function.
## The function below assumes the matrix can be inverted.
cacheSolve <- function(x, ...) { 
    inver <- x$getinverse()
    if(!is.null(inver)) {
      message("getting cached data.")
      return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver
  }
