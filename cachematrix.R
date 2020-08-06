## Put comments here that give an overall description of what your
## functions do

## Caching the inverse of a matrix
## Matrix inversion is usually a costly computation
## The benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Two functions below cache the inverse of a matrix
#### 1. Store a matrix
#### 2. Cache its inverse matrix


## Write a short comment describing this function
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#### If the inverse has already been calculated (and the matrix has not changed), 
###### then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve (data, ...)  ## calculate the inverse
      x$setinverse(i)
      i
}

## Test functions
#### Create a matrix
test <- matrix(c(1:4),2,2)

#### Get the inverse matrix
test_inverse <- makeCacheMatrix(test)

#### Return inverse matrix by computing
cacheSolve(test_inverse)

#### Return inverse matrix from cache
cacheSolve(test_inverse)
