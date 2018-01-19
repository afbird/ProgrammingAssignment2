## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list (set = set, get = get, 
              setsolve = setsolve,
              getsolve = getsolve)
}


## Write a short comment describing this function

## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix but  
## checks first to see if the matrix inverse has already been calculated and, if so,
## reads the cached inverse matrix and bypasses the computation. If the inverse 
## matrix does not exist, it is calculated and saved to cache via the setsolve function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
