## This is Coursera R Programming, Programming Assignment 2
## The two functions below, makeCacheMatrix and cacheSolve, are used to cache
## the inverse of a matrix.  Putting the inverse of a matrix in cache will save
## time and machine resources, since the operation will only need to be performed once
## and stored for retrieval, rather than performing it each time it is needed.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}

## Here's how to test it.  First we will create a special matrix without caching it
## > x = rbind(c(-1, 8), c(8, -1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]   -1    8
## [2,]    8   -1
## > 
## Now we will cache the inverse of the matrix created above and cache it
## > cacheSolve(m)
## [,1]       [,2]
## [1,] 0.01587302 0.12698413
## [2,] 0.12698413 0.01587302
## > 
## Now we will get the matrix from the cache instead of re-inverting it.
## > cacheSolve(m)
## [,1]       [,2]
## [1,] 0.01587302 0.12698413
## [2,] 0.12698413 0.01587302


