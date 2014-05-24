## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## i is the inv of x
        i <- NULL
        set <- function(y) {
## we have a new matrix x we still dont know i we know that x equal to y
                x <<- y
                i <<- NULL
        }
## we get x
        get <- function() x
## we set the inv
        setinverse <- function(solve) i <<- solve
## we get the inv
        getinverse  <- function() i
## all we want to have is a list
        list(set = set, get = get, setinv = setinverse , getinv = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first we want to know if we have i
        i<-x$getinverse()
        ## if we have i we return i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if we dont have i we find the inverse of the matrix and we set the inverse of x
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}