## The below two functions are used to calculate the inverse of matrix 
## and cache the result. Caching the inverse will prevent its costly 
## re-calculation  

## makeCacheMatrix defines the 'get' and 'set' methods for storing
## and retrieving a matrix and its inverse from cache 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve return the inverse of a matrix. It first checks the cache
## for inverse. If inverse is not retrieved from cache then the function 
## calculates the inverse using function solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } 
        message("calculating inverse")
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}
