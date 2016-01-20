## makeCacheMatrix defines a list of functions that cache the matrix input and it's 
## inverse. cacheSolve takes the list of functions to return a cached version of 
## the matrix inverse, if available

## makeCacheMatrix defines a list of functions that cache the matrix input and it's 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve takes the list of functions to return a cached version of 
## the matrix inverse, if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i<-0
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
