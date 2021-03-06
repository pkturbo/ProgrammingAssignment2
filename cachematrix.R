## The function makeCacheMatrix creates a special "matrix"
## object that can store its own inverse.

## The set function sets the Inv to NULL, as the inverse has not yet
## been computed.
## The get function reurns the special matrix
## The setinv function calls the Solve function
## the getinv function calculates the inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) Inv <<- solve
        getinv <- function() Inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), then cachSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinv(Inv)
        Inv
}
