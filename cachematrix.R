## The functions below create a special vector that stores a matrix along with 
## its cached inverse if the inverse has been previously solved by cacheSolve
## below

## makeCacheMatrix creates a special vector that stores a matrix and its 
## cached inverse if computed.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve checks to see if the inverse of a matrix is stored in the
## vector created by makeCacheMatrix, if so it returns the stored value
## if not it computes the inverse and stores the value in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
