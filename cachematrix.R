## The following fuctions are used to optimize the matrix inverse calculation
## by using a cache variable to store the calculation result.

## makeCacheMatrix: builds a list structure to store the input matrix and its 
## inverse. Functions get/set are defined to access the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve: computes the inverse of the given matrix. The input x is the list 
## obtained by calling makeCacheMatrix (see above) on the matrix.
## The computation is performed once. Unless the input matrix doesn't change,
## next calls would access only to the cached result.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # check the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # computes the inverse
        x$setinverse(m)       # stores the result in the cache
        m
}
