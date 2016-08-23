## This file holds two functions 
## 1) for creating a more informative matrix object, with which you're able to 
##    cache and retrieve a given matrix and its inverse (this might shorten the 
##    runtime of your code significantly) and
## 2) a function with which the inverse of the original matrix can be 
##    computed and cached or retrieved.



################################################################################
# This function takes a matrix for an argument and returns a special matrix 
# object, which is actually a list, holding four functions for retrieving or 
# caching the original matrix as well as its inverse; The original matrix which 
# is handed over as an argument gets cached by default
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    return(list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse))
}


################################################################################
# This function takes a special matrix object for an argument, the type which it
# is produced by the makeCacheMatrix function, as well as optional arguments.
# If not yet cached the inverse of the original matrix gets computed, cached
# and returned. If already cached, the inverse of the original matrix is 
# retrieved and returned, which is indicated by a message on the screen.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) # compute inverse of matrix
    x$setinverse(m)
    return(m)
}



