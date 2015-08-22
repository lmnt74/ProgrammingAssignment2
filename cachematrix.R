##this file is create a Cache Matrix and then reference it back in the event that a matrix
##is already stored with the same name

## this function caches the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ##set value of matrix
    set <- function (y){
        x <<- y
        m <<- NULL
    }
    ##get value of matrix
    get <- function() x
    ##set value of inverse
    setInverse <- function(solve) {
        m <<- sapply(x, FUN=solve, simplify = "array")
    }
    ##get value of inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##this function solves if there is already a cached version of this matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## if cached, return message
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ##else below, if not cached
    data <- x$get()
    ## solve
    m <- solve(data, ...)
    x$setInverse(m)
    m
}