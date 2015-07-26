## makeCacheMatrix stores the inverse of a matrix into the cache
## cacheSolve returns the stored inverse matrix from the cache

## makeCacheMatrix takes a matrix input and computes the inverse
## to be saved to the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## cacheSolve computes the inverse of a matrix and saves it to the cache 
## unless it has already been cached, in which case it returns the previously 
## cached result

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    return(m)
}
