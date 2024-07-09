##These functions allow for caching of a matrix's inverse to reduce computation

##This function can cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(solved) inv <<- solved
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##This function solves for the inverse matrix, or retrieves it if already present

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
