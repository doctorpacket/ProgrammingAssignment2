# makeCacheMatrix returns a list of functions related to an invertible matrix
#
# cacheSolve returns either the cached value of x's inverse matrix
# or compute/recomputes the inverse matrix

# Returns a list of functions related to an invertible matrix
# param x   the object encapsulating the invertible matrix
# return    a list of four functions related to an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInvM <- function(i) invM <<- i
    getInvM <- function() invM
    list(set = set,
         get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


# Returns either the cached value of x's inverse matrix
# or compute/recomputes the inverse matrix
# param x   the object encapsulating the invertible matrix
# return    the cached or recomputed inverse matrix
cacheSolve <- function(x, ...) {
    invM <- x$getInvM()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setInvM(invM)
    invM
}
