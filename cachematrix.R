# Assignment: Lexical Scoping
# Caching the Inverse of a matrix

#Function 1: makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        # Create empty 'inv' object to later assign value from a different environment through use of '<<-'
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        # List containing above functions to set and get a matrix and its inverse, respectively
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#Function 2: cacheSolve
cachesolve <- function(x) {
        # Returns calculation of inverse of makeCacheMatrix matrix
        inv = x$getinv()
        # If prev calc is available - i.e. an inverse of matrix with these values has been cached previously, meaning 'inv' is not empty this inverse is retrieved and returned without unneccessary computations to re-calculate.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        # If a previous calculation is not available, cacheSolve calculates inverse of matrix...
        mat.data = x$get()
        inv = solve(mat.data, ...)
        #...and sets in cache.
        x$setinv(inv)
        return(inv)   
}