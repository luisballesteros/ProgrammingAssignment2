## makeCacheMatrix  creates a special "matrix", which is really a list
## containing a function to
## 1.-set the value of the matrix
## 2.-get the value of the matrix
## 3.-set the value of the inv
## 4.-get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
#Calculates the inverse of an array if it is not cached and if it is cached
# returns the cached array.
# It takes the getsolve() function of makeCacheMatrix() and if the value 
# is null (not cached), it calculates the inverse of the matrix with solve().
# If it is, it returns its value.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
