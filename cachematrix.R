## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly.

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        # set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # get the value of the matrix
        get <- function() x

        # set the value of the inverse
        setmatrix <- function(solve) m <<- solve

        # get the value of the inverse
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {

        # NOTE : If the inverse has already been calculated (and the
        # matrix has not changed), then the cachesolve should retrieve
        # the inverse from the cache.

        m <- x$getmatrix()

        # use the cached inverse
        if(!is.null(m)) {
                message("getting the previously cached inverse matrix")
                return(m)
        }

        matrix <- x$get()
        
        # return a matrix that is the inverse of 'x'
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
