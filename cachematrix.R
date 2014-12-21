## makeCacheMatrix creates a special vector which is a list containing get/set functions
## cacheSolve calls the solve function which retunrs the inverse of a matrix

## This function creates a list containing functions to set/get the value of a matrix
## and set/get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function calculates the inverse of the matrix
## if the inverse has already been cached it will return cached data otherwise call the solve function to calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getmatrix()
         ## if inverse matrix is already cached return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise calculate inverse matrix and return it
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
