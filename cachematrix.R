## The file, cachematrix.R, is for the course, R Programming on Coursera,
## Programming assignment 2. It is for caching the inverse of a matrix.


## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s_inv <- NULL
    set <- function(y) {
        x <<- y
        s_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) s_inv <<- inv
    getinv <- function() s_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates the inverse of the special matrix 
## it first checks to see if the inverse matrix has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value in the 
## cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
