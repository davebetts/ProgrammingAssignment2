## Put comments here that give an overall description of what your
## functions do

# This function stores matrices via the list of functions saved within machCacheMatrix()
# a matrix can be entered by the user, or the user will be able to use the cachesolve()
# function to create and store the inverse matrix of an already existing matrix 
# that was previously stored within makeCacheMatrix().

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # create a variable where an inverted matrix could be stored
    set <- function(y) { # store a new matrix within makeCacheMatrix
    x <<- y
    m <<- NULL    # with a new matrix, any previously inverted matrix is dropped and restored to NULL
    }
    get <- function() x # returns a matrix stored within makeCacheMatrix()
    setMatrix <- function(solve) m <<- solve # assigns the value of a matrix to m
    # setMatrix provides a place to store the inverted matrix resulting from the use of cacheSolve()
    getMatrix <- function() m  # retrieves the current value of m (either a matrix or NULL if m is empty)
    list(set = set, get = get, #storing the functions within the larger function
    setMatrix = setMatrix,
    getMatrix = getMatrix) 
}


# cacheSolve will look for previously stored values in makeCacheMatrix or calculate and
# store the inversion of a matrix in makeCacheMatrix.
# The stored inverted value or the newly created inverted value are returned to the user.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()  # retrieves a value from makeCacheMatrix() 
    if(!is.null(m)) {   # if that value is not NULL, the value returned is the most recent inversion of a matrix
        message("getting cached data") 
        return(m)      # return the stored value of the inverted matrix
    }
    data <- x$get()    # if no stored inversion of a matrix exists, retrieve a non-inverted matrix from makeCacheMatrix()
    m <- solve(data, ...) # invert the retrieved matrix
    x$setMatrix(m)  # store the newly inverted matrix back in makeCacheMatrix()
    m # return the values of the newly created inverted matrix
}
