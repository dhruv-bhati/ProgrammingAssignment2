## This function creates methods for getting inverse of the matrix and setting inverse
## of the matrix. This function also defines methods to get and the matrix data.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverseMatrix <<- inv
    getInverse <- function() inverseMatrix
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function checks if inverse of the matrix is already known and returns the 
## inverse if it is known otherwise this function calculates the inverse of the 
## matrix and stores in it inverseMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null()) {
        message("Getting cached inverse")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
