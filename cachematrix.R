## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function creates a special matrix that can store or cache itself and its inverse matrix once it is calculated and stored
# the function assumes that the matrix sent in as argument is an invertible matrix and does not do any validations for the same
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

#cacheSolve function computes the inverse of the special matrix created by the above function of makeCacheMatrix
# the cacheSolve checks if the special matrix has the inverse cached and if yes returns the cached inverse
# if no cached inverse if found, the function computes the inverse, sets it in the special matrix and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInv()
        if(!is.null(m)) {
                message("returnung cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m

}
