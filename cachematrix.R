## Put comments here that give an overall description of what your
## functions do
##------------------------------------------------------------------------------------------------------------
# the script has two functions - makeCacheMatrix, cacheSolve
# the function makeCacheMatrix creates a special matrix as described below
# cacheSolve function is used to get the cached or new value of the matrix that was input to the makeCacheMatrix function
##------------------------------------------------------------------------------------------------------------


## ------------------------------------------------------------------------------------------------------------
## comments for function makeCacheMatrix
## ------------------------------------------------------------------------------------------------------------
# makeCacheMatrix function creates a special matrix that can store or cache itself and its inverse matrix once it is calculated and stored
# the function assumes that the matrix sent in as argument is an invertible matrix and does not do any validations for the same
## ------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse
        setInv <- function(inv) m <<- inv

		#get the value of the inverse
        getInv <- function() m
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## ------------------------------------------------------------------------------------------------------------
## comments for function cacheSolve
## ------------------------------------------------------------------------------------------------------------
# cacheSolve function computes the inverse of the special matrix created by the above function of makeCacheMatrix
# the cacheSolve checks if the special matrix has the inverse cached and if yes returns the cached inverse
# if no cached inverse is found, the function computes the inverse, sets it in the special matrix and returns the inverse of the matrix
## ------------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInv()
        if(!is.null(m)) {
                message("returning cached matrix data")
                return(m)
        }
        
        # no cached inverse was found, so get the data, compute the inverse, cache it and return the value
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m

}

## ------------------------------------------------------------------------------------------------------------
## Test : Steps to test the functions above
## > n<-3
## > mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
## > matCached <- makeCacheMatrix(mat)
## > matSolved1 <- cacheSolve(matCached)
## > matSolved2 <- cacheSolve(matCached)
##   returning cached matrix data
## > identical(matSolved1, matSolved2)
## ------------------------------------------------------------------------------------------------------------