## functions used to create inverse of a matrix and cache the result.

## the makeCacheMatrix function creates a
## list containing functions to do the following:

## 1.  set the value of the matrix.
## 2.  get the value of the matrix.
## 3.  set the value of the matrix inverse.
## 4.  get the value of the matrix inverse.

makeCacheMatrix <- function(mat = matrix()) {
        
        matInv <- NULL ## intialize matrix inverse variable.
        
        set <- function(newMat) { ## set the matrix variable.
                mat <<- newMat
                matInv <<- NULL
        }
        
        get <- function() {mat} ## get the matrix variable.
        
        setInvMat <- function(inv) {matInv <<- inv} ## set the matrix inverse using "<<-" operator to cache the value.
        
        getInvMat <- function() {matInv} ## get the matrix inverse.
        
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat) ## create list of function names.
        
}


## this function will return a matrix that is the inverse of the matrix passed
## as the first argument.

cacheSolve <- function(mat, ...) {
        
        mI <- mat$getInvMat() ## initialize matrix inverse variable.
        
        if(!is.null(mI)) {
                message("getting cached data")  ## check to see if value is cached and use cached value if available.
                return(mI)
        }
        
        data <- mat$get() ## get matrix.
        
        mI <- solve(data, ...) ## find matrix inverse value.
        
        mat$setInvMat(mI) ## assign matrix inverse value to return variable
        
        return(mI)
}
