## Computing the inverse of a Square Matrix, if the inverse of the Square Matrix was onced computed,
## keep it in cache for next fast retrive.

## Create a matrix with 4 operations as follows
## setMt/getMt: Set/get the square matrix
## setRMt/getRMt: Set/get the inverse of the specified suqare matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Inverse of the square matrix 
    r <- NULL
    setMt <- function(y){
        x <<- y
        r <<- NULL
    }
    getMt <- function() x
    setRMt <- function(y) r <<- y
    getRMt <- function() r
    ## return a list contains all the 4 opetations
    list(setMt = setMt, getMt = getMt, setRMt = setRMt, getRMt = getRMt)

}


## Calculate the inverse of the square matrix if onced not computed
## and then return the inverse matrix

cacheSolve <- function(x, ...) {
            
    r <- x$getRMt()
    ## Check whether the inverse matrix has already cached
    if(!is.null(r)){
        message("Get cache inverse matrix")
        return (r)
    }
    m <- x$getMt()
    r <- solve(m, ...)
    ## Save the computed result in cache
    x$setRMt(r)
    r
}
