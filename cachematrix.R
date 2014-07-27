## Put comments here that give an overall description of what your
## functions do

# Author Sunil Buge - week 3 completed on 26 July 2014
# Assignment: Caching the Inverse of a Matrix
# Assemtion - assume that the matrix supplied is always invertible

## Write a short comment describing this function
# function wriiten to create cache matrix
makeCacheMatrix <- function(x = matrix()) {
        myinv <- NULL
        set <- function(y) {
                x <<- y
                myinv <<- NULL
        }
        get <- function() x
        setmyInv <- function(i) myinv <<- i
        getmyInv <- function() myinv
        list(set = set, get = get,
             setmyInv = setmyInv,
             getmyInv = getmyInv)
}


## Write a short comment describing this function
# function wriiten to read cache matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myinv <- x$getmyInv()
        if(!is.null(myinv)) {
                print("Retrive cached inversion matrix from myinv")
                return(myinv)
        }
        data <- x$get()
        myinv <- solve(data, ...)
        x$setmyInv(myinv)
        myinv
}
