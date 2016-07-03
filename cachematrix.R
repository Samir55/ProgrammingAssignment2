## Put comments here that give an overall description of what your
## functions do

## The first function makeCacheMatrix creates a list containing
## 1. an empty matrix as default unless the user send a matrix as an argument
## 2. get the matrix
## 3. set inverse matrix
## 4. get inverse matrix

## The second function cacheSolve computes and returns the inverse of the matrix
##after checking it's computed or not


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        # New matrix assigned so we have to assign NULL to the inverse matrix
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(y) {
        i <<- y
    }
    getInverse <- function() i
    # returns a list containing (set the matrix func, get the matrix, set the inverse to be
    # called by cacheSolve , get the inverse)
    list ( set = set, get = get, setinverse = setInverse, getinverse = getInverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #checking if the inverse haven't been computed yet
    if(is.null(x$getinverse()) ){
        ## we have passed ... as the user may need to add some parameters in solve function
        x$setinverse( solve(x$get(), ...) )
        return (x$getinverse())
    }
    (x$getinverse())
}
