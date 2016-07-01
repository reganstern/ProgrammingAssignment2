## Together, makeCacheMatrix and cacheSolve, provide a set of functions that can be used to set
## and retrive a matrix and it's inverse. If the inverse matrix has not been determined, the cacheSolve
## function will use solve to get the inverse matrix and cache it in the passed object.
## Per the assignment instructions, it is assumed that the matrix is invertible. An error is thrown
## by solve if that is not the case.

## Stores and returns an original input matrix and it's 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        #inverse matrix
        inverse <- NULL
        
        if((class(x) != "matrix") | (mode(x) != "numeric"))
                stop("input variable must be a numeric matrix")
        #matrix
        sm <- x
        
        isCacheMatrix <- function() TRUE
        
        getMatrix <- function() sm
        
        setMatrix <- function(y){
                sm <<- y
                inverse <<- NULL
        }
        
        setInverse <- function(x)
                inverse <<- x
        
        getInverse <- function()
                inverse
        
        list(getMatrix = getMatrix, 
             setMatrix = setMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse,
             isCacheMatrix = isCacheMatrix)
}



## Takes an object of type makeCacheMatrix and returns the inverse
## of the matrix stored on the makeCacheMatrix object. 
## If the inverse matrix is stored on the makeCacheMatrix object,
## it is retrieved and returned. If it is not stored (returns NULL), it is
## computed, stored on the makeCacheMatrix object, and returned.

cacheSolve <- function(x, ...) {

        if(!x$isCacheMatrix())
                stop("x must be an instance of makeCacheMatrix.")
        
        inverse <- x$getInverse()
        
        if(!is.null(inverse))
                return(inverse)
        
        x$setInverse(solve(x$getMatrix()))
        x$getInverse()
        
}
