## cachematrix.R shows the functionality of caching in R
## This feature can be used to improve efficiency by calculating a value only once..
## ..instead of calculating every time it's accessed.
## There are 2 methods: makeCacheMatrix and cacheSolve
## Usage:-
## >source("cachematrix.R")
## >mymatrix <- makeCacheMatrix(matrix(c(4,2,7,6), ncol=2))
## >cacheSolve(mymatrix)
## Any subsequent call to mymatrix$getinverse() or cacheSolve(mymatrix) will 
## return the cached value

## function: makeCacheMatrix
## Input: Invertible Square Matrix
## Output: A list of setters and getters for the matrix value and it's inverse
## Description: Given an invertible square matrix, creates a list which gives access
##              to the input matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Check if input is correct
    if(!is.matrix(x) | ncol(x) != nrow(x)) {
        stop("makeCacheMatrix can only work on a square matrix")
    }
    
    ## Initialize inv which represents the inverse 
    inv <- NULL
    
    ## Setup functions: set, get, setinverse and getinverse  
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## Return as list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## function: cacheSolve
## Input: Expects a list which was created using makeCacheMatrix function
## Output: Returns the inverse of the matrix representing the input list.
## Description: If inverse value is already cached, returns the cached inverse value.
##          Otherwise, calculates the inverse, sets the cache and then returns the inverse.
cacheSolve <- function(x, ...) {
    ## Check to see if inverse is already cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## Return the cached inverse
        message("Returning cached inverse")
        return(inv)
    }
    
    ## Calculate inverse
    data <- x$get()
    inv <- solve(data)
    
    ## Save the inverse
    x$setinverse(inv)
    
    ## Return
    inv
}
