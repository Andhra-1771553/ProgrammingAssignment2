## (makeCacheMatrix, cacheSolve) is a pair of functions that cache the inverse of a matrix

## Following is a function for a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i       <- NULL
        set     <- function(y){
                x  <<- y
                i  <<- NULL
        }
        get     <- function() x
        setinv  <- function(isolve) i <<- isolve 
        getinv  <- function() i 
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
        
}


## The following function "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix"

cacheSolve      <- function(x, ...) {
        
        i       <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return (i)
        }
        data    <- x$get()
        i       <- solve(data)
        x$setinv(i)
        i
}

## Checking the pair of functions
## Its important to keep the size of matrix to square, otherwise "Error in solve.default(data) : 'a' (4 x 3) must be square" will pop up!

s               <- matrix(rpois(25,2),5,5)
special         <- makeCacheMatrix(s)
cacheSolve(special)

## Results: Program worked like a gem.!

