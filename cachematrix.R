## Comments have been added throughout the script for the sake of ease

## "function creates a special "matrix" object that can cache its inverse"

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse
        i <- NULL
        
        ## Set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the matrix and return value
        get <- function() {
                m
        }
        
        ## Set inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse of the matrix and return
        getInverse <- function() {
                i
        }
        
        ## Return list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## "function computes the inverse of the special "matrix" returned by makeCacheMatrix above"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Rreturn the inverse if already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get matrix 
        data <- x$get()
        
        ## Calculate inverse
        m <- solve(data) %*% data
        
        ## Set inverse and return value
        x$setInverse(m)
        m
}
