## Finding the inverse of a matrix if it is not already calculated and then
## caching them later use

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
        
        inv <- NULL
        
        set <- function( matrix ) {
                m <<- matrix
                inv <<- NULL
        }
        
        get <- function() {
                m
        }
        
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        getInv <- function() {
                inv
        }
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInv()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInv(m)
        
        m
}
