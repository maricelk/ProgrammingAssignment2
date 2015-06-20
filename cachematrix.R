## makeCacheMatrix creates a special matris that can 
##cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m  
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve computes the inverse of the special matric
##returned by makeCacheMatrix
cacheSolve <- function(x, ...) { 
        m<- x$getInverse()
        if(!is.null(m)) {
                message("Getting the inverse from the cache")
                return(m)
        }
        matrix <- x$get()
        m<- solve(matrix, ...)
        x$setInverse(m)
        m        
}
