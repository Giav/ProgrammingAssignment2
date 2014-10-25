## Pair of functions to calculate the inverse of a matrix, store the inverse 
## and retrieve the inverse on request.
## Usage: 1: Store matrix to invert by call to makeCacheMatrix( a matrix )
##        2: Retrieve invert by call to cacheSolve( the makeCacheMatrix object)
## Error: Returned if matrix is not invertable
## 


## makeCacheMatrix 
## parameter : a matrix
## returns : true

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



## cacheSolve
## parameter1 : a makeCacheMatrix object
## parameter2 : optional arguments for solve()
## returns : inverse of matrix must be square & invertable


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
