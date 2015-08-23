## Compute for the inverse matrix using "solve" function and then save the result in cache environment
## the makeCacheMatrix function expect a matrix argument (e.g. rbind(c(2,0,0), c(0,2,1), c(2,0,2)) -- this is a 3x3 matrix)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list(set = set, get = get, 
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)

}


## Function which computes for the inverse matrix. If the matrix is not in the cache, try to solve it and save in the cache.
## else return the cache inverse matrix.
cacheSolve <- function(x, ...) {
    m <- x$getinversematrix()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m    
}
