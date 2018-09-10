## Implementation of a matrix with a cachable inverse


## The matrix constructor function. Input matrix `x` and
##   can be accessed with set, get, setinv, and get inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           ## Initialize inverse to NULL      
    set <- function(y) {  ## Set matrix to y
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse   ## Assign inverse to inv
    getinv <- function() inv
    list(set = set, get = get,                    ## Define function list
         setinv = setinv,
         getinv = getinv)
}


## Gets the inverse of the matrix x, from cache if available

cacheSolve <- function(x, ...) {
        inv <- x$getinv()       ## Try to get the inverse from x
        if(!is.null(inv)) {     ## If it exists in the cache, return it
            message("getting cached data")
            return(inv)
        }
        data <- x$get()         ## If it doesn't, calculate it and 
        inv <- solve(data, ...) ##   assign it to x$inv
        x$setinv(inv)
        inv
}
