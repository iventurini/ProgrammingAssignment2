## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               ## creates the variable i with NULL as default where the inverse will be cached
        set <- function(y) {    ## when called it will overwrite the value of x and assign NULL to the i, so the inverse should be computed again (no cache)
                x <<- y
                i <<- NULL
        }
        get <- function() x                ## when called it returns the matrix x
        setinv <- function(inv) i <<- inv  ## when called it will overwrite the value of i
        getinv <- function() i             ## when called it returns the value of i   
        ## Return the special "matrix"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ## x should be the special matrix, the ... allow passing more arguments to solve function
        
        i <- x$getinv()           
        if(!is.null(i)) {         ## evaluate the cached inverse, and if it is already computed return it
                message("getting cached data")
                return(i)
        }
        data <- x$get()           ##get the matrix itself, compute and chache the inverse of it
        i <- solve(data, ...)
        x$setinv(i)
        ## Return a matrix that is the inverse of 'x'
        i
        
}
