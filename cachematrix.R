## These pair of functions create a special matrix object that is able 
## to compute and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Arguments:
## x an inversible matrix

makeCacheMatrix <- function(x = matrix()) {
        ## creates the variable i with NULL as default where the inverse will be cached
        i <- NULL               
        
        ## Set closure: when called it will overwrite the value of x and assign NULL to i
        ## so the inverse should be computed again (no cache)
        set <- function(y) {    
                x <<- y
                i <<- NULL
        }
        
        ##get closure: when called it returns the matrix x
        get <- function() x                
        
        ##setinv closuser:  when called it will overwrite the value of i
        setinv <- function(inv) i <<- inv  
        
        ##get inv closure:  when called it returns the value of i   
        getinv <- function() i             
        
        ## Return the special "matrix"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.

## Arguments: 
## x should be the special matrix
## ... allow passing more arguments to solve function

cacheSolve <- function(x, ...) {  
        
        ## evaluate the cached inverse and if it is already computed return it
        i <- x$getinv()           
        if(!is.null(i)) {         
                message("getting cached data")
                return(i)
        }
        
        ## get the matrix itself, compute and cache the inverse of it
        data <- x$get()           
        i <- solve(data, ...)
        x$setinv(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}
