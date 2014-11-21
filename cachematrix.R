## We need to create functions that invert square matrix with the option
## of saving the result in cache. So next time when we need the inverse matrix,
## and initial one haven't change, we can just take the saved result.
## No need to perform the same computations again.

## In the first function, we construct the list of four functions
## that perform basic operations with the initial and the inverse matrixes

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The second function should return inverse matrix
## In case it already exists, we just retrieve the result of previous computations
## from cache.
## Otherwise, we compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
    
}

## This is the copy of the second function, but with the check of working time.
## So we can see the difference between computing the inverse matrix
## and retrieving the ready result from cache.

cacheSolve_checkTime <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    time1<-Sys.time()
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(list(inv,Sys.time()-time1))
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    list(inv,Sys.time()-time1)
    
}

