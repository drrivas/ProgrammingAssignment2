## Contains two functions.
## The makeCacheMatrix constructs a list which contains functions
## for the behavior of the cached matrix object
## casheSolve returns matrix inverse for cached matrix object
## and saves it for future computations

##Constructor function for creating a cached matrix object 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(knowninverse) inverse <<- knowninverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Takes in a cached matrix object
## Returns inverse matrix either  
## Using previously computed value if known
## Otherwise uses solve to calculate the inverse matrix 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("Getting Cached Data")
        return(inv)
    }
    data<-x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
