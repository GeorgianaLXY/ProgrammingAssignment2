## this function aims caching the inverse of a matrix calculated 
## during computing 
## to avoid repeating calculating of this inverse
## with the hopes of reducing time spent  

## this function caches the inverse of the matrix
## for extraction later 
## to avoid repeating computation

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
    )
}


## calculate cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}

