## This is for coursera assignment (week3) to write a code that can repeatedly compute the inverse of a matrix, cache the inverse of the matrix and return the inverse of the matrix. If the inverse has already been computed and the matrix has not changed, then the "cacheSolve" function retrieves the stored inverse from the cache.
## There are two functions: makeCacheMatrix and cacheSolve. the full description is below.

## The makeCacheMatrix function computes and caches the inverse of a matrix (x).

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve(x)
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. if the inverse has already been computed and the matrix has not changed, then this function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cahced data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}



