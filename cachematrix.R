## This function caches the inverse of a matrix.

## Creates a list that takes a matrix as an input and caches its inverse.

## First it stores a default value for the inverse
## Then it creates four functions: set, get, setinverse, getinverse
## --set stores the input matrix (and resets the inverse)
## --get retrieves the input matrix
## --setinverse stores the inverse (as the value of an argument named 'solve')
## --getinverse retrieves the inverse 
## Then it returns a list containing the four functions

## Note that 'm' might be changed to 'inv' for clarity
## But I am making as few changes as possible

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

## Computes the inverse of the matrix stored in makeCacheMatrix.
## If the inverse has already been computed and the matrix is unchanged, then
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix held in 'x'
        ## Each '$' calls a function from the list 'x'

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

## Remember, here, 'x' is just some object where makeCacheMatrix is stored,
## for example: 
## > a <- makeCacheMatrix(SOME_MATRIX)
## > cacheSolve(a)
## Then 'x' is is just the 'a' object, and 'x$get' would be 'a$get', etc.