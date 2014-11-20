
## makeCacheMatrix creates an object that is entered into cacheSolve to allow it to access or calculate the inverse of the matrix, "x" that is given the makeCacheMatrix. They work together to allow the inverse of the matrix to be calculated or fetched, as required, based on the values of a matrix which it stores.

## makeCacheMatrix stores the value of the original matrix and the cached inverse of this matrix (reset to "NULL" each time makeCacheMatrix is called), before cacheing the functions that get x (the original matrix), set the inverse and fetch the inverse.

makeCacheMatrix <- function(x = matrix()) {
				i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {i <<- inverse}
        getinverse <- function() {i}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks if there is an existing value for the inverse (i) of x. If there is then it returns that value, otherwise it fetches x from the cache and calculates the inverse using the solve function before returning it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                matrx <- x$get()
                i <- solve(a = matrx, ...)
                x$setinverse(i)
                i
}
