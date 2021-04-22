## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        # setting the value of vector
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # seting the inverse of the matrix using "solve"
    setinverse <- function(slove) m <<- solve
    # getting the inverse of the matrix cache
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # checking if inverse matrix has already calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if cache does not exsist, inverse is calculated.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
