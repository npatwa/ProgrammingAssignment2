## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function stores the matrix (as its argument) and inverse
## in its environment and it returns a pointer to that environment
## which can be directly used to call the get, set,getinverse and setinverse fns

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(sol) m <<- sol
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function is passed the enviroment (created by the function 
##  above) and it invokes the get, getinverse and setinverse functions on the
##  environment to generate the inverse matrix(if not already cached)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
