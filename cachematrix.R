## Put comments here that give an overall description of what your
## functions do

## this function takes a matrix as a parmeter and returns a list of functions
## set - used to the cache the input matrix and initialize the inverse matrix value to null
## get - used to get the input matrix
## setinverse - used to cache the inverse of the input matrix 
## getinverse - used to acquire the cached inverse matrix 
makeCacheInverse<-function(x=matrix()){
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse<<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function takes a list as input this list is
## generally, the list returned in the above function
## first the getinverse() function is used to access
## the inverse matrix value
## if it's not equal to null i.e it's not set
## the inverse matrix value is calculated 
## and set using the setinverse function
## and the inverse of the input matrix is returned

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(x$get())
        x$setinverse(inverse)
        inverse
}
