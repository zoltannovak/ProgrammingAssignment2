## Implement matrix cache and inverse calculation 
## Usage example:
## source("cacheSolve.R")
## source("makeCacheMatrix.R")
## cache <- makeCacheMatrix()
## tm <- "(put your invertible matrix here)"
## cache$set(tm)
## 
## // invert matrix calculated
## cacheSolve(cache) 
## 
## // invert matrix from cache
## cacheSolve(cache)




##creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##setInverse the value of the inverse matrix
##getInverse the value of the inverse matrix
makeCacheMatrix <- function(m = numeric()) {
               
        ## creates local inverseMatrix variable
        inverseMatrix <- NULL

        ## set - sets the value of the matrix & NULLs the value of the already stored inverse matrix
        set <- function(y) {
                m <<- y
                inverseMatrix <<- NULL
        }

        ## get - gets the value of the matrix that the inverse matrix will store the inverse value for
        get <- function() m

        ## setInverse - sets the value of the inverse matrix
        setInverse <- function(inverse) {
                inverseMatrix <<- inverse
        }
        
        ## getInverse - gets the value of the inverse matrix
        getInverse <- function() {
                return(inverseMatrix)
        }
        
        list(set = set, get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
##
## Parameters:
##
## x - the cache instance (makeCacheMatrix function)
cacheSolve <- function(x, ...) {
        
        ## in case the inverse is already calculated in cache, return the cached instance
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 

        ## getting the matrix stored in the matrix cache
        data <- x$get()

        ## calculating the inverse matrix
        m <- solve(data)
        
        ## setting the inverse for future references
        x$setInverse(m)
        
        ## returning the inverse
        m
}
