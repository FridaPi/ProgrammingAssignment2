## This program calculates the inverse matrix and stores it in cache so that it is 
##saved and does not have to be recalculated next time.


## creates a special matrix to store the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse has been calculated and if so, grabs it from the cache.
## If the inverse is not availble in the cache, the inverse is calculated

cacheSolve <- function(x, ...) {
        mi <- x$getinverse()
        if (!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
        
}
B<-matrix(c(1:4),2,2)
B1<-makeCacheMatrix(B)
cacheSolve(B1)