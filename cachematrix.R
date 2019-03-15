## Overall, these two functions cache the inverse of a special metrix


## This function creates an object to store the inverse of the metrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculate the inverse of the special metrix returned by the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
       
}
