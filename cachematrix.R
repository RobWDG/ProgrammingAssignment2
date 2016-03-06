## These functions allow the user to cache a matrix and also the inverse of the matrix
## if the inverse matrix hasn't been created it creates it or if the matrix has changed then it recreates the inverse for this new matrix

## This function saves the matrix value x and provides a set method to save it and a get method to retrieve it
## It also saves the inverse of the matrix x and provides a setinverse method and a getinverse method to retrieve it 


makeCacheMatrix <- function(x = matrix()) {

 i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function returns the precached inverse matrix unless the matrix has changed or the inverse hasn't been precached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  i <- x$getinverse()
	#check to see if i is not empty and if the matrix hasn't changed
	
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
