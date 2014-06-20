## The function makeCacheMatrix creates a special object - a matrix,
## and stores an inverse of this matrix.
## The function CacheSolve returns an inverse, either cached or calculated.

## makeCacheMatrix: takes a matrix as argument and creates an object which stores
## this matrix (object x), inverse matrix (which is empty in the beginning) and 4 functions.
## The function returns a list of 4 objects (functions) and names the items in the list.

makeCacheMatrix <- function(x = matrix()) {
        stored_inverse <- NULL
        set <- function(new_matrix) {
                x <<- new_matrix
                stored_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(new_inverse) stored_inverse <<- new_inverse
        getinverse <- function() stored_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve takes a matrix object (and "..." - all its "internals")
## then uses this object's internal getinverse() function to get stored_inverse
## from this object.
## If the valid value is found within the object - then stored_inverse is returned (and 
## the function terminates), if the value found is NULL - then the matrix was either
## changed or its inverse was never calculated. So a new inverse gets calculated,
## written into the object and returned.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting stored inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
