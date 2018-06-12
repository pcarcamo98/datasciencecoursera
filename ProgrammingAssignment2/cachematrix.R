## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #create an inverse value
        
        #set the original matrix and reset inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        #get original matrix
        get <- function() x
        
        #set inverse value
        set_inverse <- function(inv) inverse <<- inv
        
        #get inverse value
        get_inverse <- function() inverse
        
        #return list of 4 functions, making it into the "special" matrix
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}
