        ## The following function creates a kind "matrix" that can cache its inverse.
        ## To save computing time inverted matrices are cached
        ## Using Set to set new matrix also deletes cach.
        ## Cache is populated by calling function cachesolve(x)
        ## x is a data matrix, from the original data
        ## inverse is an inverse matrix. Inverse is also set to solve



makeCacheMatrix <- function(x = matrix()) {
        
        #if the matrix is not square the function is terminated
        if (nrow(x)!=ncol(x)) stop("The matrix is not invertible because it is a non-square matrix")
        
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        
        # setsolve sets the inverse of the original matrix and store it
        setsolve <- function(solve) inverse <<- solve
        
        # getsolve retrieves the inverse of the original matrix, it returns NULL if none exists 
        getsolve <- function() inverse
        
        # Creates a list of internal functions
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
}


        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
        ## Returns cache if already present.

cachesolve <- function(x, ...) {
        inverse <- x$getsolve()         # retrieves the inverse of the matrix
        
        # Checks and returns the inverse that has been calculated and cached
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)        
        }
        data <- x$get()
        inverse <- solve(data, ...)      # the inverse matrix is calculated
        x$setsolve(inverse)              # caches the calculated inverse matrix
        inverse                          # returns the inverse matrix
}
