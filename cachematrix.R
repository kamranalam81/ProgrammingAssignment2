##The first function, makeVector creates a special "makeCacheMatrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse Matrix
#get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
    ## matrix parametrization    
	set <- function(y) {
         x <<- y
         m <<- NULL
     }
	 ##return matrix parameters
     get <- function() x
     ##set the matrix value into cache
	 setmatrix <- function(mtrx) m <<- mtrx
     ##return inverse matrix from cache
	 getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## The following function calculates the inverse matrix of the special "matrix" created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getmatrix()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
	 ## store matrix into the variable data
     data <- x$get()
     ## calculate matrix inverse
	 m <- solve(data)
     ## call setmatrix
	 x$setmatrix(m)
     m
	}
