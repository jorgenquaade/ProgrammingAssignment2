## The 2 funtions below work is unison to cache inverted matrices. 
## The makeCacheMatrix function defines the functions needed to set and get
## inverted matrices. The cachesolve function will check if a matrix has been
## inverted and will then get the value from memory or proceed to set 
## the inverted matrix with. The free variable i is used to signal whether
## the inverse of x has been calculated and to return the inverse of x

## The makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
	## the variable i is used to signal whether 
	## the inverse has been previously calculated or not.

	i <- NULL

	## The set function is not used in this example but keeping 
	## it for completeness. i is set to NULL since the inverse 
	## has not yet been calculated.
	set <- function(y) {
               x <<- y
               i <<- NULL
      }
	## The get function returns the matrix the cacheSolve
	## function was called with.
 	get <- function() x

	## Here setinv is defined. It sets i to a value
	## to signal that the matrix x has been inverted already.
      setinv <- function(solve) i <<- solve

	## getinv returns i so that it can be tested if 
	## setinv has been called.
      getinv <- function() i

	## returning the list of functions defined in makeCacheMatrix.
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function checks to see if x has already been inverted
## If it has been inverted it returns the cached result and if not inverts
## it and returns the result. The variable i is used to test if 
## the setinv function has been called.

cacheSolve <- function(x, ...) {
	  ## Calling the function getinv() defined above
	  i <- x$getinv()	  
	  
	  ## i now has value of either NULL or solve
	  ## If i is defined as solve, ie not NULL, the inverse has been
	  ## calculated and will be returned from memory
	  if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

	  ## if we get to this point in the code i was NULL and we have to 
	  ## inverse the matrix x.
	  ## Using get to get the matrix and data to store the value
	  data <- x$get()

	  ## Now invert the matrix using the solve function
	  i <- solve(data, ...)

	  ## And call the setinv function to signal that x has been inverted
	  x$setinv(i)
	  
	  ## return a matrix that is the inverse of 'x'
	  i

}
