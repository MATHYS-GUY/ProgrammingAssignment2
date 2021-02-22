# These functions caches the inverse of a matrix.
# To generate the cache inverse matrix
#    makeCacheMatrix(x)}
# where a is an ordinary matrix
# To get the inverse
#   with {cacheSolve(x)}.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
#  set the value of the matrix
#  get the value of the matrix
#  set the value of inverse of the matrix
#  get the value of inverse of the matrix

makeCacheMatrix <- function(x) 
{
        invmat <- NULL
        set <- function(y) 
        {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmat <<-solve
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...)
{
        invmat <- x$getinv()
        if(!is.null(invmat)) 
        {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}        


# #sample main for testing purposes
# x <- matrix(1:4, ncol = 2, nrow =2)
# invmat <- makeCacheMatrix(x)
# invmat$get()
# 
# # No cache in the first run
# cacheSolve(invmat)
# 
# # Retrieving from the cache in the second run
# cacheSolve(invmat)
# 
# 

