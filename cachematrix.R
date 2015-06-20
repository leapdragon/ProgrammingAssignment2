## Put comments here that give an overall description of what your
## functions do

## This function is rather similar to what happens in object-oriented
## languages when a new object type is created, then methods are created
## for it. In this case, the object is a Matrix object for which we
## are interested in maintaining a cached version of its inverse.
##
## 'set' is analagous to the constructor, and gives us an instance
## of the object (in this instance, in R, it is a list containing
## functions and objects intrinsic to the implementation, that are
## analagous to data and methods)
##
## 'get' returns the 'value' of the object (the matrix for this list)
##
## 'setinv' sets a value of the (presumed--it does not check) solved
## (inverted) matrix
##
## 'getinv' returns the current value of the (presumed) solved (inverted)
## matrix
##
## Of note is the fact that during 'construction' of the list, the
## value of the inverted solution is set to NULL, so that when these
## functions are called, we have a way to determine whether the value
## of the inverse is "good," i.e. whether it has yet been calculated
## and stored.
##
## Note that changing the value of the matrix either via a new assignment
## to the same variable, or by using the set() function, automatically
## "clears" the cache, as desired.

makeCacheMatrix <- function(x = matrix()) {
       	i <- NULL
        set <- function(y) {
           	x <<- y
           	i <<- NULL
        }
        get <- function() x
        setinv <- function( solved ) i <<- solved
        getinv <- function() i
        list(set = set, get = get,
           	setinv = setinv,
           	getinv = getinv)
}

## The 'cacheSolve' function makes use of our 'object' above to
## essentially enable matrix inversions that have intrinsic caching.
##
## It requires that we pass in a list object ot the type created
## by makeCacheMatrix whose matrix value has already been set, e.g.
## by using:
##
## foo <- makeCacheMatrix( matrix( c( somevalues...), somesize, somesize ) )
##
## Assuming that this step has been completed, foo can be passed to
## cacheSolve to either (a) calculate and return its inverse matrix, storing
## the result also in the cache, or (b) return the cached inverse matrix
## if one has already been stored.

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if( !is.null( i )) {
		message("getting cached data")
		return( i )
	}
	mtrx <- x$get()
	i <- solve( mtrx )
	x$setinv( i )
	i
}
