##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##makeCacheMatrix stores within a list the functions set, get, setinverse, and getinverse.
##The "get" function returns the stored matrix object x while the "set" function 
##resets the matrix object x.
##The function "setinverse" uses the "<<-" operator to set and cache the
##inverse of x as "inversematrix" while the function "getinverse" returns "inversematrix".
makeCacheMatrix <- function(x = matrix()) {
	inversematrix <- NULL
	set <- function(y) {
		x <<- y
		inversematrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inversematrix <<- inverse
	getinverse <- function() inversematrix
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


##cacheSolve attempts to retrieve the cached inverse returned by the "getinverse" function in makeCacheMatrix.
##If a calculated inverse is retrieved, cachesolve returns the inverse to the calling function. 
##If the inverse has not been calculated (or the matrix has changed), cacheSolve imploys the "get" and the
##"setinverse" functions (stored in makeCacheMatrix) to compute and cache the inverse of the "matrix" object.
##The calculated inverse is then returned to the calling function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inversematrix <- x$getinverse()
	if(!is.null(inversematrix)) {
		message("getting cached data")
		return(inversematrix)
	}
	data <- x$get()
	inversematrix <- solve(data, ...)
	x$setinverse(inversematrix)
	inversematrix
}
