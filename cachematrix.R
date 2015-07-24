## Caching the Inverse of a Matrix
##  makeCacheMatrix(): Creates a object can cache its inverse.
##  cacheSolve():      Return cache of the inverse matrix if exist.
##  testCacheMatrix(): Test code of the above two functions.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}


## Test code of the above two functions.

testCacheMatrix <- function() {
	cat("Generate matrix\n")
	t <- proc.time()
	x <- matrix(runif(1000000),1000,1000);
	print(proc.time()-t)

	cat("\nMake cache matrix\n")
	t <- proc.time()
	y <- makeCacheMatrix(x)
	print(proc.time()-t)

	cat("\nCalculate inverse matrix\n")
	t <- proc.time()
	inv1 <- cacheSolve(y)
	print(proc.time()-t)

	cat("\nCalculate inverse matrix(called from cache)\n")
	t <- proc.time()
	inv1 <- cacheSolve(y)
	print(proc.time()-t)

	cat("\nCalculate inverse matrix usually\n")
	t <- proc.time()
	inv1 <- solve(x)
	print(proc.time()-t)
}

