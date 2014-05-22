## Computing inverse matrix is computationaly expencive therefore we create 
## a "cached" version of this procedure, so previously cmoputed value can be reused

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        # if the inverse has already been calculated then retrieve from cache
        s <- x$getsolve()
        if(!is.null(s)) {
                return(s)
        }
        
        # otherwise compute inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}


# Optional benchmark:
#
# x <- matrix(rnorm(10^6), nrow=10^3, ncol=10^3)
# 
# a <- Sys.time()
# solve(x)
# t1 <-Sys.time() - a
# print(t)
# 
# a <- Sys.time()
# solve(x)
# t2 <-Sys.time() - a
# print(t)
# 
# s <- makeCacheMatrix(x)
# a <- Sys.time()
# cacheSolve(s)
# t3 <-Sys.time() - a
# print(t)
# 
# a <- Sys.time()
# cacheSolve(s)
# t4 <-Sys.time() - a
# print(t)

