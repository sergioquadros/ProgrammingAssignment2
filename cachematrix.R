## 2014-09-18 shsq
## See example at botton

## Function "makeCacheMatrix"
## Input:  a matrix invertible 'x'
## Output: a list with $names=("set","get","setinv","getsolve")
##         and superassigment in 'inv' at her environment
##         inv=solve(x) numerically in cache

makeCacheMatrix <- function(x = matrix(),...) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get<- function() x
        setinv<- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getsolve = getsolve)
}

## Function "cacheSolve"
## Input:       a list with $names=("set","get","setinv","getsolve")
##              result from function "makeCacheMatrix"
## Output:      an inverse matrix of 'x' stayed in cache 
##              superassigment to 'inv' at her environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
# EXAMPLE:
# > x
#       [,1] [,2] [,3]
# [1,]   -1   -2    2
# [2,]   -2    2    1
# [3,]    2    1    2
# > xm<-makeCacheMatrix(x)
# > xc<-cacheSolve(xm)
# > xc  ###  Inverse Matrix in cache
#          [,1]       [,2]      [,3]
# [1,] -0.1111111 -0.2222222 0.2222222
# [2,] -0.2222222  0.2222222 0.1111111
# [3,]  0.2222222  0.1111111 0.2222222

# X*X⁻¹=I from linear algebra below
# > x%*%xc   ###  Identity Matrix for little epsilon
#           [,1]         [,2]       [,3]
# [1,]  1.000000e+00  2.775558e-17    0
# [2,]  0.000000e+00  1.000000e+00    0
# [3,] -1.110223e-16 -2.775558e-17    1
# 
