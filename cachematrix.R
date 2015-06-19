## ---------------------------------------------------------------------------------------------------------------------
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The two functions below are used to cache the inverse of a matrix.
##----------------------------------------------------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It creates a list that contains function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## ---------------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##------------------------------------------------------------------------------------------------------------------------
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
## If not, it computes the inverse, sets the value in the cache via setinverse function.
## This function assumes that the matrix is always invertible.
##
## Computing the inverse of a square matrix is  done with the solve function in R
## ---------------------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) 
{
 ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("return cached inverse.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
} 

# ---------------------------------------------------------------------------------------------------------------------
# The above functions have been successfully tested using the following sample test cases.
#
# Sample Test Cases and Output
#
# > source("cachematrix.R")
# > m=matrix(c(9,10,3,20,25,30,35,40,45,50,3,8,65,70,75,80), nrow=4, byrow=T)
# > m
#      [,1] [,2] [,3] [,4]
# [1,]    9   10    3   20
# [2,]   25   30   35   40
# [3,]   45   50    3    8
# [4,]   65   70   75   80
# > nm = makeCacheMatrix(m)
# > cacheSolve(nm)
#         [,1]         [,2]         [,3]         [,4]
# [1,]  0.0625 -0.341826923 -0.028846154  0.158173077
# [2,] -0.0625  0.307211538  0.048076923 -0.142788462
# [3,] -0.0625  0.011057692 -0.009615385  0.011057692
# [4,]  0.0625 -0.001442308 -0.009615385 -0.001442308
# > cacheSolve(nm)
# return cached inverse.
#         [,1]         [,2]         [,3]         [,4]
# [1,]  0.0625 -0.341826923 -0.028846154  0.158173077
# [2,] -0.0625  0.307211538  0.048076923 -0.142788462
# [3,] -0.0625  0.011057692 -0.009615385  0.011057692
# [4,]  0.0625 -0.001442308 -0.009615385 -0.001442308
# > im = cacheSolve(nm)
# return cached inverse.
# > om = makeCacheMatrix(im)
# > cacheSolve(om)
#      [,1] [,2] [,3] [,4]
# [1,]    9   10    3   20
# [2,]   25   30   35   40
# [3,]   45   50    3    8
# [4,]   65   70   75   80
# > cacheSolve(om)
# return cached inverse.
#      [,1] [,2] [,3] [,4]
# [1,]    9   10    3   20
# [2,]   25   30   35   40
# [3,]   45   50    3    8
# [4,]   65   70   75   80