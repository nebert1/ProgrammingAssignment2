## These R functions take an input of a square matrix and caches its data and its inverse. 
## The purpose of these functions is to save time and processing power. 

## the 'makeCacheMatrix' function takes a square matrix as an input and creates a special
## "matrix" (class = list) that can cache the inverse of the input matrix. 
## This function sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, and gets the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        s <- function(y){
                x <<- y
                inv <<- NULL
        }
        g <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(s = s, 
             g = g, 
             setInv = setInv, 
             getInv = getInv)
}


## The 'cacheSolve' function calculates the inverse of the special "matrix" from the 
## 'makeCacheMatrix' function. This function checks to see if the inverse has already
## been calculated. If the inverse has already been caclulated it will retrieve the 
##inverse from the cache. If it has not been calculated it uses the solve() function
## to get the inverse then caches the result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Retrieving cached data")
                return(inv)
        }
        m <-x$g()
        inv <- solve(a = m,...)
        x$setInv(inv)
        inv
}

## to test functions:
## a <- matrix(data = c(1, 3, 2, 1, 2, 1, 1, 1, 2), nrow = 3, ncol = 3)
## inverse = 
## [,1] [,2] [,3]
## [1,] -1.5  0.5  0.5
## [2,]  2.0  0.0 -1.0
## [3,]  0.5 -0.5  0.5
