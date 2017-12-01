## This programming assignment requires me to write an R function that can cache potentially time-consuming computations
## Matrix inversion is one of the time-consuming computations, and caching the inverse of a matrix may be
## beneficial than computing it repeatedly

## The first function is makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        valueholder <- NULL                                             #this clears the cache
       
        setmatrixvalue <- function (matrixvalue){                       #set the value of the matrix
                x <<- matrixvalue                                       #x gets the value of the matrix
                valueholder <<- NULL                                    #clears the cache again since the matrix has a new value
        }
        
        getmatrixvalue <- function() x                                  #this gets the value of the stored matrix
        setinverse <- function(inverse) valueholder <<- inverse         #cache gets the value of the inverse
        getinverse <- function () valueholder                           #gets the value from the cache
        
        list(setmatrixvalue = setmatrixvalue,                           #return list
             getmatrixvalue = getmatrixvalue,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## The second function is cacheSolve
## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        valueholder <- x$getinverse()                                   #get inverse
        if (!is.null(valueholder)) {                                    #since the divisor should not be 0
                message("getting cached data")
                return(valueholder)
        }
        
        data <- x$getmatrixvalue()                                      #else, get the matrix value
        inverse <- solve(data, ...)
        x$setinverse(valueholder)
        
        inverse                                                         #return the inverse
}
