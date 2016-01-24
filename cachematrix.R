
## goal is to play with environment variables to cache data inside functions for efficient re-use
## we are creating two functions, one (makeCacheMatrix) is used as a data store for setting and getting a matrix and 
## setting and getting its inverse. The other function (cacheSolve) actually does the work, it attempts to get the 
## inverse from the datastore function (makeCacheMatrix) and failing that, it gets the actual matrix and inverts it, 
## and stores it back into the data store. The next time its called, it runs through the same routine above. This time
## it is able to find the inverse of the matrix stored in the data store matrix.
## example run:
## > source("cacheMatrix.R")
## > m <- matrix(c(2,3,2,4), nrow = 2, ncol = 2)
## > theDataStore <- makeCacheMatrix(m)
## > cacheSolve(theDataStore)
## > ## the above run did not hit the cache
## >  
## > ## let's run it again..
## > cacheSolve(theDataStore)
## getting cached data
##     		[,1] [,2]
## 		[1,]  2.0   -1
## 		[2,] -1.5    1
## > 
## > # the 2nd run did hit the cached data, and returned it
## > 
## Omer Ansari (knail1) - 1.24.2016



## makeCacheMatrix (is a data store matrix) creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
		inverseOfTheMatrix <- NULL
		Set <- function(theMatrix) {
				  x <<- theMatrix
				  inverseOfTheMatrix <<- NULL
		}
		Get <- function() x
		SetInverse <- function(inversed) inverseOfTheMatrix <<- inversed
		GetInverse <- function() inverseOfTheMatrix
		list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. . 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theInverse <- x$GetInverse()
        if(!is.null(theInverse)) {
        		message("getting cached data")
        		return(theInverse)
        }
        theMatrix <- x$Get()
        theInverse <- solve(theMatrix)
        x$SetInverse(theInverse)
}