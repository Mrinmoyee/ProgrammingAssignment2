## The first function "makeCacheMatrix() uses a matrix (that is assumed always
## square and invertible and stores it in the cache, and then the second
## cacheSolve() function solves it for it's inverse and stores that value.
## if the inverse has already been computed it simply finds that value in the 
## cached data and reproduces it

## FUNCTION 1: to store matrix in cached data

makeCacheMatrix <- function(x = matrix()) { ##input is a matrix (square, invertible)
        s <- NULL                           ## stored inverted matrix (from cacheSolve()
                                            ## is reset to NULL everytime a new matrix is 
                                            ## stored in makeCachematrix()
        	set <- function(y) {
                x <<- y
                s <<- NULL
        	}

        get <- function() x

       	 setinverse <- function(solve) s <<- solve

        getinverse <- function() s

        list(set = set, get = get,          ## this is the list returned when makeCacheMatrix()
             setinverse = setinverse,       ## is called, but these are only used in cacheSolve()
             getinverse = getinverse)
}

## FUNCTION 2: to get matrix inverse or calculate it


cacheSolve <- function(x, ...) {
        s <- x$getinverse()                    ## function caluculates inverse of x from makeCacheMatrix()
        if(!is.null(s)) {                      ## unless it's already calculated (i.e. not NULL), in which
                message("getting cached data") ## case, it simply fetches it from cache
                return(s)
        }
        data <- x$get()                        ## reaches this if it is not already calculated
        s <- solve(data, ...)
        x$setinverse(s)
        s
}