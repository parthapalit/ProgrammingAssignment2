#################################################
#
# Partha Palit: 01/28/2017
#
#################################################

# Main purpose of makeCacheMatrix is to set the variables in parent env
# As Alan E. Berger has explained in the discussion board - makeCacheMatrix returns 'inversemat' and 'x' along with
#         functions - set, get, setinverse and getinverse
# This does not actually calulate the inverse
# tested using: https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

makeCacheMatrix <- function(x = matrix()) {
        inversemat <<- NULL
        
        set <- function(y) {
                x <<- y
                inversemat <<- NULL
        }
        
        get <- function() x
        
        
        # set function to stick the inverted matrix (as calculated in calling function) in parent environment
        setinverse <- function(inverse) inversemat <<- inverse
        
        # get function to get the inverse matrix (anonymous)
        # getmean is a numeric vector that retrieves inverse (from the parent environment)
        getinverse <- function() inversemat
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


###########################################################
#
###########################################################
cacheSolve <- function(x, ...) {
        # the whole function is retrieved from the global environment
        # since getmean() is returned as a list from makeVector it is referenced as a $ variable
        inversemat <- x$getinverse()
        
        # if it has content then return the content - this is currently the mean of the numeric vector
        # inversemat is returned from x <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        
        # using solve 
        localdata <- x$get()
        
        # this creates the inverse of the matrix and sets it to the global variable
        inversemat <- solve(localdata)
        
        # now calling makeCacheMatrix.setmean function to set the value of m (the global variable)
        x$setinverse(inversemat)
        
        # return the global variable m
        inversemat
}
