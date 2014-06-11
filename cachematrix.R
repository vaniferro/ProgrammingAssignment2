## ASSIGNMENT 2 - Calculate the inverse matrix and store in a cache

## Together, these functions calculate the inverse of a matrix and store 
## this matrix in a cache for fast retrival if required later.
## Such a function is beneficial because calculating the inverse of 
## matrices is a computationally laborious task, requiring much CPU
## power. Be sure to use appropriate syntax when inputting and recalling
## matrices.  Below are a few short example commands:
## 
## "inv <- makeCacheMatrix()" will setup a list that stores the original
##                            and inverse matrices
## 
## "inv$set(x)", where x is a matrix, will set the matrix to be inversed.
##               Changing inv$set() will reset the inverted matrix.
## 
## "inv$get()", recalls the original matrix
## 
## "cacheSolve(inv)" will calculate and store the inversed matrix stored
##                   in "inv".  This results in the cached matrix.  There
##                   must be a matrix stored in "inv" via inv$set() in
##                   order for this function to work. If the inverse
##                   matrix has already been stored in "inv", the cached
##                   matrix will be shown.


## makeCacheMatrix() is the function that stores the original and
## inverted matrices which contains 4 functions within a list:
## 
## set() sets the original matrix and resets the inverse matrix stored
## get() returns the original matrix stored by set()
## setinv() sets the inverse matrix, when commanded by cacheSolve() function
## getinv() returns the inverse matrix stored by setinv()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #resets inverse matrix upon initialization
    
    #set() function
    set <- function(y = matrix()) {
        x <<- y # make the x variable (input matrix) available outside the
                # function based on the inputted value of y (lexical scoping)
        m <<- NULL # reset the inverse matrix in the upper environment
    }
    
    #get function
    get <- function() x # recall x
    
    #setinv function
    setinv <- function(inv) m <<- inv #set inv. matrix calculated by cacheSolve()
    
    #getinv function
    getinv <- function() m # return inverse matrix stored in "m" by setinv()
    
    # this list makes the individual functions available via "$" 
    # ex. z <- makeCacheMatrix()
    #     z$get() will return the original matrix
    
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve is the function that actually calculates the
## inverse matrix.  However, this function checks whether
## the calculation has already been made by querying the 
## m variable.  If the m variable already has a value, a
## calculation has already been made.  If the m variable 
## does not contain any value (NULL), the calculation proceeds.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x' (original matrix)
    m <- x$getinv() # attempt to recall inverse matrix from cache
    
    ## if m is NOT null, retrival from cache successful,
    #   give message, return 'm' (inverse matrix) and exit function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if m is null, carry forward calculation...
    data <- x$get() # retrieve original matrix, assign to 'data'
    m <- solve(data, ...) # solve inverse matrix, allow pass through of commands
    x$setinv(m) # assign 'm' to cache in makeCacheMatrix() function
    m # auto-print variable 'm' (inverse matrix)
}