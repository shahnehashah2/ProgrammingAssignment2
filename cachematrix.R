## Helpful guide: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## makeCacheMatrix: Sets and retrieves the cached inverse matrix; if present
## cacheSolve: Display the inverse of a matrix. Use makeCacheMatrix to 
## set the inverse in cache and retrieve from cache

## Setting the setters and getters in this function. 'inv' stores the inverse 
## of the matrix 'x'. A list of the function names and the values of x and inv 
## are returned.

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    
    # x and inv variables in parent function cacheSolve are set
    set <- function(y) {    
        x <<- y
        inv <<- matrix()
    }
    
    # Inverse calculated in cacheSolve() is set to inv 
    setinv <- function(inverse) inv <<- inverse
    
    get <- function() x
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'. If a non-square matrix is 
## provided, throw an error.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    data <- x$get()
    
    # Display an error message and stop execution if matrix is not square
    if (nrow(data) != ncol(data)) {
        stop("Not a square matrix")
    }
    
    # Check whether the values of inverse matrix are NA. 'all' checks for NA 
    # in each (row,col) value
    if(all(!is.na(inv))) {
        message("getting cached data")
        return(inv)
    }
    
    # Setting tolerance really low to allow inverse computation of small numbers
    inv <- solve(data, tol = 1e-20)
    x$setinv(inv)
    inv
}
