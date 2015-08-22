## These two functions, makeCacheMatrix and cacheSolve, allow the user to cache
## a matrix and its inverse and then calculate the inverse of the matrix.

## A function that contains a list of four functions that allow the user to:
##  set the matrix, get the matrix, set the inverse, and get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y) {   ##Replaces matrix with matrix from user
                x <<- y
                inverse <<- NULL
        }
        get <- function () x
        setinverse <- function(new_inverse) inverse <<- new_inverse
        getinverse <- function() inverse
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)
}


##  A function that takes a matrix and uses the function makeCacheMatrix to
## calculate the matrix's inverse. If an inverse has already been provided,
## it will be tested and identified as correct or incorrect.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        matrix <- x$get()
        identity <- diag(nrow(matrix)) ##Identity matrix of same dimensions
        
        if(!is.null(inverse)) {
                ## Verifies that matrix stored as inverse is the inverse matrix
                if (all(matrix %*% inverse == identity) &&
                        all(inverse %*% matrix == identity)) {
                                message("getting cached inverse matrix")
                                return(inverse)  
                }
                else    print("Inverse cached is incorrect. Correct inverse is:")
        }
        
        inverse <-solve(matrix)
        x$setinverse(inverse)
        inverse
}
