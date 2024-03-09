## For this assignment I am tasked with creating functions to cache a matrix and
## its inverse. the first function creates and stores a square matrix and caches
## its inverse. the second function calculates the inverse and attempts to store 
## but first checks to see if this has already been done. 

#The code in question
## This function sets and retrieves the matrix and the inverse matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {            #initializes inverse as NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse #sets and retrieves the inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the above special function but first 
## it checks to see if this action has already been done. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {             #if there is already an"inv" in the cache it retrieves it
                message("getting cached data") #fun little message to confirm its already cached 
                return(inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)     #otherwise it creates the inverse from scratch
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

#Testing my code
my_Matrix <- makeCacheMatrix(matrix(6:9, 2, 2))
my_Matrix$get()
cacheSolve(my_Matrix)
