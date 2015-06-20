## cachematrix.R file have two function. This function will return inverse
## matrix value
##
## 1. Set the matrix variable.
##    The matrix data must be square (1x1, 2x2, 5x5)
##
## Eg. > mydat <- matrix(rnorm(36),6,6)
## 
## 2. create a variable for cache data. Used the makeCacheMatrix function
##
## Eg. > cacheMat <- makeCacheMatrix(mydat)
##
## 3. lastly, run the cacheSolve function and cache data as its parameter
##
## Eg. > cacheSolve(cacheMat)
##    First time result will give you the inverse matrix
##    Second attempt and more will give you the inverse matrix and also it 
##    will return message "getting cache data"

## makeCacheMatrix function
## used to initialize variable and store data. 
## 4 function (set, get, setinverse and get inverse)
## "set" is to store the matrix
## "setinverse" to store matrix inverse
## "get" and "getinverse" will return the value that being store.
## If it return NULL value, theres no value is being set yet 
## into the variable

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function act to inverse the matrix value and do checking
## if there any cache value, it will skip the process and return the
## cache value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## check if 'm' return a matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

