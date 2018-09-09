
# Following functions creates a special" matrix" (which is a list with a function to:
## 1. set the value of a matrix, 2. get  avalue of the matrix, 3. set the value of solved 
##system of equations, 4. get the value of solved system of equations) and afer creating they
##calculate the inversion of the matrix. If this matrix inversion have been already calculated,
## functions skips the calculation and present already cached result, if no - it calculates it
## and then saves in the cache.


## MakeCacheMatrix function creates a special "matrix" which is a list with a function to:
## 1. set the value of a matrix, 2. get  avalue of the matrix, 3. set the value of solved 
##system of equations, 4. get the value of solved system of equations

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() X
        setsolve <- function(solve) m <<- solve
        getsolve <- function () m
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


#This function calculates the inversion of the "matrix" created with the above function. 
#Firstly, it checks if the matrix have been already calculated - if so, it skips the computation
## and presents already saved result from the cache.
# If it have not been calculated yet, it calculates the inversion and sets its value in the cache
#via the setman function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}
