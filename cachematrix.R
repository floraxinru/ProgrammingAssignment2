
## Overall description of what your functions do:
#a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL #sets inverse matrix to NULL
    set <- function(y) #set the value of the matrix; the "setter" or mutator function
    x <<- y #assigns the input argument y to the x object in the parent environment
    invM <<- NULL #Assign the value of NULL to the invM object in the parent environment. 
    #This line of code clears any value of invM that had been cached by a prior execution of cacheSolve().
    }

    get <- function() x #gets the value of the matrix x; the "getter" or accessor function
    setinv <- function(solve) invM <<- invM
    #setmean <- function(mean) m <<- mean
    getinv <- function() invM
    
    list(set = set, ## gives the name 'set' to the set() function defined above
         get = get, ## gives the name 'get' to the get() function defined above
         setinv = setinv, ## gives the name 'setinv' to the setinv() function defined above
         getinv = getinv) ## gives the name 'getinv' to the getinv() function defined above
    


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#For this assignment we assume the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getinv() #calls the getmean() function on the input object, x
    #Then it checks to see whether the result is NULL
    if(!is.null(invM)) {
        message("getting cached data") #if result is not null, we have a valid cached mean that we can return to the parent environment
        return(invM)
    }
    
    #If the result of !is.null(invM) is FALSE:
    data <- x$get() #cacheSolve() gets the vector from the input object (x),  
    m <- solve(data, ...) #calculates inverse of the matrix; if X is a square invertible matrix, then solve(X) returns its inverse.
    
    x$setinv(invM) #uses the setinv() function on the input object to set the inverse in the input object
    invM #then returns the value of the inverse to the parent environment 
}
