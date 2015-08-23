# makeCacheMatrix() is used to set up a matrix that we want to invert
# cacheSolve() is used to provied the inverse of the matrix we set up in makeCacheMatrix()

## makeCacheMatrix() is a function that takes an invertible matrix. This function contains other 
# functions [get(), set(), setin(), getinv()] that can be used to store the inverse of the argument matrix
# into a cache

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y=matrix()){
        
        x<<- y
        i<<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) i<<-inv
    
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)

    
}


## cacheSolve() takes the function makeCacheMatrix() as its argument and uses this matrix to determine 
# if the inverse of a matrix is already in the cache or it needs to be calculated

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    i<- x$getinv()
    
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }else{
        
        matrx<-x$get()
        i<-solve(matrx,...)
        x$setinv(i)
        i
    }
    
}


