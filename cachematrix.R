## This Script has two functions cacheSolve and makeCacheMatrix
## makeCacheMatrix function returns a list object containing list of functions
## set , get, setSolve and getSolve. 
## set is used for caching the initial matix. 
## get is used to return the initial matrix
## setsolve is used for caching the inverted matrix
## getSolve is used for returning the cached value of inverted matrix.

## function to get a list of functions used for caching and returning matrix 
## and it's inverse. 
makeCacheMatrix <- function(x = matrix()) {
    ## Arguements : takes a matrix which is assumed to be always invertible. 
    
    ##initialize the inverse to NULL
    cs <- NULL
    
    
    # set is used to cache the initial data and initialize the inverse to NULL, If
    ##the matix has not changed call to set will be ignored to preserve the cached
    ##values of computed inverse
    set <- function(y){
        if(!identical(y,x)){
            x <<- y
            cs <<- NULL
        }
        
        
    }
    
    ## function to return the initial matrix  
    get <- function() x
    
    ## function to cache the  computed inverse of the matrix
    setSolve <- function(sol) cs <<-sol
    
    getSolve <- function() cs
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}



## cacheSolve is function used for computing the inverse of a matrix. it uses
## the cached value of the inverse if its already computed else computes a new 
## inverse and then caches it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cs <- x$getSolve()
    if(!is.null(cs)){
        ## Return the cached value of inverse of the matrix if its already computed     
        message("getting cached data")
        
        return(cs)
    }
    
    ## Compute the inverse of the matrix and cache it for subsequent uses. 
    cs <- solve(x$get())
    
    x$setSolve(cs)
    
    # return the invesre of the Matrix
    cs
    
    
    
    
}