## The purpose of the makeCacheMatrix function is to create a cached matrix of values which can be 
## overwritten or accessed by another function which calculates the inverse of a matrix.
## Inverting a matrix works for certain square matrices.

## After creating the null valued cache, makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix to null inititally
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## Sequence:
## Set the answer to nullin the current space
## Set the cached answers to null using the "superassignment" variable.  
## Create an anonymous function for "get"
## Set new variables for getsolveit and setsolveit are vectors of functions for the answer
## Create a list of these variables that can be referenced by another function

makeCacheMatrix <- function(x = matrix()) {
    ans <- NULL
    set <- function(y) {
        x <<- y
        ans <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) ans <<- solved
    getsolved <- function() ans
    list(set=set, get=get, setsolved=setsolved, getsolved=getsolved)
}

## The cacheSolve function tries first to retrieve the matrix inversion answer from the cache.
## If it finds it then it returns the value from cache and prints a message.
## If it does find it as the matrix of answers is null then it solves for the matrix inversion using the solve commmand. 


cacheSolve <- function(x, ...) {
    ans <- x$getsolved()
    if(!is.null(ans)) {
        Notification <- "Answer retrieved from cache"
        print(Notification)
        return(ans)
    }
    data <- x$get()

    ans <-tryCatch(solve(data), 
                   error = function(e) {stop    
                       return(paste( "Non-Invertible Matrix:", conditionMessage(e)));
                   }, 
                   finally={
                       print("All systems go")
                  ##     ans <- solve(data)                                         
                  ##           return(ans)
                  ##           x$setsolved(ans)
                  ##           Notification2 <-"Calculating from scratch"
                  ##           print(Notification2)
                            ans
                   });
    
    ## ans <- solve(data) 
    
  
    
    return(ans)
    x$setsolved(ans)
    Notification2 <-"Calculating from scratch"
    print(Notification2)
    ans
}

Testing sequence:
    
# 1 Create your matrix of data to be converted x = matrix(1:4,2,2)
# 2 Run makeCacheMatrix to load the formula
# 3 Run cacheSolve to load the formula
# 2 Run the makeCacheMatrix to z=makeCacheMatrix(x) to load the cache and create z as parameters for cacheSolve
# 3 Run cacheSolve(z) - this time it should calcuate the matrix inversion from scratch
# 4 Run cacheSolve(z) agin - this time it should load the pre-calculated matrix from the cache
# Other square matrices: (6:9,2,2), (9:12,2,2).  What will not work: x = rbind(c(2,6),c(1,3)) 
