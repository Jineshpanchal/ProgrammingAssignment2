## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
    set <- function(y=matrix()) {    #set the value of the matrix
        x <<- y            ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
        
        m <<- NULL      # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    y <- x$get()      # run the getmatrix function to get the value of the input matrix
    x$set(y)          # run the setmatrix function on the input matrix to cache it
    inverse <- x$get()
    m <- solve(y, ...)  # compute the value of the inverse of the input matrix
    x$setinverse(m)     # run the setinverse function on the inverse to cache the inverse
    m                   # return the inverse
}
## Sample run:


##Store matrix in d 
##  d<-makeCacheMatrix(matrix(1:4,2,2))
##  No cache while running for the first time.
##  1st time computing
##> cacheSolve(d)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##Computing again

##2nd Time
##cacheSolve(d)
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
