#
#The first function caches the inverse of the matrix.
#"SOlve" command is used to do the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(solve) m<<-solve
    getinverse<-function()m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


#The second function calculates the inverse. It checks if the inverse is already calculated.


cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
