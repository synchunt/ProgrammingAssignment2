## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        setMat <- function(o){
                x <<- o
                inver <<- NULL
        }
        get <- function(){x}
        setInv <- function(inverse) {inver <<- inverse}
        getInv <- function() {inver}
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInv()
        if(is.null(inver)){
                message("getting cached matrix")
                return(inver)
        }
        mat <- x$get
        inver <- solve(mat, ...)
        x$setInv(inver)
        inver
}
