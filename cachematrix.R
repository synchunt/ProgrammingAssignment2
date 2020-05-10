## The first function is used mainly to get the matrix, which in reality
## is used to set and get the value of matrix and
## to then set and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        setMat <- function(o){
                x <<- o
                inver <<- NULL
        }
        get <- function(){x}
        setInv <- function(inverse) {inver <<- solve(x)}
        getInv <- function() {inver}
        list(set = setMat,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## The second function is basically used to Solve the inverse of the
## matrix but as we have addressed that computing the inverse of the 
## matrix is heavy on resources thus this function first checks if the
## inverse has already been calculated or not, if yes then it returns
## the cached value else it solves the matrix and returns new inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInv()
        if(!is.null(inver)){
                message("getting cached matrix")
                return(inver)
        }
        mat <- x$get
        inver <- solve(mat, ...)
        x$setInv(inver)
        inver
}
