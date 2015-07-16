## These functions compute the inverse of a matrix, and caches the 
## result (in a function closure). If asked to invert the same matrix
## again at any time, cachesolve will retrieve the previously calculated
## value instead of calculating it again.

## makeCacheMatrix creates a list that allows storage of a 
## matrix and its mean. The storage (or caching) is made possible
## by defining the list as a function (creating a function closure)
## and so taking advantage of lexical scoping.

makeCacheMatrix <- function(X = matrix()) {
        In <- NULL
        set <- function(Y) {
                X <<- Y
                In <<- NULL
        }
        get <- function() X
        setinv <- function(Inverse) In <<- Inverse
        getinv <- function() In
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes the list created by makeCacheMatrix and
## checks whether the inverse of the matrix in question is 
## already stored therein. If yes, it retrieves it, if not, 
## it calculates it (and then stores it in the list).

cacheSolve <- function(X, ...) {
        In <- X$getinv()
## If inverse has been calculated previously,it is stored in X$getinv
        if(!is.null(In)) {
        message("getting cached data")
        return(In)
        }
## If not, cacheSolve retrieves the matrix from X$get and calculates the inverse
        data <- X$get()
        In <- solve(data)
        X$setinv(In)
        In
}
