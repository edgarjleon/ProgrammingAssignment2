##
## These functions implement matrix inverse hanlding using cached values if appropriate.
##

##
## makeCacheMatrix()
##
## This function takes the length of the matrix as inout and creates the matrix 
## instead of accepting the matrix as parameter, this way it makes sense to have a 
## set() function and it simplifies life for whole humanity as human kind will never
## have to create a matrix of random numbers.... you are welcome world. the new 
## created matrix is part of the list this function creates.
##

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
				# set will create the matrix with random numbers
                ma <<- matrix(runif(y*y,1,y),y,y)
                m <<- NULL
				ma
        }
        get <- function() ma$m
        setSolve <- function(solveData) m <<- solveData
        getSolve <- function() m
        list(ma=set(x),set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

##
## cacheSolve()
##
## this function returns the inverse of a matrix created using makeCacheMatrix
## if such inverse aready exists (cached) the the cached value is returned
## otherwise the inverse is calculated (via solve()), the new inverse matrix
## is cached (via serSolve()) and it is returned.
##

cacheSolve <- function(x) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        d <- x$get()
        m <- solve(d)
        x$setSolve(m)
        m
}
