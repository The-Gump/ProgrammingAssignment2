## Put comments here that give an overall description of what your
## functions do

## Cache an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   ##create Null matrix to cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_mat <- function(inverse) m <<- inverse  ##cache matrix m
    get_inv <- function() m   ##call matrix m
    list(set = set, get = get,
         set_mat = set_mat,
         get_inv = get_inv)
    }



## check to see if inverse  of matrix has been calculated 
## (whether or not m is Null)
## If so return inverse matrix, if not calculate inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {   ##checks to see if inverse (m) has been calculated
        message("getting cached data.")
        return(m)  ##returns m if above is TRUE
    }
    data <- x$get()   ##calculates inverse of matrix given 
    m <- solve(data)
    x$set_mat(m)
    m
}


