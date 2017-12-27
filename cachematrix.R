## Assignment number 2 - Lexical Scoping
## Based on the Caching the Mean of a Vector example. Just changed x to Matrix, m to im, function names (getinv and setinv) and mean() to solve()

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        im <- NULL
        set <- function(y) 
        { 
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) im <<- inverse
        getinv <- function() im
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This functon returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
        x <- im$getinv()
        if (!is.null(im)) 
        {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverse(im)
        im
}