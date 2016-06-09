## Matrix inversion is a costly computation; these functions allow a user to
## cache the inverse of a matrix to avoid computing it repeatedly

## makeCacheMatrix creates a special list object which contains a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix object created with the
## above function. It first checks to see if the inverse has been calculated. If
## returns the inverse from the cache and skips computation. Otherwise it 
## calculates the inverse of the matrix and sets the value of the inverse
## in the cache using the setinverse function. The matrix must be invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)){
        message("retrieving inverse from cache...")
        return(minv)
     }
    matrix <- x$get()
    minv <- solve(matrix)
    x$setinverse(minv)
    minv
}
