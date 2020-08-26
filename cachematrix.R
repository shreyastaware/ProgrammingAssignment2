## Put comments here that give an overall description of what your
## functions do

## so when the makeCacheMatrix function receives a matrix which caches its inverse
## and then the cacheSolve function computes the inverse. 
## cacheSolve will retrieve the inverse from the makeCacheMatrix if it already exists 
## otherwise it will compute it

## Write a short comment describing this function

## So the makeCacheMatrix function
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the inverse
## 4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## So the cacheSolve function takes the the matrix from the previous function, 
## If the inverse exists, then it will print the msg "getting cached data" and retrieves inverse from that
## If it does not, then it will directly print the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
