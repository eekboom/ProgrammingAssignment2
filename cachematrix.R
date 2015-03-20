## Offers a feature to cache a matrix's inverse matrix to avoid potentially
## costly recalculations.
##
## This is the Programming Assignment 2 for Coursera's "R Programming" course:
## Goal: Use lexical scoping to create a closure where values can be cached.

## Function comments are in roxygen format, see
## http://kbroman.org/pkg_primer/pages/docs.html
## Code uses indentation of 4 spaces as a compromise between readability and
## space efficiency.

#' Creates a cache that stores a matrix and can cache its inverse matrix
#' @param x initial value of the matrix
#' @export
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    #' Set a new matrix (will clear the cached inverse matrix)
    #' @param y new value
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #' Get the matrix
    get <- function() {
        x
    }

    #' Stores the matrix's inverse matrix for future retrieval
    setInverse <- function(inverse) {
        # Interestingly the following assignment works (though it is not
        # recommended).
        # I found an eplanation in the "R Language Definition",
        # section 3.4.4 "Subset assignment".
        # Right hand side is evaluated in current environment (parameter),
        # left hand side is evaluated in parent environment and up (closure).
        inverse <<- inverse
    }

    #' Returns the cached inverse (or NULL if no inverse has been stored since
    #' the matrix was set)
    getInverse <- function() {
        inverse
    }

    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


#' Takes a cache matrix (as returned from makeCacheMatrix()) and returns its
#' (potentially cached) inverse matrix.
#'
#' @param x a cache matrix object as returned from makeCacheMatrix()
#' @return the inverse
#' @export
#' @examples
#' m <- makeCacheMatrix(matrix(1:4, 2, 2))
#' inverse <- cacheSolve(m)
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()

    if(is.null(inverse)) {
        # not yet cached, calculate and put into cache
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }

    inverse
}
