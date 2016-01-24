## Put comments here that give an overall description of what your
## functions do

## Providing the caching Inverse of a Matrix, the Matrix inversion in most of the times has a cost computation 
## and there may be some gain to caching the inverse of a matrix rather than compute it repeatedly.
## Then there are a couple of functions that can be used to create a special object stores a matrix and stores its inverse cached.


makeCacheMatrix <- function(x = matrix()) 
{ mvinv <- NULL
  mvset <- function(mvy) 
  { x <<- mvy
  mvinv <<- NULL }
  
  mvget <- function() x
  mvsetmvinv <- function(mvivrs) mvinv <<- mvivrs
  mvgetmvivrs <- function() mvinv
  list(mvset = mvset, mvget = mvget, mvsetmvinv = mvsetmvinv, mvgetmvivrs = mvgetmvivrs)}


## The following function calculates the inverse of the "matrix" created by
## Previous MakeCacheMatrix . If the reverse has already been calculated ( and
## Matrix has not changed ) , then you should recover the reverse cache .


cacheSolve <- function(x, ...) 
{ ## Return a matrix that is mvivrs of 'x'
  mvinv <- x$mvgetmvivrs()
  if (!is.null(mvinv)) 
  { message("mvgetting cached data")
  return(mvinv) }
  mat <- x$mvget()
  mvinv <- solve(mat, ...)
  x$mvsetmvinv(mvinv)
  mvinv}