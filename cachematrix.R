## Coursera
## R Programming Course
## Week 3
## Second Programming Assignment
## Started on July 7
## Developed by eta528
##
## Assignment: Caching the Inverse of a Matrix
##
## two functions to create -> makeCacheMatrix & cacheSolve
##
## The following comments describe the functions needed.
##
## makeCacheMatrix -> This function will:
##    (1) Create a matrix variable;
##    (2) Store the results of matrix inverse calculation;
##    (3) The stored results will be cached for easy future access.
##
## cacheSolve -> This function will:
##    (1) Compute the inverse calculation of a 'special matrix' from the 
##          makeCacheMatrix function above;
##    (2) If the inverse is already calculated, the function will instead
##          source data from the cache, avoiding duplicate computation.
##
## Assumptions: 
##    (1) Matrix is ALWAYS invertible
## 
## Functionality notes:
##    (1) '<-' will refer to a variable in the current environment
##    (2) '<<-' will refer to a variable in the parent environment
##

## As mentioned above, this funciton will create a matrix variable

## x <- matrix passed to the function
makeCacheMatrix <- function(x = matrix())
{
      ## define a matrix to store the inverted matrix and set to null
      iMatrix <- NULL
      ## define a series of inner functions
      ## the first function sMatrix should test 
      sMatrix <- function(y)
      {
            x <<- y           ## saves data at parent level
            iMatrix <<- NULL  ## saves data at parent level
      }
      ## the second function gMatrix will return the matrix x
      gMatrix <- function() x
      ## the third function siMatrix will set inverted matrix value in the
      ## parent environment
      siMatrix <- function(inverse) iMatrix <<- inverse
      ## the fourth function gMatrix will return the inverted version of x
      giMatrix <- function() iMatrix
      
      ## the next step will allow for easier referencing with the $ operator
      list(sMatrix = sMatrix, gMatrix = gMatrix, siMatrix = siMatrix,
           giMatrix = giMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
      ## Return a matrix that is the inverse of 'x'
      iMatrix <- x$giMatrix()
      if(!is.null(iMatrix))
      {
            message("getting cached data")
            return(iMatrix)
      }
      data <- x$gMatrix()
      iMatrix <- solve(data, ...)
      x$siMatrix(iMatrix)
      iMatrix
}
