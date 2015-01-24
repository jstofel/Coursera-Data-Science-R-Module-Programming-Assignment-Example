############################################################################
## SCRIPT: cachematrix.R
## PURPOSE: two scripts that use anonymous functions (closures) to allow the user to 
##          perform a matrix inversion, then cache the results for future use
## AUTHOR: J Stofel (with lots of input from the discussion forums!)
## DATE:   19 Jan 2015
## CLIENT: Courser R Course, Programming Assignment 2
##
##  Example invertible 3 x 3 matrix: 
##       B<-matrix(c(1,2,3,2,5,2,6,-3,1), nrow=3, ncol=3)
##  > B
##  [,1] [,2] [,3]
##  [1,]    1    2    6
##  [2,]    2    5   -3
##  [3,]    3    2    1
##
##   Example usage:
##   passthru <- makeCacheMatrix(B)
##               cacheSolve(passthru)
##
##   results on first pass: 
##
##  > cacheSolve(passthru)
##  [,1]        [,2]        [,3]
##  [1,] -0.1428571 -0.12987013  0.46753247
##  [2,]  0.1428571  0.22077922 -0.19480519
##  [3,]  0.1428571 -0.05194805 -0.01298701
##
##   results on second pass:
##  > cacheSolve(passthru)
##  getting cached data
##  [,1]        [,2]        [,3]
##  [1,] -0.1428571 -0.12987013  0.46753247
##  [2,]  0.1428571  0.22077922 -0.19480519
##  [3,]  0.1428571 -0.05194805 -0.01298701
##
##  Notes on how to use the anonymous functions
##
##  > vec <- makeVector(rnorm(4))
##  > vec$setmean(mean(vec$get()))
##  > vec$get()
##  [1] -0.0593134  1.1000254  0.7631757 -0.1645236
##  > vec$getmean()
##  [1] 0.409841
##  > vec$set(rnorm(6))
##  > vec$get()
##  [1] -0.2533617  0.6969634  0.5566632 -0.6887557 -0.7074952  0.3645820
##  > vec$getmean()
##  NULL
##  >
##
## NOTES: 
##        1. the ... in the arguments allows different options
##                   to be sent to the interior function, in this
##                   case "solve" (get the inverse matrix).  It has 
##                   been pointed out that this can allow undesireble
##                   results - different output is possible for the same
##                   input. 
##         2. But, you want to make sure that the changed circumstances
##                  are recognized by the code, and the correct solution,
##                  not necessarily the cached solution, is returned.
##                  Jorg Sauer solved the issue by caching the arguments 
##                  as a list in cacheSolve(), and then testing for identity
##                      if(!is.null(i) && identical(list(...), a)) {  
##                          x$setarg(list(...))\
##                          #stuff
##                       }
##                  As he pointed out, this is optional in terms of the 
##                  requirements, which are for matrix inversion only, 
##                  and in that case, no additional arguments to solve() 
##                  are used.  
##
## TESTING
##        These functions were tested with manual unit testing of several
##        different matrix inputs.
##        For addtional information on unit testing in R in general,
##             see "testthat" library (http://r-pkgs.had.co.nz/tests.html) 
#              as a means of checking that implementation behaves as expected.
##
## REVISION HISTORY
##      24 Jan 2015  J Stofel removed the ... optional argument in the 
##                    cacheSolve function, to ensure that only matrix 
##                    inverse is returned and no optional parameters need
##                    to be dealt with.
##
############################################################################

## makeCacheMatrix
##   Input:   an inversible matrix
##   Output:  a list object containing 4 anonymous functions
##
##      Function 1: get.          Returns the value (matrix) that was input. 
##                                This allows "passthru" of the input to the 
##                                function that reads the list object 
##                                (the cacheSolve function)
##      Function 2: setinverse.   Populates the list object with the result:
##                                the inverse of the input matrix
##
##      Function 3: getinverse.   Returns the value of the inverse matrix
##                                
##      Function 0:  set.         This function allows the input matrix to be
##                                changed / re-set
#


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL         #initializes(clears) value of inverse matrix in current env
  
  #The set function is used as the 'correct' way to change the input function
  #  and ensure that the cache is cleared, so that a new inverse is calculated
  #  for the new matrix.
  
  set <- function(y){
    x <<- y        #resets value of x in parent env to value of y in current env
    i <<- NULL     #initializes(clears) value of inverse matrix in parent env
  }
  
  #returns the value of x (a matrix)
  get <- function() x                       
 
  #calculates inverse, and assigns value to var i in parent env
  setinverse <- function(solve) i <<- solve   
                                             
  #returns the value of i (the inverse)
  getinverse <- function() i                 
  
  #Return the result: a list of anonymous functions that includes:
  #                             function to change (re-set) the initial matrix
  #                             function to retrieve initial matrix, 
  #                             function to create the inverse matrix
  #                             function to retrieve the inverse matrix
    
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve
##   Input:   the list object output from makeCacheMatrix
##   Output:  a matrix (the inverse of the matrix supplied to makeCacheMatrix)
## 
##    This function tests to see if the input list contains the desired data 
##    by running the getinverse function from makeCacheInverse. If it exists,
##    then it is returned.  If it does not exist, then it is created by 
##    running the solve funcion from Base R.  The result is then 
##       a) added to the list object by running setinverse from makeCacheInverse
##       b) return

cacheSolve <- function(x) {
  i <- x$getinverse()                  #retrieve the inverse matrix from cache
  if (!is.null(i)) {                   #check to see if it exists
    message("getting cached data")     #if so, let us know!
    return(i)                          #return value
  }
  data <- x$get()                      #retrieve the input matrix
 
  i<-solve(data,...)                   #solve for the inverse
  x$setinverse(i)                      #add to list cache
  i                                    #return value
}
