## Put comments here that give an overall description of what your
## functions do

# set <- function(y= matrix())
#     Declare a function that takes as argument the matrix y,
#     resets the matrix x to the new value y and Ix, the inverse of x, to a null 
#     and pass both of them to the parent environment.
#     This function can be used to test if the inverse of 
#     the inverse matrix is the same as the original matrix.

# get <- function() x
#     Declare a function that returns x. In our case x is a matrix.


# setInverse <- function(solve)
#     Declare function setInverse that takes as argument a matrix (in our case) 
#     and assigns Ix to the parent environment.
#     The value of Ix is equal to solve. This function is used to store the 
#     value of the inverse matrix of x in cache.

# getInverse <- function() Ix
#     Declare void function getInverse such that it gets the inverse of x from cache.


## Write a short comment describing this function

# This function returns a list of functions. The names of the returned functions
# are set, get, setInverse and getInverse and their purpose is to 
# reset the matrix x, get the value of matrix x, store the inverse of x 
# in cache and get the inverse of x from cache, respectively.
makeCacheMatrix <- function(x = matrix()) {
     # Initialize the inverse of x as a NULL
     Ix <- NULL 
     
     set <- function(y) {
          x <<- y 
          Ix <<- NULL
     }
     
     get <- function() x

     setInverse <- function(solve) Ix <<- solve
     
     getInverse <- function() Ix
     
     # Return the functions declared above as a list.
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}

## Write a short comment describing this function

# cacheSolve <- function(x, RESET=FALSE, ...)
#    This function actually returns the inverse of the matrix. Its arguments
#    are the matrix x, and one or more optional logical variables.
#    If RESET = TRUE, then it sets the matrix to be the inverse of the original
#    matrix, so that when cacheSolve is used again it returns the original matrix.
#    Additional options inherited from the solve function can be passed to the 
#    cacheSolve function, such as tolerance and so on. We don't check if the matrix is
#    square or trivial. We assume it is square and not trivial.

cacheSolve <- function(x, RESET=FALSE, ...) {
     ## Return a matrix that is the inverse of 'x'.

     # Check if inverse of x is stored in cache and get the value
     Ix <- x$getInverse()
     
     # If the inverse exists (i.e. not null) then return the value
     # and print information message.
     if(!is.null(Ix)) {
          message("***Info: Getting cached data.")
          data <- x$get() 
          if (RESET==TRUE){
               # Set x to the inverse in case we want to check if by executing 
               # executing the function again we end up with the original matrix.
               message("***Info: Resetting x to inverse of x.")
               x$set(Ix)
          }
          return(Ix)
     }

     # Otherwise compute it from solve!
     
     message("***Info: Computing inverse matrix.")

     # First get the value of the matrix x.
     m <- x$get()
     
     # Set an identity matrix of the same shape in order to use it in 
     # R's solve function to compute the inverse of x. This step is 
     # needed in case additional options are required in the 
     # R's solve function. We don't check if the matrix is
     # diagonal or trivial. We assume it's not.
     identityMatrix <- diag(1.0,nrow(m),nrow(m))
          
     # Set data as x
     data <- x$get()
          
     # Compute inverse by solving the a*y=b equation,
     # where a=x, b=identityMatrix, and y=Ix
     Ix <- solve(data, identityMatrix, ...)
          
     # Set the Inverse of x
     x$setInverse(Ix)

#     Uncomment the code below for debugging purposes only. The code below checks if 
#     the inverse of the inverse of the matrix is identical to the original matrix.
#     If yes, it prints a successful message, if not it prints an error and returns
#     the difference between the two matrices.

#     DEBUG = TRUE
#     if (DEBUG){
#     # Report differences between the two matrices
#          IIx <- solve(Ix)
#          diff <- IIx-data
#          if (identical(IIx,data)){
#               message("***Info: Test 1/1 -> success")
#          }
#          else {
#               message("***Error: There was an error in the calculation. Test failed!")
#               return(diff)
#          }
#     }

     if (RESET==TRUE){
          # Set x to the inverse in case we want to check if by executing 
          # executing the function again we end up with the original matrix.
          message("***Info: Resetting x to inverse of x.")
          x$set(Ix)
     }
     # Return the inverse of x
     Ix

}
