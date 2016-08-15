## Function makeCacheMatrix creates a special object that provides
## cached inverse calculation of matrices.
##
## Function cacheSolve computes the inverse of matrices utilizing the
## cashing capabilities of the makeCacheMatrix function.
## 

###################################################################
###   makeCacheMatrix
###################################################################
## This function saves the matrix passed to it, 
## sets the cached inverse matrix to NULL,
## and returns the myMatrix object containing 4 functions
## that will be used by the cacheSolve function.


makeCacheMatrix <- function(m = matrix()) {
  
  inv <- NULL   # Initial matrix inversion is NULL
  mat <- m      # Saved matrix
  
  # 
  setNewMatrix <- function(m) {
    mat <<- m           # save the new matrix
    inv <<- NULL        # reset inv to NULL
  }
  
  # get: returns x (i.e. data) that were saved in calling environment
  sameMatrix <- function(m) {
    identical(m, mat) 
  }
  
  # Retrieve the cached inverse
  saveInv <- function(val) {
    inv <<- val
  }
  
  # Retrieve the cached inverse
  getInv <- function() {
    inv 
  }
  
  # Returns a list of 4 functions
  myMatrix <<- list(
    setNewMatrix = setNewMatrix, 
    sameMatrix = sameMatrix,
    saveInv = saveInv,
    getInv = getInv)
}

###################################################################
###   cacheSolve
###################################################################
## This function utilizes the functions defined in makeCacheMatrix 
## function to calculate the inverse matrix only once and than return 
## saved inverse matrix.
## Before using this function, the makeCacheMatrix() function 
## must be called in order to create the myMatrix object.

cacheSolve <- function(m = matrix()) {
  
  # Check if this matrix was already saved
  if (!myMatrix$sameMatrix(m)) {
    myMatrix$setNewMatrix(m)
  }
  
  # Get saved inversion (if any)
  inv <- myMatrix$getInv()
  if (is.null(inv)) {
    # calculate new inversion and cache it
    print("Calculating the inversion")
    inv <- solve(m)
    myMatrix$saveInv(inv)
  }
  else {
    print("Returning saved inversion")
  }
  
  inv # return the inversion
}
