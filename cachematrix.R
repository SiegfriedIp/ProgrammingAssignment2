## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix has a few variable and methods (C++ language):
##  1) storeVar: A storage unit for storing calculation result
##	2) orgMat: The original matrix for calculation
##	3) get: A method to return the value of orgMat
##	4) setOrgMatrix: A method to change the orgMat to a new one
##	5) setResult : make the storage unit to store a value (calculation result)
##	6) getResult: a method to return the result
##

makeCacheMatrix <- function(mat = matrix()) {
  
  ## Initialize the storage unit/stored variable first
  storedVar <- NULL
  
  ## This function will update the original matrix for calc
  ## As well as clear the storage unit
  setOrgMatrix <- function(newMat) {
    mat <<- newMat
    storedVar <<- NULL
  }
  
  ## This function will return the original matrix
  get <- function() mat
  
  ## This function will set the storage unit
  ## It should be call in the cache function
  setResult <- function(resultMat) storedVar <<- resultMat
  
  ## This function will return the value of the storage unit
  getResult <- function() storedVar
  
  ## The list is unnessary, but we do it anyway because it is in the original example
  list(setOrgMatrix = setOrgMatrix, get = get, setResult = setResult, getResult = getResult)
}


## 
## Thsi function calculates the inverse of the matrix and store it at cache
## If the calculation has performed previously, then it will just load from cache
##

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cacheMat <- mat$getResult()
  
  ## First check if the result already stored in cache, if yes, the don't calculate anything
  if (!is.null(cacheMat)) {
    message ("getting result from cache")
  } 
  else {
    ## If it is not stored in cache, then calculate on the spot
    tempMat <- mat$get()
    cacheMat <- solve(tempMat)
    
    ## Now store it the cache
    mat$setResult(cacheMat)
  }
  
  ## Now return the result
  return(cacheMat)
}
