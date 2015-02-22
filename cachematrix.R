## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: this function creates a special object that stores the inverse
## of the given matrix before caching it. Four other functions are defined in this
## function.
## setMatrix(): this function sets (stores in the environment) the matrix
## getMatrix(): this function gets (retrieves from the enviroment) the matrix
## setIMatrix(): this function sets the inverse of the given matrix
## getIMatrix(): this function gets the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  
  ## takes myMatrix as the input and caches it. Sets the inverse to be NULL as a future placeholder
  setMatrix<- function(myMatrix)
  {
    matrixStore <<- myMatrix
    iMatrix <<- NULL
  }
  
  ## retrieves the matrix from the cache. This function is paired with setMatrix()
  getMatrix <- function() matrixStore
  
  ## sets iMatrix to the newly calculated inverse matrix. Hopefully, this will be done only once
  setIMatrix <- function (tmpIMatrix) iMatrix <<- tmpIMatrix
  
  ## gets iMatrix that was previously computed. This function is paired with setIMatrix
  getIMatrix <- function() iMatrix
  
  ## Now return a vector (function pointers, basically) containing the functions created above
  list (setMatrix = setMatrix, getMatrix = getMatrix, setIMatrix = setIMatrix, getIMatrix = getIMatrix)
} ## end of function makeCacheMatrix


## cacheSolve: this is the function where the checking is done to determine whether the inverse of the
## given matrix be computed or whether it already exists in the cache. Functions returned from the
## makeCacheMatrix are used in this function.

cacheSolve <- function(funMakeCacheMatrix, ...) 
{
  ## Try to retrieve the inverse matrix
  iMatrix <- funMakeCacheMatrix$getIMatrix
  
  ## check to see if it is populated with data
  if(!is.null(iMatrix))
  {
    ## so the inverse exists. Simply return iMatrix and we are done
    print("inverse exits, getting it from cache")
    return(iMatrix)
  }
  
  ## oops the inverse does not exist. So compute it using function solve
  tmpMatrix <- funMakeCacheMatrix$getMatrix
  iMatrix <- solve(tmpMatrix)
  
  ## call the set inverse matric function to set the inverse matrix in cache and then return the
  ## inverse
  funMakeCacheMatrix$setIMatrix(iMatrix)
  return(iMatrix)
} ## end of function cacheSolve
