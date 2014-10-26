## -----------------------------------------
## R Programming
## https://class.coursera.org/rprog-008
## Programming assignment 2
## 
## To save resources, use a cached copy of a matrix's
## inverse instead of calculating the inverse each time
##
## makeCacheMatrix(maxtrix)
## -- creates an object with functions to store & retrieve
## -- the cached matrix inverse
##
## cacheSolve (makeCacheMatrix-List, ...)
## -- Uses object created in makeCacheMatrix to calculate or retrieve
## -- the matrix inverse
##
##
## Code is heavily based on the sample provided at:
## https://class.coursera.org/rprog-008/human_grading/view/courses/972581/assessments/3/submissions
##
## makeCacheMatrix (x=matrix)
## Create object that stores a matrix and its inverse, so that the
## inverse can be retrieved from cache instead of being recalculated
## See paired function cacheSolve()
## -----------------------------------------
## Returns an object based on the passed matrix
## with functions to:
## set: Sets the base matrix for the object
## getmatrix: Retrieves the base matrix for the object
## setinv: Caches the inverse of the matrix
## getinv: Returns the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  #message("In makeCacheMatrix")
  minv <- NULL
  
  mname <- deparse(substitute(x))
  
  ## set parameters for the object
  set <- function(newval) {
    x <<- newval
    minv <<- NULL
  }
  
  ## return the underlying matrix
  getmatrix <- function ( ) x
  
  ##set the cached inverse of the underlying matrix
  setinv <- function(inv) minv <<- inv
    
  ## return the inverse
  getinv <- function() minv
  
  originalMatrix <- x
  
  list(set=set, getmatrix=getmatrix, setinv=setinv, getinv=getinv, mname=mname, originalMatrix = originalMatrix)

}

## cacheSolve (x, ...)
## Use list object from  makeCacheMatrix() 
## Checks to see if a cache of the target matrix exists
## in object x; if so, uses the cached value of the inverse
## See paired function makeCacheMatrix()
##
## Parameters:
##    x     List of type created by makeCacheMatrix()
## -----------------------------------------

cacheSolve <- function(x, ...) {
  # check if the matrix has changed
  
  # variable passed to this call
  cachedObject <- deparse(substitute(x))
  
  # if it's changed, then re-run the make object using the 
  # current value of the matrix
  
  
  if(!(identical(x$getmatrix(), eval(parse(text=x$mname))))) {
#       message("matrices identical")
        message(paste(c("WARNING: Matrix", x$mname, "has changed since initiation of object", cachedObject), collapse=" "))
        message(paste(c("Updating object", cachedObject), collapse=" "))
        x$set(eval(parse(text=x$mname)))
#         message(x$originalMatrix)
#         x$originalMatrix <- testmatrix
#         message(x$originalMatrix)
# #         txMatrixName <- x$mname
#         message(x$getmatrix()[1,1])
#         message(eval(parse(text=x$mname))[1,1])
#         tx <- paste(c(cachedObject, " <- ", "makeCacheMatrix(", txMatrixName, ")"), collapse=NULL)
#         message(tx)
#         eval(parse(text=tx))
#         
#         message(x$getmatrix()[1,1])
#         message(eval(parse(text=x$mname))[1,1])
        
  }
#   else
#   { message(paste(c("Matrix", x$mname, "has changed since initiation of object", cachedObject), collapse=" "))
#     message(paste(c("Reinitializing object", cachedObject), collapse=" "))
#     message(x$getmatrix()[1,1])
#     message(eval(parse(text=x$mname))[1,1])
#     message("Try")
#     assign(cachedObject, makeCacheMatrix(eval(parse(text=x$mname))))
#     message(t)
#     message("Try2")
#     message(x$getmatrix()[1,1])
#     message(eval(parse(text=x$mname))[1,1])
#   }
#   
  
  # check if inverse is already stored in the object
  # if so, return cached value
  localvalue <- x$getinv()
  if (!is.null(localvalue)) {
      message(paste(c("Using cached value of inverse of ",x$mname), sep=""))
      return(localvalue)
  }
  
  #if it's not recorded, set inverse and return calculated value
  locinv <- solve(x$getmatrix())
  x$setinv(locinv)
  locinv
    
}
