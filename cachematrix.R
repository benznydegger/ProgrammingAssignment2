## --------------------------------
## CourseraCourse: DataScience - R Programming -
## --------------------------------
## ProgrammingAssignment2 : Caching the Inverse of a Matrix
## Set of functions to cache computed inversed square matrix
## Student: Benz Nydegger
## Date: 07-Apr-2018


## Below function creates a square matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Set the  matrix
  inversed <- NULL
  set_martix <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  # get matrix
  get_matrix <- function() x	
  
  # Cache the inversed 
  set_inversed_matrix <- function(inverse_matrix) inversed <<- inverse_matrix
  get_inversed_matix <- function() inversed		
  
  # Returned list		
  list(set_martix = set_martix, get_matrix = get_matrix, set_inversed_matrix = set_inversed_matrix, get_inversed_matix = get_inversed_matix)
  
}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inversed <- x$get_inversed_matix()
  
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get_matrix()
  inversed <- solve(data)
  x$set_inversed_matrix(inversed)
  inversed
}



