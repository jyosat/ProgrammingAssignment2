makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  
  
  # set function
  
  # Sets the matrix itself but not the inverse
  
  set <- function(y) {
    
    x <<- y
    
    invmat <<- NULL
    
  }
  
  
  
  get <- function() x
  
  
  setinverse <- function(inverse) invmat <<- inverse
  
  
  # Get the inverse
  
  getinverse <- function() invmat
  
  
  
  # Encapsulate into a list
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse)	
  
}


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getinverse()
  
  
  if(!is.null(invmat)) {
    
    message("Getting cached matrix")
    
    return(invmat)
    
  }
  
  # Get the matrix itself
  
  data <- x$get()
  
  # Find the inverse
  
  invmat <- solve(data, ...)
  
  x$setinverse(invmat)
  
  # Return this new result
  
  invmat    
  
}

