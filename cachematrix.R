
## This function creates a special "matrix" object that 
## has a list of functions attached to it; a 'function list'. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL #Set the local inverse to nil when this function is called
      set <- function(y) {
            x <<- y
            i <<- NULL 
            # each time 'set' function is run, 
            # inverse 'i' in higher environment set to null
      }
      get <- function() x #This function changes the matrix assigned to the function list

      setinv <- function(inv) i <<- inv
      # can set the inverse manually if you want. 
      # Not recommended. Called by 'cacheSolve' though!
      getinv <- function() i

      list(set = set, get = get,
           getinv = getinv, setinv= setinv)
}


## This function computes the inverse of the special "matrix" 
## returned by the 'makeCacheMatrix' function above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      # source("cacheSolve.R")
      i <- x$getinv()
      if(!is.null(i)) {
            print("if(!is.null(i))")
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data) #Calculates the inv of the currently 
      # assigned matrix
      x$setinv(i) 
      i
      }
