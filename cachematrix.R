# These functions takes an input (e.g. matrix) then calculates the inverse of it and it is 
# stored in the cache so that whenever you call 

# This function calculates the inverse of the matrix if there is nothing in the cache. And
# stores it in the cache so that whenever you call cacheSolve it automatically returns the
#inverse of the matrix if present.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #this will be our inverse matrix which can be printed, if a new matrix is 
                  #entered then it will be set to zero
  
  get <- function() x #This function will return the orginal value that was put in
  setinverse <- function(ginv) inverse <<- ginv 
              # setinverse is called by cachinvserse() during the first excution or when new input
              #  is given and this will store the value using superassignment
  getinverse <- function() inverse #calling this function will recall the cached value
  
  list(set = set, get = get, #  This is accessed each time makeCacheMatric() is called, 
       setinverse = setinverse, #   Each time we make a new object. This is a list of 
       getinverse = getinverse)} #   the internal functions ('methods') so a calling function      
       setmean = setmean,    #   knows how to access those methods.
       getmean = getmean)  
}

## The cachesolve function prints out an inversed matrix after calling this function.
## This result is also autsaved in to the cache, and can be recalled untill new matrix
## is called into the above function.

cacheSolve <- function(x, ...) {   #input x created by makeCacheMatrix
  inverse <- x$getinverse()       #
  if(!is.null(inverse)) {         # if inverse matrix was in cache then it continues
    message("getting cached data") #Leaves a message that its taking the inverse from the cache
    return(inverse)                 #Returns the inverted matrix
  }
  data <- x$get()             #If there was no inverse matrix in cache then it will get here
  inverse <- ginv(data, ...)  #so it now will inverse the input matrix
  x$setinverse(inverse)       #and stores it in the cache as specified in makeCacheMatrix
  inverse                     #returns the inverse matrix
}
