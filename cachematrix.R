## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a object contain get, set, getinv and setinv functions
## to get data, set data, get inverse matrix and set inverse matrix respectively
makeCacheMatrix <- function(x = matrix()) {
  ## Set inverse to NULL
  i <- NULL
  ## Set matrix data function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Get matrix data function 
  get <- function() x
  ## Function to compute and set cached inverse matrix
  setinv <- function(inv) i <<- solve(x)
  ## Function to return inverse matrix 
  getinv <- function() i
  ## Returning the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Return matrix inverse from cahce if it's cached compute it then return if not
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if cached inverse exists, if so, return it
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Computing and returning inverse
  data <- x$get()
  i <- solve(data)
  message("getting computed data")
  x$setinv(i)
  i
}
