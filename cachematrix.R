## Creates an object contain get, set, getinv and setinv functions
## to get data, set data, get inverse matrix and set inverse matrix respectively
makeCacheMatrix <- function(x = matrix()) {
  ## Set inverse to NULL
  i <- NULL
  ## Function to set matrix data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Function to get matrix data 
  get <- function() x
  ## Function to compute and set cached inverse matrix
  setinv <- function(inv) i <<- solve(x)
  ## Function to return inverse matrix 
  getinv <- function() i
  ## Returning the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return matrix inverse from cahce if it's cached compute otherwise
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
