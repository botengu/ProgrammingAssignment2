## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix gives the right type of matrix to perform the required problem 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cachesolve allows us to perform the inverse of the matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  print("almost")
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}

# this is another function to get the inverse 

inv <- function(mat){
  m <- mat^(-1) 
  m 
}

# To run the program, take an initial function A and run > cacheSolve(makeCacheMatrix(A))

