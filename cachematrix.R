# Calculation of the inverse of a matrix is computationally expensive
# The code below lets you calculate the inverse of a matrix
# However, if the inverse already has been calculated and stored
# in makeCacheMatrix object, the cached inverse is returned instead

# 1.
# This function creates a special "matrix" 
# It creates four functions which allow you to:
# 1. Get the value of a matrix
# 2. Set the value of a matrix
# 3. Get the inverse of a matrix
# 4. Set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y){
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) inv_mat <<- invert
  getinvert <- function() inv_mat
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

# 2.
# This function calculates the inverse of the special "matrix"
# If the inverse has already been calculated it returns the cached inverse
# from an object created by makeCacheMatrix()
# The commented out "if" is just a check if it is a square matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert_matrix <- x$getinvert()
  if(!is.null(invert_matrix)) {
    message("getting cached data")
    return(invert_matrix)
  }
  #if(nrow(x$get()) != ncol(x$get())){
   # message("square matrix is required to obtain the inverse")
    #return(NULL)
  #}
  data <- x$get()
  invert <- solve(data)
  x$setinvert(invert)
  invert
}

# quick tests

m1 <- matrix(1:4, 2, 2)
mCM <- makeCacheMatrix(m1)
mCM$get() # gives me the input matrix
mCM$getinvert() # gives me NULL
# let's calculate the inverse
cacheSolve(mCM) # prints out the inverse
cacheSolve(mCM) # prints out the cached data

# let's set a new matrix
m2 <- matrix(11:14, 2, 2)
mCM$set(m2)
mCM$get() # gives me the newly set matrix
mCM$getinvert() # gives me NULL (as set() made invert "NULL")
# calculating the inverse again
cacheSolve(mCM) # prints out the inverse of the newly set matrix
cacheSolve(mCM) # prints out the cached data
