## These functions take a matrix and cache its inverse to prevent the need to recalculate the 
## inverse constantly. The makeCacheMatrix function creates an object with the ability to cache 
## its inverse, while the cacheSolve matrix solves for the inverse (if it has not been calculated) 
## and saves the inverse to the object of type makeCacheMatrix. 


## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #m initialized to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } #the set function pushes the value of y to x and resets m to NUL if a new matrix is passed
  get <- function() x
  setmatrix <- function(solve) m <<- solve #setmatrix is an anonymous function which replaces m with the inverse of m
  getmatrix <- function() m #get matrix returns m, if it has been cached
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix) #creates a makeCacheMatrix object with set, get, setmatrix, and getmatrix attributes
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cacheSolve function will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  m <- x$getmatrix() #this takes the value of getmatrix from a makeCacheMatrix object, if it exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #if getmatrix is not NULL, the cached data are returned
  }
  data <- x$get() # the data are grabbed from the get() object
  m <- solve(data, ...) # m is replaced with the inverse of the requested matrix
  x$setmatrix(m) # the calculated inverse is cached to the setmatrix object
  m # the inverse m is calculated and returned
}