# function cacheSolve finds inverse matrix 
# if you want to clear a cache then type: m <- NULL in console


#auxiliary function ; it contains smaller functions which are passed as a result
makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  set_inv <- function(solve) m <<- solve
  get_inv <- function() m
  list(set=set, get = get,  #list of functions
       set_inv = set_inv,
       get_inv = get_inv)
}

#main function; it returns inverse matrix
#before typing another matrix remember to write m <- NULL in the console. Otherwise, it will print previous inverse matrix

cacheSolve <- function(x, ...) {
  if(!exists("var")) var <- matrix()
  m <- x$get_inv()
  if(!is.null(m)) {    # if m is assigned
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set_inv(m)
  m
}
#example input
#cacheMatrix(makeCacheMatrix(matrix(c(1,4,6,10,567,3,100,100,999), 3, 3)))
