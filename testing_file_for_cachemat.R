#1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
#2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

##below are the makeCacheMatrix and cacheSolve functions
makeCacheMatrix <- function(matrix = matrix()){
  inverse <- NULL
  set <- function(cacheMat){
    matrix <<- cacheMat
    inverse <- NULL
  }
  get <- function() matrix
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(matrix, ...){
  mat <- matrix$getinverse()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#below is a testing function to make sure it runs
test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)



################
#below are the example functions provided by the JHU coursera course
###############
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}