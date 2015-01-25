## The two functions below are used concurrently to find the inverse of a matrix x. 
## If the inverse has already been found in cache, then it is returned from cache without the need to calculate the inverse again.


## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'm' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setImatrix() to assign computed inverse matrix (of x) to m;
## 4. Provides function getImatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

  mymatrix <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) mymatrix <<- Imatrix
  getImatrix <- function() mymatrix
  
  # return a list of functions as an R object
  list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)  
  
}


## --------------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.
## NOTE: argument x for this function must be cached, i.e. a list returned from
##       calling makeCacheMatrix(x).
## --------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  mymatrix <- x$getImatrix()
  start.time = Sys.time()
  if(!is.null(mymatrix)){
    message("Cached data found. Getting result... Done.")
    dur = Sys.time() - start.time
    print(dur)
    ##print(mymatrix)
    return(mymatrix)
  }
  else {
    message("No cached data found. Calculating inverse matrix...")
    data <- x$get() # obtains matrix from object x
    mymatrix <- solve(data) # finds inverse matrix
    x$setImatrix(mymatrix) # assigns resulting inverse matrix to object x
    message("Done.")
    dur = Sys.time() - start.time
    print(dur)
    ##print(mymatrix)
    return(mymatrix)
  } 
}
