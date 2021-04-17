##In the following code we specify a particular
##matrix and we cache its inverse from the first function
##the next process involves calculating inverse
##But if matrix remains same then we retrieve from cache
##____________________________
##Function 1
##This Function 'makeCacheMatrix' creates matrix object
##and can cache its inverse
##inbuilt function 'solve' is used to compute inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function() {inver} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Function 2
##In this function we compute inverse of matrix
## If already solved that is null, we retrieve from cache
##We write message for the same

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat,...)
  x$setInverse(inver)
  inver
}