## Put comments here that give an overall description of what your
## functions do


# This function creates a special matrix that:
# 1) Sets the value of the matrix
# 2) gets the value of the vector
# 3) sets the value of the iverse of the matrix
# 4) Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  set <- function (y){
    x <<- y
    inverse <<- NULL
  }

  get <- function(){
    x
  }

  getInverse <- function(){
    inverse
  }

  setInverse <- function(z){
    inverse <<- z
  }

  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)

}



# This funtion calculates the inverse of the special matrix created with the above function. However, it 
# first checks whether the inverse has already been calculated. If so, it gets the inverse from the 
# cache ans skips the computation. . Otherwise, it calculates the inverse of the matrix and sets the
# value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting inverse from the cache")
    return (inverse)
  } else{
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    return (inverse)
  }

}
