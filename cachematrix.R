## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      i<-NULL
      #Store new matrix and reset cashed inverse
      setMatrix<-function(y) {
            x<<-y
            i<<-NULL
      }
      #
      setInverse<-function(inverse){i<<-inverse}
      
      #return matrix (raw data)
      getMatrix<-function() {x}
      #return cashed inverse 
      getInverse<-function() {i}
      
      list(setMatrix = setMatrix, setInverse = setInverse,
           getMatrix = getMatrix,
           getInverse = getInverse)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      #Check if inverse is cashed. If so return 
      #casched data and exit function
      inverse<-x$getInverse()
      if(!is.null(inverse)) {
            message("cached data exist")
            return(inverse)
      }
      
      #Inverse is not cashed. Lets calculate inverse for x
      data<-x$getMatrix()
      inverse<-solve(data)
      #cash inverse
      x$setInverse(inverse)
      #return  Inverse
      inverse
      
}
