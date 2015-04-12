## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse<<-NULL
      #Store new matrix and reset cashed inverse
      SetMatrix<-function(x) {
            matrix<<-x
            inverse<<-NULL
      }
      #
      setInverse<-function(y){inverse<<-y}
      
      #return matrix (raw data)
      getMatrix<-function() {matrix}
      #return cashed inverse 
      getInverse<-function() {Inverse}
      
      
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      #Check if inverse is cashed. If so return 
      #casched data and exit function
      inverce<-x$getInverse()
      if(!is.null(inverse)) {
            message("cached data exist")
            return(inverse)
      }
      
      #Inverse is not cashed. Lets calculate inverse for x
      data<-x$getMatrix()
      inverse<-solve(data)
      #cash inverse
      x$storeMatrix()
      #return  Inverse
      inverse
      
}
