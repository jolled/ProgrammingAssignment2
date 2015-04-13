#Example on how to run the code
#1. Load file Source(pathToFile)
#2. create a "special" matrix
#     2.1 x<-makeCacheMatrix(matrix(c(3,2,1,2),2,2))   //set matrix when object is created, or
#     2.2 x<-makeCacheMatrix()
#         x$setMatrix(matrix(c(3,2,1,2),2,2))          //set the matrix afterwards
#3. Calculate the inverse, cacheSolve(x)
#4. Print the cached inverse x$getInverse()
#5. Calculate the inverse again (cached version will be printed), cacheSolve(x)


#The function makeCacheMatrix return a list of functions that
#that is used to calculate the inverse of a square matrix.
makeCacheMatrix <- function(x = matrix()) {
      #Variable i contains the cached inverse (if calculated) 
      #set default value for i
      i<-NULL
      
      #Function setMatrix: Store new matrix and reset cached inverse
      setMatrix<-function(y) {
            x<<-y
            i<<-NULL
      }
      
      #Function setInverse: Cach a new inverse
      setInverse<-function(inverse){i<<-inverse}
      
      #Function getMAtrix: returns matrix
      getMatrix<-function() {x}
      
      #Function getInverse: return cashed inverse 
      getInverse<-function() {i}
      
      #List for all functions in makeCacheMatrix
      list(setMatrix = setMatrix, 
           setInverse = setInverse,
           getMatrix = getMatrix,
           getInverse = getInverse)
      
}


#The function cacheSolve calculates the inverse of matrix 
#created with makeCacheMatrix
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
      
      #get matrix to calculate inverse for
      data<-x$getMatrix()
      
      #Calculate the inverse
      inverse<-solve(data)
      
      #Cache the inverse
      x$setInverse(inverse)
      
      #return  Inverse
      inverse
}
