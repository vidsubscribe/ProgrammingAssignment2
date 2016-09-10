## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {
           xinverse<-NULL
           set<-function(matrixtobereversed) 
                {x<<-matrixtobereversed
                 xinverse<<-NULL
                }
           get<-function() x
           setinverse<-function(matrixinverse) xinverse<<-matrixinverse
           getinverse<-function() xinverse
           list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversedmatrix<-x$getinverse()
  if (!is.null(inversedmatrix))
  {
    message("getting cached inverse matrix")
    return(inversedmatrix)
  }
  data<-x$get()
  inversedmatrix<-solve(data)
  x$setinverse(inversedmatrix)
  inversedmatrix
  
}
