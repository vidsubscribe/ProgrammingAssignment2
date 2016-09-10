## Put comments here that give an overall description of what your
## functions do

## -- ADDED VIDYADHAR 09/09
## makeCacheMatrix creates environment to cache a 'matrix' symbol to be reversed with the respective methods
## cacheSolve accesses the cached value of inverse for the 'matrix' symbol or creates new inverse and caches in the environment

## Write a short comment describing this function
## -- ADDED VIDYADHAR 09/09
## This function accepts a matrix and creates a cached environment
## matrixtobereversed is the passed in matrix
## matrixinverse is the cached matrix for the inversed value

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
## -- ADDED VIDYADHAR 09/09
## This function accepts the makeCacheMatrix environment
## calculates the inverse of the matrix set in makeCacheMatrix, if it was not cached already
## returns the inverse of the matrix, if it was already cached

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
