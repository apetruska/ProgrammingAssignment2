## Create a matrix and calculate the inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x,nrow, ncol) 
	{
      matrix <- matrix(x,nrow,ncol)
	inverse <- NULL
	library(MASS)
	inverse <- ginv(matrix)

	#Cache variables
	matrix <<- matrix
	matrixinv <<- matrix
	inverse <<- inverse

	#Output
	matrix
	matrixinv
	inverse
      }


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix,nrows = nrow(matrix),ncols = ncol(matrix)) 
	{
	matrixsolve <- matrix(x,nrows,ncols)
      	if(!is.null(inverse)
			&& identical(matrixsolve, matrix)
			&& identical(matrix,matrixinv))
			{
			message("getting cached data")
            	return(inverse)
			}		
		else
			{
			library(MASS)
			inverse <- ginv(matrixsolve)
			inverse <<- inverse
			}
	matrixsolve
	inverse	
	}
