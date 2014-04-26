## This function creates a special "matrix" object that can cache its inverse.
## It actualy is a list containing a function to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix
## 
## Code Example 
## > m<-matrix(1:4, 2,2)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > x<-makeCacheMatrix(m)
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(x)
## getting inverse matrix from cache
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
makeCacheMatrix <- function(x = matrix()) {
        ## initialize a variable to store the inverse matrix  
        i <- NULL
		
		## setter used to set a new matrix
        set <- function(y) {
				## The operators <<- cause a search to made through parent environments
				## for an existing definition of the variable being assigned. 
				## If such a variable is found then its value is redefined, 
				## otherwise assignment takes place in the global environment. 
                x <<- y
				
				## reset the inverse matrix value
                i <<- NULL
        }
		
		## getter used to get the matrix
        get <- function() x
		
		## setter used to set the inverse matrix
        setInverse <- function(inverse) i <<- inverse
        
		## getter used to get the inverse matrix
		getInverse <- function() i
        
		## return a list of all related functions.
		## It works a wrapper of the matrix which include its inverse.
		## The inverse is stored in the parent environment
		list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated and the matrix has not changed, then the 
## cachesolve should retrieve the inverse from the cache. Otherwise, 
## it will calculate its inverse and store the inverse in the Cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## get the inverse of the matrix
		i <- x$getInverse()
        if(!is.null(i)) {
		        ## find and get the inverse from Cache and return it
                message("getting inverse matrix from cache")
                return(i)
        }
		
		## Not found in the cache
		## get the input matrix
        mx <- x$get()
		
		## solve the inverse of matrix
        i <- solve(mx, ...)
		
        ## Store the inverse to the cache
		x$setInverse(i)
		
		## return the inverse 
        i
}
