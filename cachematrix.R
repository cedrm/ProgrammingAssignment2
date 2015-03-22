## Caching the Inverse of a Matrix

## Tow R Functions
## makeCacheMatrix & cacheSolve
## This pair of functions permit cache the inverse of a matrix
## Exemple :
## square invertible matrix : |5 6   => Inverse |-4    3
##                            |7 8              | 3.5  -2.5
## c=rbind(c(5, 6), c(7,8))
## a <- makeCacheMatrix(c)
## cacheSolve(a)
## a = |-4    3
##     | 3.5  -2.5


## R function 
## This function creates a special "matrix" object that can cache its inverse
## Prototype : makeCacheMatrix(square invertible matrix)
## 
makeCacheMatrix <- function(x = matrix()) {
      
	  m <- NULL
      
	  set <- function(y) {
        x <<- y
        m <<- NULL
        }
    
		get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
		list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## R function 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Prototype : cacheSolve(makeCacheMatrix return)
##
cacheSolve <- function(x, ...) {

       ## Return a matrix that is the inverse of 'x'
		
	   m <- x$getsolve()
       
	   if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
       
	   data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}
