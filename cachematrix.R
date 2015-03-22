## This is a R script for calculating & caching an inversion of a matrix. 

## makeCacheMatrix: make cache of inverted value after calculation
## input: matrix
## output: special type of list
makeCacheMatrix <- function(x = matrix()) {

            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(vinverse) m <<- vinverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
    }




## cacheSolve: calculate inverse of a matrix: 
## input: special type of list from makeCacheMatrix
## output: inverted matrix

##first of all the class of input will be checked
##then this function will search a cached value 
##finally will calculate solve function if cache is unavailable. 

cacheSolve <- function(x, ...) {
	if(class(x) !="list"){
		
		message("you should run makeCacheMatrix before calculating inversion")
	} else {	

	m <- x$getinverse()
	if(!is.null(m)) {
	
		message("getting cached data")
		return(m)
	
	}
	message("calculating new value")	
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
	
        ## Return a matrix that is the inverse of 'x'

	}
}
