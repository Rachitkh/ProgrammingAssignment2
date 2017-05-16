## Below functions help in caching the inverse of the matrix which is sometimes costly computation with huge data sets


## makeCacheMatrix function create a matrix object which can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL  #intialize inverse
	
	#Set value, and set invr=NULL whenever set value
	
	set <- function(y) {
                x <<- y
                invr <<- NULL
    }
	
	#get value
	get <- function() {
				x    #return x
	}
	
    setInverse <- function(inverse){
				invr <<- inverse  #Set the inverse
	}
	
    getInverse <- function(){
				invr   #get the inverse
	}
	#Return list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
	

}


## It create inverse of matrix created in makeCacheMatrix. If the inverse already exists, then it pull the data from  cache
## Thus, helping in saving the computational time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getInverse()
		#Check  if inverse exists in cache
        if (!is.null(inv)) {
                message("Cached data Exists. Sending the cache data.")
                return(inv)
        }
		#Cache data doesn't exist
		message("Cached data doesn't exists.")
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
		
}
