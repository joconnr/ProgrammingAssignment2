
cacheArea = new.env(hash=TRUE)

## Create a Matrix that Can Cache its Inverse
makeCacheMatrix <- function(x = matrix(nrow=2, ncol=2, 1:4)) { 
      m <- NULL
      ## Load the set/get and inversing functions
      set <- function(y) {
            x <<- y
            m <<- NULL
		cacheArea$x <- y
		cacheArea$m <- NULL
      }
	get <- function() x
	setinverse <- function(solve, data) { 
		m <<- solve
		cacheArea$m <- solve
		cacheArea$mOrig <- data
	}
      getinverse <- function() {
		m <- cacheArea$m
		m  
      }
	vec <- list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

	## Create the Matrix to return
	mat <- matrix(nrow=1, ncol=4)
	dimnames(mat) = list(c("1"), c("set", "get", "setinverse", "getinverse"))
	for(i in 1:4) {
		mat[[i]][[1]] = vec[i]	
	}
	mat
} 

## Computes the Inverse of the makeCacheMatrix Matrix
cacheSolve <- function(x, ...) {
	## Unload the methods from the matrix
	setV <- x[[1]][[1]]	
	getV <- x[[2]][[1]]	
	setInvV <- x[[3]][[1]]
	getInvV <- x[[4]][[1]]
	## Get the matrix
	data <- getV$get()

	## Get and return inverse if it exists in cache
      m <- getInvV$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
		## Make sure originally cached matrix hasn't changed 
		if(!identical(data, cacheArea$mOrig)) {
			m <- solve(data, ...)
			setInvV$setinverse(m, data)
		}          
		return(m)
      }

	## Invert it and set it back to cache
	m <- solve(data, ...)
	setInvV$setinverse(m, data)

	## Return a matrix that is the inverse of 'x'
      m
}