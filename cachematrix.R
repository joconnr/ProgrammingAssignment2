
## Create a Matrix that Can Cache its Inverse
makeCacheMatrix <- function(x = matrix(nrow=2, ncol=2, 1:4)) { 
	m <- NULL
	## Load the set/get and inversing functions
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
	get <- function() x
	setinverse <- function(solve) m <<- solve
      getinverse <- function() m
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

	## Get and returned inverse if it exists
      m <- getInvV$getinverse()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- getV$get()

	## Invert it and set it back
	m <- solve(data, ...)
	setInvV$setinverse(m)

	## Return a matrix that is the inverse of 'x'
      m
}