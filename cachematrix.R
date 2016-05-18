## makeCacheMatrix creates a list object of four functions.
## The list object can be assigned to a unique input in the 
## cacheSolve function. The cacheSolve function calculates the 
## inverse matrix. It also checks to see if the input has already
## been assigned a list object.

## This function stores a matrix as a list object.

makeCacheMatrix <- function(x = matrix()) {
	im<- NULL
	set<-function(y){
	x<<-y
	im<<-NULL
	}
	get <- function() x
	setmatrix <-function(inversematrix) im <<-inversematrix
      getmatrix <-function() im
	list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)

}


## This function calculates the inverse and checks if the 
## input has already been assigned a list object. If so, 
## then it will return the values in the cache and not re-
## calculate its inverse.

cacheSolve <- function(x, ...) {
        im <-x$getmatrix()
	if(!is.null(im)){
		message("getting cached data")
	return(im)
	}
	data <- x$get()
	im <-solve(data,...)
	x$setmatrix(im)
	im

}


