## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL	#initialise the inverse of x
        set <- function(y) { #when the 'x' is set or updated,inv is set to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x	#return  the matrix x
        setinv <- function(i) inv <<- i #inv is set to i
        getinv <- function() inv	#return the inv
        list(set = set, get = get, #list contains the function within this object
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinv() #get the inverse of x for the object
        if(!is.null(inv)) {	#if the inv is not null,it means that the inv has been calculated before
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #get x
        inv <- solve(data, ...) #calculate inverse of x
        x$setinv(inv) #update value of inv
        inv # print the inverse of x

}
