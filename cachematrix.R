#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #default matrix
        m <- NULL #set initially to 0
        set <- function(y) {  #New assignment
                x <<- y #value in parent environment
                m <<- NULL #If new matrix, again set to NULL
        }
        get <- function() x #returns maxtrix
        setmatrix <- function(solve) m <<- solve #assigns m to parent environment
        getmatrix <- function() m #returns m
        list(set = set, get = get, #makes a list with the 4 functions
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  #returns the inverted matrix
        m <- x$getmatrix() #get the matrix
        if(!is.null(m)) { #checks if matrix had been cached
                message("getting cached data") #if cached, returns message
                return(m) #and returns matrix which already had been inverted
        }
        data <- x$get() #returns matrix defined in x
        m <- solve(data, ...) #inverts the matrix 
        x$setmatrix(m) #sets the inverted matrix to m
        m #prints m (the inverted matrix)
}