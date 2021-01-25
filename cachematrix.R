## Put comments here that give an overall description of what your
## functions do

##In this function there are 4 functions inside: set(), get(), setinverse() and gtinverse().
##In the first function set(), takes y as the argument, inside the function the argument y
##is assigned to the object x and the object i is given the value of null. In the get function 
## is retrieved the object x. In the function set inverse the argument is the inverse, at the
## same time this is assigned to the object i. In get inverse the value of the object i is
## retrieved. Finally, in the final part it creates a list in which each element (function)
## is named.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function()x
     setinverse <- function(inverse)i <<- inverse
     getinverse <- function()i
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## The argument in this function is the same x than in the makeCacheMatrix. Inside the function
## cavheSolve, first, is called the function getinverse and this is assigned to the object i.
##After this, there is a conditional loop, if the i value is not null then is returned the value 
## and is printed a message.If the previous condition is false, then is called the function get
## to assign the value of the object x to the object data. In the last part is computed the 
## inverse of the matrix and is set in the object i.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
          message("Getting cached data")
          return(i)
     }
     ## Return a matrix that is the inverse of 'x'
     data <- x$get()
     i <- solve(data,...)
     x$setinverse(i)
     i
}
aMatrix <- makeCacheMatrix(matrix(1:9, 2, 2))
cacheSolve(aMatrix)

