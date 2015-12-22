## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        set<-function (y) { #Set the value of the matrix
                x<<-y
                inv<-NULL
        }
        
        get<-function() x #get the value of the matrix
        setinv<-function(solve) inv<<-solve #set the inverse of the matrix
        getinv<-function() inv #get the inverse of the matrix
        list (set=set, get=get, setinv=setinv, getinv=getinv) #return the list of functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        
        if (!is.null(inv)) { #evaluate if the data has been cached
                message ("getting cached data")
                return (inv) #return the data from cache
        }
     
        data<-x$get() #if not in cache, the program runs the functions necessary to get the inverse 
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
