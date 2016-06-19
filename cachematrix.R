## This function is very similar to the "vector" examples given on the assignments page
## the makeCacheMatrix takes a matix (that we assume is square and inversable) 
## and "cache" it. There are 4 subfunctions in the overall function
## they are setm(x) (set the value of a matrix "x" as the new current martix)
## getm() returns the value of the current martix
## setinver(inv) this sets the inverse of a new matrix "inve"
## getinver() return the current value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inver<- NULL
  setm<- function(inv){
    x<<-inv
    inver<<-NULL
  }
  getm<- function() x
  setinver<- function(inve) inver<<- inve
  getinver<- function() inver
  list(setm=setm,getm=getm,
       setinver=setinver, 
       getinver=getinver)
}


## the function cacheSolve is used together with makecachematrix
## the function takes the agruements of x where x is an object of makecachematrix
## as makecachematrix has subfunctions we can use those subfunctions to "solve" for the inverse
## once we solve for the inverse of a matrix we can set it using the $setinver function
## this function also checks if the value has already been cached, if so it just returns that value along with message


cacheSolve <- function(x, ...) {
 
  inv<-x$getinver()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$getm()
  inv<- solve(mat)
  x$setinver(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}


