## We need to cach the Inverse of a Matrix. 
## We assuming that the matrix supplied is always invertible.
## Let's try to solve this assigment.

## First function will make special cach matrix from our variable.

makeCacheMatrix <- function(x = matrix()) {
   i<-NULL
   set<-function(m){
      x<<-m
      i<<-NULL
   }
   get<-function()x
   setinver<-function(inver)i<<-inver
   getinver<-function()i
   list(set=set,get=get,setinver=setinver,getinver=getinver)
   
}


## Second function will check if we have computed value of matrix inversion, 
## if we have the value it will show massage "We have computed matrix inversion",
## and if we haven't it will compute this value.

cacheSolve <- function(x, ...){
   i<-x$getinver()
   if(!is.null(i)){
      message("We have computed matrix inversion")
      return(i)## Return a matrix that is the inverse of 'x'
   }
   inv<-x$get()
   i<-solve(inv,...)
   x$setinver(i)
   i
}
