## The following functions are used to cache the inverse of a matrix. It is useful due to
## the fact that matrix inversion is time-consuming to compute repeatedly. 
## It is assumed the matrix is always invertible.


## This function creates a special "matrix". A list contatining a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}




## The function below calculates the inverse of the special "matrix" from the function 
## above. It first checks to see if the inverse has already been calculated. If so, it gets
## the result and skips computation. Otherwise, it is calculated and sets the value in the
## cache through the setinv function.

cacheSolve<-function(x,...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

