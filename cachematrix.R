#Making a special Matrix that helps in caching
makeCacheMatrix<-function(mat=matrix(1)){
  inverse<-NULL
  set<-function(mat1){
    mat<<-mat1
    inverse<<-NULL
  }
  get<-function()mat
  setinverse<-function(inv1){
    inverse<<-inv1
  }
  getinverse<-function()inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#finds the inverse of matrix which uses caching
cacheSolve<-function(mat=matrix(1)){
  inverse<-mat$getinverse()
  if(is.null(inverse)){
    inverse<-solve(mat$get())
    mat$setinverse(inverse)
  }
  inverse
}