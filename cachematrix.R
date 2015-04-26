## This function is to caculated the inverse of matrix, to cache the results and to return the cached inverse if the input argument is repeated.

## This function is to create a list containing 4 functions that assign arguments later used.
makeCacheMatrix <- function (x=matrix()){
	i <-NULL
	set <- function(y){
		x<<-y
		i<<-NULL}
	get <-function()x
	setinv<-function(inv) i<<-inv
	getinv<-function()i
	list(set=set,get=get,setinv=setinv,getinv=getinv)
	} 


## This function is to calculate the inverse of matrix 'x' or to return the inverse matrix of which the matrix 'x' already exists.
cacheSolve<-function(x,...){
	i<-x$getinv()
	if(!is.null(i)){
		message("getting cached data")
		return(i)}
	data <-x$get()
	i<-solve(data,...)
	x$setinv(i)
	i
	}
