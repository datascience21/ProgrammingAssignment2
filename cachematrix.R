## Functions below accepts matrix as input and calculates the inverse of the matrix 
## If available in memory inverse is fetched from there, if not it computes the inverse  and stores it in memory. 
## It also prints out an error message if the matrix created is singular and inverse cannot be computed

## makeCacheMatrix  creates a square matrix (nxn). It has 3 functions getmatix,setinverse and getinverse
## 1.setmatrix sets the value of the matrix
## 2.getmatrix returns the matrix created
## 3.setinverse stores the inverse of the matrix
## 4.getinverse returns the inverse of the matirx
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y = matrix() ){
        x <<-y
        inverse <<-NULL
    }
    getmatrix <- function() x 
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    list(setmatrix =setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve returns the inverse of the matrix. If the result is available  in memory ,
## it fetches the inverse from memory, otherwise inverse is calculated,
## stored in memory and printed out.
## If the matrix is singular and inverse cannot be computed it prints out an error message

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        print('Result from Cache')
        return(i)
    }
    data<-x$getmatrix()
    if (det(data) == 0)
    {
        print("Matrix is singular. Inverse cannot be computed")
    }
    else
    {
        i<-solve(data)
        x$setinverse(i)
        i
    }
}
