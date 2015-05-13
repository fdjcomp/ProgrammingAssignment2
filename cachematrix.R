#Description:
#Usage:
#Arguments:
#Value:
#References:
#See Also:
#Examples:


## Put comments here that give an overall description of what your functions do

#Description: 

#	makeCacheMatrix stores matrix data and specialized matrix functions (to get/set the matrix and its inverse) in a list

#	cacheSolve will takes a list made with makeCacheMatrix as argument, and will return the inverse of the matrix caching
#	the inverse matrix result, so that it will not have to be recalculated when needed again; 
#	when the inverse is requested a message ("getting cached matrix inverse") is printed when using the cached result


#Usage:
# sm<-makeCacheMatrix(matrixM)
# cacheSolve(sm)

#Example
# > sm<-makeCacheMatrix(matrix(1:4,2,2))
#first call no message
# > cacheSolve(sm)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#second and later calls: the message is printed
# > cacheSolve(sm)
# getting cached matrix inverse
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



#makeCacheMatrix: 
## Write a short comment describing this function

#Description: store in a list a matrix and special functions to get/set the matrix and its inverse 
#Argument: a matrix (should be invertible, but is not checked)
#Value: a list with four items, each a function 
#	fsetmat: to set the matrix data
#	fgetmat: to get the matrix data
#	fgetminv: to get the inverse of the matrix (will be null if inverse not yet calculated)
#	fsetminv: sets the inverse matrix

makeCacheMatrix <- function(mat = matrix()) {

        minv <- NULL

        fsetmat <- function(arg) {
                mat <<- arg
                minv <<- NULL
        }

        fgetmat <- function() mat

        fsetminv <- function(arg) minv <<- arg

        fgetminv <- function() minv
	
        #return list
	list(	fsetmat = fsetmat, 
		fgetmat = fgetmat,
             	fsetminv = fsetminv,
             	fgetminv = fgetminv
	)


}





## Write a short comment describing this function

#Description: return a matrix that is the inverse of the argument
#Usage: cacheSolve(arg<-makeCacheMatrix(matrixM))
#Arguments: argument is a list as produced by makeCacheMatrix()
#Value: returns inverse of matrix stored in argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$fgetminv()

        if(!is.null(minv)) {
                message("getting cached matrix inverse")
                return(minv)
        }

        data <- x$fgetmat()

        minv <- solve(data)

        x$fsetminv(minv)

        minv
	

}


#could simplify these three lines
#data <- x$fgetmat()
#minv <- solve(data)
#x$fsetminv(minv)
#to
#x$fsetminv(solve(x$fgetmat))











