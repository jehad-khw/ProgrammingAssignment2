##"makeCacheMatrix" builds a function that return a list of 4 functions that can manipulate the 
##storage of a matrix and its inverse. 
##1- set(): it receives a matrix as an argument and assign it to an object "x" in the 
##           "makeCacheMatrix" environment using "<<-". "<<-" allows to keep the assignee 
##           object within the function's environment for subsequent retrievals. Each time 
##           a matrix set, the inverse object "x_inverse" got reset to ensure that x_inverse 
##           is correct "if" it has a value.
##2- get(): returns a matrix saved previously in environment object "x".
##3- set_inverse(): it reveives the calculated inverse matrix and assign it to an object 
##           "x_Inverse" in the "makeCacheMatrix" environment using "<<-".
##4- get_inverse(): returns the inversed matrix, or null if it is not calculated yet.

makeCacheMatrix <- function(x = numeric()) {
  x_inverse<-NULL
  set<- function(new_x){
    x<<-new_x
    x_inverse<<-NULL
  }
  get<- function() x
  set_inverse<- function(new_x_inverse) x_inverse<<-new_x_inverse
  get_inverse<-function() x_inverse
  
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


##"cacheSolve" function receives an object of "makeCacheMatrix" class. 
##It first check if the inverse "wasn't" previously  calculated and assigned to an 
##environment object (i.e. cashed). If the matrix inverse is cashed, then it returns 
##the cashed inverse matrix, else it calculates the new matrix inverse and assign it 
##to the "makeCacheMatrix" object received as an argument.
cacheSolve <- function(x, ...) {
  
  
  check_x_Inverse<-x$get_inverse()
  if(!is.null(check_x_Inverse)){
    print("Inverse is cashed")
    return (check_x_Inverse)      
  }
  check_x<-x$get()
  if(!is.matrix(check_x)){
    print("matrix is not set yet!")
    return (NA)      ## Return a matrix that is the inverse of 'x'
  }
  check_x_Inverse<-solve(x$get())
  x$set_inverse(check_x_Inverse)
  check_x_Inverse    ## Return a matrix that is the inverse of 'x'
}

##Examples of inversible matrises
##5x5:  x$set(matrix(c(10,7,14,4,22,8,9,2,18,23,3,11,15,19,24,12,13,16,20,25,6,5,17,21,26),5,5))
##3x3:  x$set(matrix(c(1,0,5,2,1,6,3,4,0),3,3))
##2x2:  x$set(matrix(c(4,3,3,2),2,2))