## Put comments here that give an overall description of what your
## functions do


#####################################################
#### 'Overall Comments' ####
# makeCacheMatrix: will scrub the matrix Inversion that may exist
#(reset it) everytime it is called
# will then create functions stored in vector elements ready to be called by our second function
# stored in a list

# cacheSole: will examine if the matrix inversion exists (in x$getMatrixInverted())
# if it is NULL, then go and solve the matrix inversion, else use what is there
######################################################

####################
### 'function' comments' follow from here ###
####################

### makeCacheMatrix ###

## args is a matrix class object
## 1 - everytime this function is called
## set the m (cache storage) to NULL
## 2 - setup other functions you will be calling in the special 'vector'
## 3 - set 'higher' level environment objects with '<<-' operate to be used by other functions
## to be evaluated in later calls from other functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixInverted <- function(matrixInverted) m <<- matrixInverted
        getmatrixInverted <- function() m
        list(set = set, get = get,
             setmatrixInverted = setmatrixInverted,
             getmatrixInverted = getmatrixInverted)
}

### CacheSolve ###

## If m is NULL, calc the matrix inversion, if m already exists,
## then use it (dont calc anything)=>will print the 'getting cache data' statement
## and return the matrixInverted that has been stored

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrixInverted()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()

        m <- solve(data,...)
        x$setmatrixInverted(m)
        m
}
