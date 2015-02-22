### cachematrix.R -- functions to get/store matrix inverse soln's in cache
#    20150222: Coursera R Programming course, assignment 2


## makeCacheMatrix -- 
#    returns list of four functions to set/get value of matrix
#    set/get inverse of matrix
#    in "cache" (outside current working environment)
#   
#    copied from R programming assn 2
#    e.g. "makeVector" w/ only changes to function/object names

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve -- finds inverse for matrix using solve(), unless 
#    solution already cached 
#    copied from R programming assn 2
#    e.g. "cachemean", changed from mean() to solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}


### Function tests
#   see help thread with example invertable matrices to test
#   https://class.coursera.org/rprog-011/forum/thread?thread_id=815

#egMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
# The inverse of this matrix should be
# -24  18   5
#  20 -15  -4
#  -5   4   1

# test
#xM <- makeCacheMatrix(egMatrix)
#xM$get()  # returns egMatrix
#cacheSolve(xM)   # returns correct (inverted egMatrix)
#cacheSolve(xM)   # returns correct w/ "getting cached data" message



