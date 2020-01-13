####################################
####
#### Sudoku with R
####
####  Created by Tomaz Kastrun
####  Date: January, 11, 2020
####  Version: 0.0.2
####


####   Minor changes 
####  ToDo: Write checker decomposition of [x,y] into number for function Valid
####  ToDo: Checker for while loop and FALSE TRUE values

#####################################


#######################
### Easy Sudoku board
#######################



sudoku <- matrix(data=c(
6,0,0,2,1,0,0,3,0,
5,0,9,0,0,0,6,0,0,
2,0,0,9,7,0,0,0,4,
0,0,2,3,0,4,0,0,0,
0,6,0,0,5,0,0,9,0,
0,0,0,1,0,9,7,0,0,
9,0,0,0,3,8,0,0,6,
0,0,7,0,0,0,2,0,5,
0,8,0,0,4,2,0,0,9), nrow=9, ncol=9, byrow=FALSE
)


#sudoku <- matrix(data=c(
#  7,8,
#  6,2,
#  2,4 ), nrow=3, ncol=2, byrow=FALSE
#)



#######################
### Helper functions
#######################



get_zeros <- function(board_su){
  #empty df
  df <- data.frame(i=NULL,j=NULL)
  for (i in 1:nrow(board_su)){
    for (j in 1:ncol(board_su)){
      if (board_su[i,j] == 0) {
        a <- data.frame(i,j)
        #names(a) <- c("i", "j")
        #df <- rbind(df, a)
        df <- a
        return(df)
      } 
    }
  }
}



solver <- function(board_su){
  state <- FALSE
  while (state == FALSE) {
    find <- get_zeros(board_su)
    print(find)
    if (nrow(find) == 0) {
      state <- TRUE
    } else {
      row <- as.integer(find[2])
      col <- as.integer(find[1])
    }
    
    for (i in 1:9){ 
      if (validater(board_su, i, c(row, col)) == FALSE){
        board_su[row,col] <- i
         if (solver(board_su) == TRUE) {
             state <- TRUE
        } else {
          board_su[row,col] <- 0
          state <- FALSE
        }
      }
    }
  }
  state <- TRUE
  return(state)
}


validater <- function(board_su, num, pos=c(NULL,NULL)){
  status <- FALSE
  a <- as.integer(pos[1])
  b <- as.integer(pos[2])
  num <- as.integer(num)
  while (status == FALSE) {
    for (i in 1:9) {    
      if ((board_su[a,i] == num & b != i) == TRUE) {
        status <- FALSE
        return(status)
      }
    }
    
    for (i in 1:9) {    
      if ((board_su[i,b] == num & a != i) == TRUE) {
        status <- FALSE
        return(status)
      }
    }
    
    #which box are we in
    boxNx <- as.integer(ifelse(as.integer(b/3)==0, 1, as.integer(b/3)))
    boxNy <- as.integer(ifelse(as.integer(a/3)==0, 1, as.integer(a/3)))
    
    #looping through the box
    for (i in boxNy*3:(boxNy*3 + 3)) {
      for (j in  boxNx * 3 : (boxNx*3 + 3)) {
        if ((board_su[i, j] == num &  i != a & j != b) == TRUE){
          status <- FALSE
          return(status)
        }
      }
    } 
    status <- TRUE
    return(status)
  }
 # return(status)
}


#####################
### Start new Game
#####################

cat("Stepping into ....")
cat("Solution ________ ")

solver(sudoku)
sudoku

