####################################
####
#### Sudoku with R
####
####  Created by Tomaz Kastrun
####  Date: January, 11, 2020
####  Version: 0.0.1
####


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


find_empty <- function(su){
  #empty df
  df <- data.frame(i=NULL,j=NULL)
    for (i in 1:nrow(su)){
    for (j in 1:ncol(su)){
      if (su[i,j] == 0) {
        a <- data.frame(i,j)
        names(a) <- c("i", "j")
        df <- rbind(df, a)
       } 
     }
    }
  return(df)
}



solver <- function(board_su){
  state <- FALSE
  while (state == FALSE) {
  find <- find_empty(board_su)
    print(find)
    if (length(find) == 0) {
      state <- TRUE
    } else {
      row <- find[2]
      col <- find[1]
    }
  }
 for (i in 1:10){ # numbers from 1... 9
  if (validater(board_su, i (row, col)) == TRUE){
    board_su[i,j] <- i
    
    if (solve(board_su) == FALSE) {
      state <- TRUE
    } else {
      board_su[i,j] <- 0
    }

  state <- FALSE
  }
 }
}


solver(sudoku)
find <- find_empty(sudoku)

validater <- function(board_su, num, pos){
  #while status <> FALSE
  status <- FALSE
  
  for (i in 1:length(board_su[,1])){
    if (board_su(pos[i,1]) == num & pos[1,] != i) {
      status <- FALSE
      return(status)
    }
  }
#}
  for (i in 1:length(board_su[1,])){
    if (board_su(pos[1,i]) == num & pos[,1] != i) {
      status <- FALSE
      return(status)
    }
  }

  box_x <- as.integer(pos[2]/3)
  box_y <- as.integer(pos[1]/3)
  
  for (i in box_y*3:(box_y*3 + 3)) {
    for (j in  box_x * 3 : (box_x*3 + 3)) {
    if (board_su[i,j] == num &  c(i,j) != pos){
      status <- FALSE
      return(status)
      }
    }
  }
  status <- TRUE
  return(status)
  
}


print_solution <- function(board_su){
  for (i in 1:length(board_su)){
    if (i %% 3 == 0 & i != 0)  {
      print("- - - - - - - - - - - - - ") }
    for (j in 1:length(board_su[1,])) { 
      
      if (j %% 3 == 0 & j != 0) {
        print(" | ") }
      
      if (j == 8) {
        print(board_su[i,j]) 
      } else {
        print(paste(board_su[i,j]), " ")
      }
    }
  }
}



#####################
### Start new Game
#####################



cat("Stepping into ....")
cat("Solution ________ ")

solver(sudoku)
print_solution(sudoku)
