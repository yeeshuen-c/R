#convert positions to indices(row,col)
to_location <- function(pos) {
  col <- match(substr(pos, 1, 1), letters)      #match first character to index in letters 
  row <- as.numeric(substr(pos, 2, nchar(pos))) #convert second character to numeric
  return(c(row, col))                           #position return as indices in vector
}

#check if queens attack each other
queen_attack <- function(p1, p2) {
  i1 <- to_location(p1)                           #store indices
  i2 <- to_location(p2)
  
  same_row <- i1[1] == i2[1]                      #check if queen on same row
  same_col <- i1[2] == i2[2]                      #check if queen on same column
  same_diag <- abs(i1[1] - i2[1]) == abs(i1[2] - i2[2]) #check if queen on same diag
  
  return(same_row || same_col || same_diag)       #return condition 
}

#process the solution and identify attacks
detect_attack <- function(solution, chessboard){    
  queen_attacks <- character()                    #store queen attacking details
  
  letters_vector <- letters[1:10]           # Get the first 10 letters: a to j
  sol <- paste0(letters_vector, solution)  # Combine letters with positions
  
  for (i in 1:(length(sol) - 1)) {
    for (j in (i + 1):length(sol)) {
      if (queen_attack(sol[i], sol[j])){    #comparing each queen to all remaining queen using nested loop
        queen_attacks <- c(queen_attacks, paste("Queens at", sol[i], "and", sol[j], "are attacking each other."))
      }
    }
  }
  
  if(length(queen_attacks) == 0){                           #no queen attack details stored 
    cat("\nFeasible solution for given positions \n\n")
  } else {
    cat("\nInfeasible solution for given positions \n\n")
  }
  
  print(chessboard[rev(row.names(chessboard)), colnames(chessboard)]) #output chessboard
  
  if(length(queen_attacks) > 0){
    cat("\nDescription of Queen Attacks:\n")              #output queen attack details
    cat(queen_attacks, sep = "\n")
  }else{                                                  #no queen attack details stored 
    cat("\nNo attacking queens.\n")
  }
}

#input vector
solution <- c(2,4,6,8,10,3,5,7,9,1)                           #modify to receive different input


#chessboard matrix
chessboard <- matrix(0, nrow = 10, ncol = 10)
rownames(chessboard) <- as.character(10:1)                      #row name as a to j
colnames(chessboard) <- letters[1:10]                           #col name as 1 to 10

#place queens on chessboard
for (i in 1:length(solution)){
  chessboard[solution[i], i] <- 1                             #set queen as 1,other as 0
}

#process solution and identify attacks
detect_attack(solution, chessboard)
