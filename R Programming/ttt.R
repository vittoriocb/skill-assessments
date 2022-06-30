
SYMBOLS = c("X","O")
COL_NAMES = c("A","B","C")
ROW_NAMES = c("1","2","3")
STATUS.WON = 1
STATUS.INPROGRESS = 2
STATUS.STALEMATE = 3

win_condition <- function(board){
  # a win conditions matrix, in reality there are 8 win conditions:
  # 3 rows, 3 columns and 2 diagonals. The extra cell in the matrix is filled by F
  cond = matrix(nrow=3, ncol=3)
  
  # rows
  cond[1,] = apply(board, 1, function(x) unique(x)[1] %in% SYMBOLS && length(unique(x))==1)
  # columns
  cond[2,] = apply(board, 2, function(x) unique(x)[1] %in% SYMBOLS && length(unique(x))==1)
  
  # diagonals
  dboard = rbind(diag(board), diag(board[,order(ncol(board):1)]))
  # only 2 diagonals so fill with FALSE
  cond[3,] = c(apply(dboard, 1, function(x) unique(x)[1] %in% SYMBOLS && length(unique(x))==1), F)
  
  # return 1 if any of the win conditions are met
  if(any(cond == T)) {
    return(STATUS.WON)
  }
  else {
    # if there are empty cells then return 2
    if (any(is.na(board))){
      return(STATUS.INPROGRESS)
    } 
    else {
      # 3 means stalemate
      return(STATUS.STALEMATE)
    }
  }
}

process_input <- function(input) {
  # accepts input as an spreadsheet type of coordinate (e.g. A1 or B3) 
  # and returns a vector containing row, col numbers or NA if input is invalid
  
  input = toupper(input)
  
  # what happens here is that variables row, col are only set if they belong to 
  # ROW_NAMES or COL_NAMES respectively, if not, they will be an empty integer
  # and wont be added to resp vector by c()
  row = which(ROW_NAMES == substring(input,2))
  col = which(COL_NAMES == substring(input,1,1))
  
  resp = as.numeric(c(row,col))
  
  # if resp is not exactly 2 elements then user input is invalid
  if (length(resp) != 2) return(NA)
  
  return(resp)
}

add_to_board <- function(board, symbol, input){
  symbol = toupper(symbol)
  
  # check if we are adding a valid symbol (i.e. X or O)
  if (!(symbol %in% SYMBOLS)) return(0)
  
  # check if user input is valid (if not then coordinates == NA)
  coordinates = process_input(input)
  if (anyNA(coordinates)) return(0)
  
  # check if that coordinate is empty (i.e. NA)
  if (!is.na(board[coordinates[1], coordinates[2]])) return(0)
  
  board[coordinates[1], coordinates[2]] = symbol
  
  return(board)
}

reset_board <- function(){
  matrix(nrow = 3, ncol = 3, dimnames=list(ROW_NAMES, COL_NAMES))
}

next_turn <- function(turn){
  ifelse(turn == SYMBOLS[1], SYMBOLS[2], SYMBOLS[1])
}

start_game <- function(con){
  board = reset_board()
  input = numeric(1)
  turn = SYMBOLS[1]
  
  print_board(board)
  
  while (T) {
    cat(paste0("Player (",turn,") enter coordinates (e.g. A3): "))
    input <- toupper(readLines(con=con,n=1))
    
    if (input == "Q") {
      cat("Oops, game has been halted.",sep="\n")
      break;
    }
    new_board = add_to_board(board, turn, input)
    
    if (length(new_board) == 1){
      cat("User input is invalid, try again",sep="\n")
    }
    else {
      board = new_board
      # did player turn win?
      win = win_condition(board)

      print_board(board)
      
      if (win == STATUS.WON) {
        cat(paste0("Congratulations player ", turn, ". You won!"),sep="\n")
        break;
      }
      else if (win == STATUS.STALEMATE) {
        cat(paste0("Stalemate. I declare a draw."),sep="\n")
        break;
      }
      
      turn = next_turn(turn)
    }
  }  
}

print_board <- function(board) {
  cat("\014")
  cat("(Enter 'Q' at anytime to quit the game)",sep="\n")
  board[is.na(board)] = "_"
  print(as.data.frame(board))
}

init <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }  
  start_game(con)
  
  cat("Do you want to play again? [y/N]")
  again <- toupper(readLines(con=con,n=1))
  
  if (again == "Y"){
    init()
  } else {
    cat("Thank you for playing. Bye.",sep="\n")
  }
}


if (!exists("LOADED_TEST") || (LOADED_TEST != "tictactoe")) {
  init()
}











