symbols <- c("X", "O")
col_names <- c("A", "B", "C")
row_names <- c("1", "2", "3")
status_won <- 1
status_in_progress <- 2
status_stalemate <- 3

computer_play <- function(board, symbol) {
  # Reads the board and returns a choice of coordinates

  empty_spaces <- which(is.na(board), arr.ind = TRUE)

  # A stalemate should always be declared before reaching this point, but
  # just in case
  if (nrow(empty_spaces) == 0) {
    return("")
  }

  # Is there a move to make to win this round?
  for (i in seq(1, nrow(empty_spaces))) {
    row <- empty_spaces[i, 1]
    col <- empty_spaces[i, 2]
    input <- indices_to_coord(row, col)

    if (win_condition(add_to_board(board, symbol, input)) == status_won) {
      return(input)
    }
  }

  # Check if opponent can win
  for (i in seq(1, nrow(empty_spaces))) {
    row <- empty_spaces[i, 1]
    col <- empty_spaces[i, 2]
    input <- indices_to_coord(row, col)

    if (win_condition(add_to_board(board, next_turn(symbol), input)) ==
      status_won) {
      return(input)
    }
  }

  # If could not find a winning move then play a random available cell
  i <- runif(1, min = 1, max = nrow(empty_spaces))
  row <- empty_spaces[i, 1]
  col <- empty_spaces[i, 2]
  return(indices_to_coord(row, col))
}

indices_to_coord <- function(row, col) {
  return(paste0(col_names[col], row_names[row]))
}

win_condition <- function(board) {
  # a win conditions matrix, in reality there are 8 win conditions:
  # 3 rows, 3 columns and 2 diagonals. The extra cell in the matrix
  # is filled by F
  cond <- matrix(nrow = 3, ncol = 3)
  # rows
  cond[1, ] <- apply(board, 1, function(x) {
    unique(x)[1] %in% symbols && length(unique(x)) == 1
  })
  # columns
  cond[2, ] <- apply(board, 2, function(x) {
    unique(x)[1] %in% symbols && length(unique(x)) == 1
  })
  # diagonals
  dboard <- rbind(diag(board), diag(board[, order(seq(ncol(board), 1))]))
  # only 2 diagonals so fill with FALSE
  cond[3, ] <- c(
    apply(dboard, 1, function(x) {
      unique(x)[1] %in% symbols && length(unique(x)) == 1
    }),
    FALSE
  )
  # return 1 if any of the win conditions are met
  if (any(cond)) {
    return(status_won)
  } else {
    # if there are empty cells then return 2
    if (any(is.na(board))) {
      return(status_in_progress)
    } else {
      # 3 means stalemate
      return(status_stalemate)
    }
  }
}

process_input <- function(input) {
  # accepts input as an spreadsheet type of coordinate (e.g. A1 or B3)
  # and returns a vector containing row, col numbers or NA if input is invalid

  input <- toupper(input)

  # what happens here is that variables row, col are only set if they belong to
  # row_names or col_names respectively, if not, they will be an empty integer
  # and wont be added to resp vector by c()
  row <- which(row_names == substring(input, 2))
  col <- which(col_names == substring(input, 1, 1))

  resp <- as.numeric(c(row, col))

  # if resp is not exactly 2 elements then user input is invalid
  if (length(resp) != 2) {
    return(NA)
  }

  return(resp)
}

add_to_board <- function(board, symbol, input) {
  symbol <- toupper(symbol)

  # check if we are adding a valid symbol (i.e. X or O)
  if (!(symbol %in% symbols)) {
    return(0)
  }

  # check if user input is valid (if not then coordinates == NA)
  coordinates <- process_input(input)
  if (anyNA(coordinates)) {
    return(0)
  }

  # check if that coordinate is empty (i.e. NA)
  if (!is.na(board[coordinates[1], coordinates[2]])) {
    return(0)
  }

  board[coordinates[1], coordinates[2]] <- symbol

  return(board)
}

reset_board <- function() {
  matrix(
    nrow = 3,
    ncol = 3,
    dimnames = list(row_names, col_names)
  )
}

next_turn <- function(turn) {
  ifelse(turn == symbols[1], symbols[2], symbols[1])
}

check_quit <- function(input) {
  if (input == "Q") {
    cat("Oops, game has been halted.\n")
    return(TRUE)
  }
  return(FALSE)
}

pick_symbol <- function(con) {
  player_symbol <- character(1)

  while (!(player_symbol %in% symbols)) {
    cat(paste0("Player please choose your symbol (X/O): "))
    player_symbol <- toupper(readLines(con = con, n = 1))

    if (check_quit(player_symbol)) {
      break
    }
  }

  return(player_symbol)
}

start_game <- function(con) {
  board <- reset_board()
  input <- numeric(1)
  turn <- symbols[1]

  print_board(board)

  player_symbol <- pick_symbol(con)


  while (player_symbol %in% symbols) {
    new_board <- numeric(1)

    if (turn != player_symbol) {
      # AI turn, computer plays
      play <- computer_play(board, turn)
      if (play == "") {
        cat("Oops, I cannot play a tile, this shouldn't have happened.", "\n")
        break
      }
      new_board <- add_to_board(board, turn, play)
    } else {
      # Player turn
      cat(paste0("Player (", turn, ") enter coordinates (e.g. A3): "))
      input <- toupper(readLines(con = con, n = 1))

      if (check_quit(input)) {
        break
      }
      new_board <- add_to_board(board, turn, input)
    }

    if (length(new_board) == 1) {
      # Length of new_board is only 1 if player input is invalid
      cat("User input is invalid, try again", sep = "\n")
    } else {
      # If player input is valid then advance the game
      board <- new_board
      print_board(board)
      # Did someone win?
      game_status <- win_condition(board)

      if (is_game_finished(game_status, turn, player_symbol)) {
        break
      }

      turn <- next_turn(turn)
    }
  }
}

is_game_finished <- function(game_status, turn, player_symbol) {
  if (game_status == status_won) {
    if (turn == player_symbol) {
      cat(paste0("Congratulations player ", turn, ". You won!"))
    } else {
      cat(paste0("The computer won this game, better luck next time."))
    }
    cat("\n")
    return(TRUE)
  } else if (game_status == status_stalemate) {
    cat(paste0("Stalemate. I declare a draw."), sep = "\n")
    return(TRUE)
  }

  return(FALSE)
}

print_board <- function(board) {
  cat("\014")
  cat("(Enter 'Q' at anytime to quit the game)", sep = "\n")
  board[is.na(board)] <- "_"
  print(as.data.frame(board))
}



init <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  start_game(con)
  cat("Do you want to play again? [y/N]")
  again <- toupper(readLines(con = con, n = 1))

  if (again == "Y") {
    init()
  } else {
    cat("Thank you for playing. Bye.", sep = "\n")
  }
}


if (!exists("loaded_test") || (loaded_test != "tictactoe")) {
  # This is my adaptation of python's if __name__ == "__main__":
  # I do this in order to keep functions and the executable in a single file,
  # while being able to load the source for unit testing
  init()
}
