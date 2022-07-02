library(testthat)
loaded_test <- "tictactoe"
source("ttt.R")
loaded_test <- numeric(1)


test_that("next_turn", {
  expect_equal(next_turn("X"), "O")
  expect_equal(next_turn("O"), "X")
})

test_that("process_input", {
  expect_equal(process_input("a1"), c(1, 1))
  expect_equal(process_input("a3"), c(3, 1))
  expect_equal(process_input("C1"), c(1, 3))
  expect_equal(process_input("B2"), c(2, 2))
  expect_equal(process_input(NA), NA)
  expect_equal(process_input(""), NA)
  expect_equal(process_input(1), NA)
  expect_equal(process_input("a0"), NA)
  expect_equal(process_input("a4"), NA)
  expect_equal(process_input("asdf"), NA)
})

test_that("add_to_board", {
  expect_equal(add_to_board(matrix(nrow = 3, ncol = 3), "y", "a1"), 0)
  expect_equal(add_to_board(matrix(nrow = 3, ncol = 3), "X", NA), 0)
  expect_equal(add_to_board(
    rbind(
      c(NA, NA, NA),
      c(NA, "X", NA),
      c(NA, NA, NA)
    ),
    "X", "b2"
  ), 0)
  expect_equal(add_to_board(matrix(nrow = 3, ncol = 3), "X", "b2"), rbind(
    c(NA, NA, NA),
    c(NA, "X", NA),
    c(NA, NA, NA)
  ))
})

test_that("win_condition", {
  board <- rbind(
    c("X", "X", "O"),
    c("O", "X", "X"),
    c("O", "O", "X")
  )
  expect_equal(win_condition(board), status_won)

  board <- rbind(
    c("X", "O", "O"),
    c("O", "O", "X"),
    c("X", "O", "X")
  )
  expect_equal(win_condition(board), status_won)

  board <- rbind(
    c("X", "O", "O"),
    c("O", "O", "X"),
    c("X", "X", "X")
  )
  expect_equal(win_condition(board), status_won)

  board <- rbind(
    c("X", "X", "O"),
    c("O", "O", "X"),
    c("O", "O", "X")
  )
  expect_equal(win_condition(board), status_won)

  board <- rbind(
    c(NA, NA, NA),
    c("O", "O", NA),
    c("X", "X", "X")
  )
  expect_equal(win_condition(board), status_won)

  board <- rbind(
    c("X", NA, NA),
    c("O", "O", "X"),
    c("X", "O", "X")
  )
  expect_equal(win_condition(board), status_in_progress)

  board <- rbind(
    c("X", "X", "O"),
    c("O", "O", "X"),
    c("X", "O", "X")
  )
  expect_equal(win_condition(board), status_stalemate)
  
  board <- rbind(
    c("X",  "X", "O"),
    c(NA,   NA, "O"),
    c("X", "O", "X")
  )
  expect_equal(win_condition(board), status_in_progress)
})

test_that("indices_to_coord", {
  expect_equal(indices_to_coord(1,3), "C1")
  expect_equal(indices_to_coord(3,1), "A3")
})


test_that("computer_play", {
  board <- rbind(
    c("X", "O",  NA),
    c(NA,  NA, "O"),
    c("X", "O", "X")
  )
  expect_equal(computer_play(board, "X"), "A2")
  
  board <- rbind(
    c("X",  NA, "O"),
    c(NA,   NA, "O"),
    c("X", "O", NA)
  )
  expect_equal(computer_play(board, "O"), "C3")
  
  board <- rbind(
    c("X", "O", NA),
    c(NA, NA, "O"),
    c(NA, "O", "X")
  )
  expect_equal(computer_play(board, "X"), "B2")
  
  
  board <- rbind(
    c("X", "X", NA),
    c(NA, NA, "O"),
    c(NA, NA, NA)
  )
  expect_equal(computer_play(board, "O"), "C1")
  
})