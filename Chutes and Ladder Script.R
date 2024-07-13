### Part 2
show_board <- function(board) {
  # Create a board with right dimensions
  plot.new()
  plot.window(xlim = c(0,board[[1]][1]), ylim = c(0,board[[1]][2]), asp = 1)
  # Vertical lines
  for (i in seq(0, board[[1]][1], by = 1)) {
    segments(x0 = i, y0 = 0, x1 = i, y1 = board[[1]][2])
  }
  # Horizontal Lines
  for(i in seq(0, board[[1]][2], by = 1)) {
    segments (x0 = 0, y0 = i, x1 = board[[1]][1], y1 = i)
  }
  
  # Total number of labels 
  label <- c(1:(board[[1]][1] * board[[1]][2]))
  # Reorder numbers 
  i <- 1
  while(i < board[[1]][2]) {
    label[seq((i * board[[1]][1]) + 1, (i + 1)* board[[1]][1], 1)] <- rev(seq((i * board[[1]][1]) + 1, (i + 1)* board[[1]][1], 1))
    i <- i + 2
  }
  # Create a list to store a location for each label
  number <- list(label)
  j = 1
  for (y in 0.5:(board[[1]][2] - 0.5)) {
    for (x in 0.5:(board[[1]][1] - 0.5)) {
      number[[label[j]]] <- c(x, y)
      text(x, y, label[j])
      j <- j + 1
    }
  }
  
  # Create ladders
  ladder <- board[[2]]
  for(i in seq_along(ladder)) {
    from_ladder <- ladder[[i]][1]
    to_ladder <- ladder[[i]][2]
    arrows(number[[from_ladder]][1], number[[from_ladder]][2], number[[to_ladder]][1], number[[to_ladder]][2], col = "green", lwd = 2)
  }
  
  # Create chutes
  chute <- board[[3]]
  for(i in seq_along(chute)) {
    from_chute <- chute[[i]][1]
    to_chute <- chute[[i]][2]
    arrows(number[[from_chute]][1], number[[from_chute]][2], number[[to_chute]][1], number[[to_chute]][2], col = "red", lwd = 2)
  }
}



### Part 4
play_solo <- function(board, verbose = FALSE) {
  turns <- 1
  chute_tally <- numeric(length(board[[3]]))
  ladder_tally <- numeric(length(board[[2]]))
  move_log <- numeric(40)
  position <- 0 # Keep track of position
  
  # Spin function
  spin <- function() { 
    sample(6, 1)
  }
  
  # Get the start and end number of ladder 
  ladder_len <- length(board[[2]])
  ladder_start <- numeric(ladder_len)
  ladder_end <- numeric(ladder_len)
  for(i in seq_len(ladder_len)) {
    ladder_start[i] <- board[[2]][[i]][1]
    ladder_end[i] <- board[[2]][[i]][2]
  }
  # Get the start and end number of chute
  chute_len <- length(board[[3]])
  chute_start <- numeric(chute_len)
  chute_end <- numeric(chute_len)
  for(i in seq_len(chute_len)) {
    chute_start[i] <- board[[3]][[i]][1]
    chute_end[i] <- board[[3]][[i]][2]
  }
  
  # Roll a die
  repeat {
    ladder_logical <- FALSE
    chute_logical <- FALSE
    if(turns == 1) {
      result <- spin()
      move_log[turns] <- result
    }
    else {
      result <- spin()
      move_log[turns] <- move_log[turns-1] + result
    }
    
    if (move_log[turns] > 100) {
      move_log[turns] <- move_log[turns-1]
    }
    else {
      # Check for ladder
      if(any(ladder_start %in% move_log[turns])) {
        indice <- which(ladder_start %in% move_log[turns])
        move_log[turns] <- ladder_end[indice]
        ladder_tally[indice] <- ladder_tally[indice] + 1
        ladder_logical <- TRUE
      }
      # Check for chute
      if(any(chute_start %in% move_log[turns])) {
        indice <- which(chute_start %in% move_log[turns])
        move_log[turns] <- chute_end[indice]
        chute_tally[indice] <- chute_tally[indice] + 1
        chute_logical <- TRUE
      }
      
      if(verbose == TRUE) {
        cat("Turn", turns,"\n")
        cat("Start at", position,"\n")
        cat("Spinner:", result,"\n")
        # Special cases
        if (ladder_logical) {
          cat("Landed on:", position + result,"\n")
          cat("Ladder!\n")
        }
        if (chute_logical) {
          cat("Landed on:", position + result,"\n")
          cat("Chute!\n")
        }
        cat("Turn ends at:", move_log[turns],"\n\n")
      }
    }
    
    position <- move_log[turns]
    turns <- turns + 1
    
    if(position == 100) {
      turns <- turns -1
      break
    }
  }
  list_end <- list("turns" = turns, "chute_tally" = chute_tally, "ladder_tally" = ladder_tally, "move_log" = move_log[1:turns])
  list_end
}