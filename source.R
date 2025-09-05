# Eli Radparvar Script for HW2
# UID:206097081

#show board function
show_board <- function(board) {
  # Extract board properties
  n_rows <- board$n_rows
  n_cols <- board$n_cols
  chutes <- board$chutes
  ladders <- board$ladders
  
  # Creates the plot
  plot.new()
  plot.window(xlim = c(0 , n_cols), ylim = c(0, n_rows), asp = 1)
  for(r in 0:n_rows){
    segments(x0 = 0, y0 = r, x1 = n_cols, y1 = r)
  }
  for(c in 0:n_cols){
    segments(x0 = c, y0 = 0, x1 = c, y1 = n_rows)
  }
  
  # Adds numbers to each square
  counter <- 1
  for (row in 0:(n_rows - 1)) {
    if (row %% 2 == 0) {
      # even rows left to right
      for (col in 0:(n_cols - 1)) {
        text(x = col + 0.5, y = row + 0.5, labels = counter, cex = 0.7)
        counter <- counter + 1
      }
    } else {
      # odd rows right to left
      for (col in (n_cols - 1):0) {
        text(x = col + 0.5, y = row + 0.5, labels = counter, cex = 0.7)
        counter <- counter + 1
      }
    }
  }
  
  # Draws chutes (red arrows)
  for (start in names(chutes)) {
    end <- chutes[[start]]
    start <- as.numeric(start)
    
    # Calculates start row/column
    start_row <- (start - 1) %/% n_cols
    start_col <- ifelse(start_row %% 2 == 0,
                        (start - 1) %% n_cols,
                        (n_cols - 1) - (start - 1) %% n_cols)
    
    # Calculates end row/column
    end_row <- (end - 1) %/% n_cols
    end_col <- ifelse(end_row %% 2 == 0,
                      (end - 1) %% n_cols,
                      (n_cols - 1) - (end - 1) %% n_cols)
    
    # Draws arrow
    arrows(x0 = start_col + 0.5, y0 = start_row + 0.5,
           x1 = end_col + 0.5, y1 = end_row + 0.5,
           col = rgb(1, 0, 0, alpha = 0.5), lwd = 2, length = 0.1)
  }
  
  # Draws ladders (green arrows)
  for (start in names(ladders)) {
    end <- ladders[[start]]
    start <- as.numeric(start)
    
    # Calculates start row/column
    start_row <- (start - 1) %/% n_cols
    start_col <- ifelse(start_row %% 2 == 0,
                        (start - 1) %% n_cols,
                        (n_cols - 1) - (start - 1) %% n_cols)
    
    # Calculates end row/column
    end_row <- (end - 1) %/% n_cols
    end_col <- ifelse(end_row %% 2 == 0,
                      (end - 1) %% n_cols,
                      (n_cols - 1) - (end - 1) %% n_cols)
    
    # Draws arrow
    arrows(x0 = start_col + 0.5, y0 = start_row + 0.5,
           x1 = end_col + 0.5, y1 = end_row + 0.5,
           col = rgb(0, 1, 0, alpha = 0.5) , lwd = 2, length = 0.1)
  }
}



# play solo function
play_solo <- function(board, verbose = FALSE) {
  position <- 0
  turns <- 0
  chute_tally <- numeric(length(board$chutes))
  ladder_tally <- numeric(length(board$ladders))
  move_log <- c()
  
  # Loops while the position is not 100
  while (position != 100) {
    
    turns <- turns + 1
    if (verbose){
      cat("Turn", turns, "\n")
    }
    
    # Calculates starting position
    if (verbose){
      cat("Start at", position, "\n")
    }
    move_log <- c(move_log, position)
    
    # Spinner
    roll <- spin()
    if (verbose){
      cat("Spinner:", roll, "\n")
    }
    
    # Calculates new position
    new_position <- position + roll
    
    # Checks for position over 100
    if (new_position > 100) {
      new_position <- position
    }
    
    # Checks for chutes or ladders
    if (new_position %in% names(board$chutes)) {
      chute_index <- which(names(board$chutes) == as.character(new_position))
      new_position <- board$chutes[[as.character(new_position)]]
      chute_tally[chute_index] <- chute_tally[chute_index] + 1
      if (verbose){
        cat("Landed on:", position + roll, "\n")
        cat("Chute!\n")
      }
    } else if (new_position %in% names(board$ladders)) {
      ladder_index <- which(names(board$ladders) == as.character(new_position))
      new_position <- board$ladders[[as.character(new_position)]]
      ladder_tally[ladder_index] <- ladder_tally[ladder_index] + 1
      if (verbose){
        cat("Landed on:", position + roll,"\n")
        cat("Ladder!\n")
      }
    }
    
    # Moves position
    position <- new_position
    if (verbose){
      cat("Turn ends at:", position, "\n\n")
    }
  }
  
  # Outputs resulting list
  list(
    turns = turns,
    chute_tally = chute_tally,
    ladder_tally = ladder_tally,
    move_log = c(move_log, position)[-1]
  )
}
