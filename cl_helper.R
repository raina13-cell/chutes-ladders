show_board <- function(board){
  plot.new()
  rdim <- board[["dim"]][1]
  cdim <-board[["dim"]][2]
  plot.window(xlim = c(0,cdim), ylim = c(0,rdim), asp = 1)
  num_to_coord <- function(num){
    vec_y <- (num-1) %/% cdim
    if(vec_y %% 2 == 0){
      vec_x <- (num-1) %% cdim
    }
    else{
      vec_x <- (num-1) %% cdim
      vec_x <- cdim-1-vec_x
    }
    c(vec_x + 0.5,vec_y +0.5)
  }
  for(row in 0:rdim){
    segments(x0 = 0, y0 = row, x1 = cdim, y1 = row)
  }
  for(col in 0:cdim){
    segments(x0 = col, y0 = 0, x1 = col, y1 = rdim)
  }
  counter <- 1
  for(row in 0:(rdim-1)){
    if(row %% 2 == 0)
    {
      for(col in 0:(cdim-1)){
        text(col + 0.5, row + 0.5, labels = counter, cex = 0.5)
        counter <- counter + 1
      }
    }
    else{
      for(col in (cdim-1):0){
        text(col + 0.5, row + 0.5, labels = counter, cex = 0.5)
        counter <- counter + 1
      }
    }
  }
  if (!is.null(board[["ladders"]][["start"]]) && !is.null(board[["chutes"]][["end"]])) {
    all_start <- c(board[["ladders"]]["start"][,1], board[["chutes"]]["start"][,1])
    all_end <- c(board[["ladders"]]["end"][,1], board[["chutes"]]["end"][,1])
    start_coords <- vapply(all_start, num_to_coord, numeric(2))
    end_coords <- vapply(all_end, num_to_coord, numeric(2))
    coords <- rbind(start_coords, end_coords)
    draw_arrows <- function(coord){
      color = "green"
      if(coord[4] < coord[2]){
        color = "red"
      }
      else if(coord[4] == coord[2]){
        if((coord[4] - 0.5) / 2 == 0){
          if(coord[1] > coord[3]){
            color = "red"
          }
        }
        else{
          if(coord[3] > coord[1]){
            color = "red"
          }
        }
      }
      arrows(x0 = coord[1], y0 = coord[2], x1 = coord[3], y1 = coord[4], col = color, length = 0.1, code = 2, lwd = 2,)
    }
    apply(coords, 2, draw_arrows)
  }
}
play_solo <- function(board, verbose = FALSE){
  turn <- 0
  position <- 0
  spinner <- numeric(1)
  output <- list(turns = numeric(1), chute_tally = numeric(dim(board[["chutes"]])[1]), ladder_tally = numeric(c(dim(board[["ladders"]])[1])), move_log = c())
  while(position != 100){
    turn <- turn + 1
    spinner <- sample(1:6, 1)
    if(verbose){
      cat("\nTurn ", turn)
      cat("\nStart at ", position)
      cat("\nSpinner: ", spinner)
    }
    if(position + spinner <= 100){
      position <- position + spinner
    }
    if(position %in% board[["ladders"]][["start"]]){
      if(verbose) cat("\nLanded on: ", position, "\nLadder!")
      output[["ladder_tally"]][which(position == board[["ladders"]][["start"]])] <- output[["ladder_tally"]][which(position == board[["ladders"]][["start"]])] + 1
      position <- board[["ladders"]][["end"]][which(position == board[["ladders"]][["start"]])]
    }
    if(position %in% board[["chutes"]][["start"]]){
      if(verbose) cat("\nLanded on: ", position, "\nChute!")
      output[["chute_tally"]][which(position == board[["chutes"]][["start"]])] <- output[["chute_tally"]][which(position == board[["chutes"]][["start"]])] + 1
      position <- board[["chutes"]][["end"]][which(position == board[["chutes"]][["start"]])]
    }
    output[["move_log"]][turn] <- position
    if(verbose) cat("\nTurn ends at: ", position, "\n")
  }
  output[["turns"]] <- turn
  output
}
heat_map <- function(board, moves){
  plot.new()
  rdim <- board[["dim"]][1]
  cdim <-board[["dim"]][2]
  plot.window(xlim = c(0,cdim), ylim = c(0,rdim), asp = 1)
  num_to_coord <- function(num){
    vec_y <- (num-1) %/% cdim
    if(vec_y %% 2 == 0){
      vec_x <- (num-1) %% cdim
    }
    else{
      vec_x <- (num-1) %% cdim
      vec_x <- cdim-1-vec_x
    }
    c(vec_x + 0.5,vec_y +0.5)
  }
  squares <- as.numeric(apply(moves, 1, function(x) x[1]))
  dens <- as.numeric(apply(moves, 1, function(x) x[2]))/10
  sq_coords <- vapply(squares, num_to_coord, numeric(2)) - 0.5
  rect(xleft = sq_coords[1,1:length(squares)], ybottom = sq_coords[2,1:length(squares)], xright = sq_coords[1,1:length(squares)]+1, 
       ytop = sq_coords[2,1:length(squares)] +1, col = "darkred", density = dens)
  for(row in 0:rdim){
    segments(x0 = 0, y0 = row, x1 = cdim, y1 = row)
  }
  for(col in 0:cdim){
    segments(x0 = col, y0 = 0, x1 = col, y1 = rdim)
  }
  counter <- 1
  for(row in 0:(rdim-1)){
    if(row %% 2 == 0)
    {
      for(col in 0:(cdim-1)){
        text(col + 0.5, row + 0.5, labels = counter, cex = 0.5)
        counter <- counter + 1
      }
    }
    else{
      for(col in (cdim-1):0){
        text(col + 0.5, row + 0.5, labels = counter, cex = 0.5)
        counter <- counter + 1
      }
    }
  }
  all_start <- c(board[["ladders"]]["start"][,1], board[["chutes"]]["start"][,1])
  all_end <- c(board[["ladders"]]["end"][,1], board[["chutes"]]["end"][,1])
  start_coords <- vapply(all_start, num_to_coord, numeric(2))
  end_coords <- vapply(all_end, num_to_coord, numeric(2))
  coords <- rbind(start_coords, end_coords)
  draw_arrows <- function(coord){
    color = "green"
    if(coord[4] < coord[2]){
      color = "red"
    }
    else if(coord[4] == coord[2]){
      if((coord[4] - 0.5) / 2 == 0){
        if(coord[1] > coord[3]){
          color = "red"
        }
      }
      else{
        if(coord[3] > coord[1]){
          color = "red"
        }
      }
    }
    arrows(x0 = coord[1], y0 = coord[2], x1 = coord[3], y1 = coord[4], col = color, length = 0.1, code = 2, lwd = 2,)
  }
  apply(coords, 2, draw_arrows)
  
}
heatMap_moves <- function(num){
  moves <- sapply(mc10000["move_log",], function(x) x[num])
  completed <- sum(moves == 100, na.rm = TRUE)
  cat(sum(is.na(moves), completed), "out of 10000 games completed")
  moves <- as.data.frame(table(moves))
  par(mar = c(0, 0, 0, 0))
  heat_map(board, moves)
}