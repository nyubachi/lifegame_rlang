library(corrplot)
library(animation)

nrow <- 30
ncol <- 50
n <- nrow * ncol

field <- matrix(rbinom(n, 1, 0.35), nrow=nrow, ncol=ncol)
row_zero <- rep(0, ncol)
field_ex <- rbind(field, row_zero)

lifegame <- function() {
  corrplot(field, method="square", cl.pos="n", tl.pos="n")
  
  for (j in 1:20) {
    for (i in 1:n) {
      x <- ceiling(i / ncol)
      y <- i %% ncol
      
      c1 <- field_ex[x-1, y-1]
      c2 <- field_ex[x, y-1]
      c3 <- field_ex[x+1, y-1]
      c4 <- field_ex[x-1, y]
      c5 <- field_ex[x+1, y]
      c6 <- field_ex[x-1, y+1]
      c7 <- field_ex[x, y+1]
      c8 <- field_ex[x+1, y+1]
      
      cells <- c(c1, c2, c3, c4, c5, c6, c7, c8)
      sum_cell <- sum(cells==1)
      
      if (field[i]==0 && sum_cell>3) {
        field[i] <- 1
      } else if (field[i]==1 && sum_cell>=2 && sum_cell<=3) {
        field[i] <- 1
      } else if (field[i]==1 && sum_cell<=1) {
        field[i] <- 0
      } else if (field[i]==1 && sum_cell>=4) {
        field[i] <- 0
      }
    }
    
    field_ex <- rbind(field, row_zero)
    
    corrplot(field, method = "square", cl.pos = "n", tl.pos = "n")
  }
}

saveGIF(lifegame(), movie.name="lifegame.gif")
