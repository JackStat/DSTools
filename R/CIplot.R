#' @title Generates a confidence limit visualization
#'
#' @param sample_size the number of obs in each sample
#' @param cl confidence limit
#'
#' @export



CIplot <- function(sample_size = 100, cl = .95){
  plot(NULL
     , xlim = c(0 - 1, 0 + 1)
     , ylim = c(0, 100)
     , yaxt = 'n'
     , xlab = "Confidence Level"
     , ylab = "Samples"
     )

  abline(v = 0, col = 'black')
  mtext(expression(mu), cex = 2, at = 0)

  for(i in 1:100){
    x = rnorm(sample_size)
    TT <- t.test(x, conf.level = cl)
    TT$conf.int

    interval1 = TT$conf.int[1]
    interval2 = TT$conf.int[2]
    if(interval1 < 0 & interval2 > 0){
      lines(c(interval1,interval2), c(i,i), lwd=2, col='black')
    } else{
      lines(c(interval1,interval2), c(i,i), lwd=2, col='red')
    }
  }

}

