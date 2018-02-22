#' @title Generates a poor man's sampling distribution
#'
#' @param samples Specify how many samples to run
#' @param sample_size the number of obs in each sample
#' @param mean population mean
#' @param sd population sd
#'
#' @export

SamplingDist <- function(samples = 50, sample_size = 100, mean = 0, sd = 1){
  sampleMeans <- rep(NA, samples)
  for(i in 1:samples){
    x <- rnorm(sample_size, mean = mean, sd = sd)
    sampleMeans[i] = mean(x)
  }
  return(sampleMeans)
}


