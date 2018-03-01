#' @title Apply a filter to a data.frame
#'
#' @param df data.frame
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export

ggDensity <- function(df = mtcars, filter.var = 'cyl', filter.val = 6, variable = 'wt'){

  if('factor' %in% class(df[,filter.var])){
    Filter <- paste0(filter.var, " == '", filter.val, "'")
  } else {
    Filter <- paste0(filter.var, " == ", filter.val)
  }

  df2 <-
    df %>%
    filter_(Filter)

  ggplot(df2, aes_string(x = noquote(variable))) + geom_density()

}


