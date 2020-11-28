#' @title Create cyclic character sequence
#' @description Create cyclic character sequence
#' @param group_name A character vector as input
#' @param output_length The length of the output sequence
#' @return A cyclic character sequence with length `output_length`
#' @details Taken from
#' https://community.rstudio.com/t/fill-in-a-sequence-of-letters-based-on-a-given-order/88823/3
#' @examples
#' group_name <- c("red", "green", "blue")
#' create_char_seq(group_name, output_length = 2)
#' create_char_seq(group_name, output_length = 5)
#'
#' @rdname create_char_seq
#' @export
create_char_seq <- function(group_name, output_length) {

  # whole integer division
  i <- output_length %/% length(group_name)

  # remainder
  r <- output_length %% length(group_name)

  # Set the number of cycles needed
  if (r > 0) {
    t <- i + 1
  } else {
    t <- i
  }

  # Create the set and cut by the output_length
  set <- rep(group_name, t)
  output <- set[1:output_length]

  return(output)

}
