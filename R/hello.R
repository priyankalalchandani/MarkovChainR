#' Hello World
#'
#' Basic hello world function to be called from the demo app
#'
#' @export
#' @param myname your name. Required.
hello <- function(myname = ""){
  #if(myname == ""){
  #  stop("Tell me your name!")
  #}
  #list(
    #message = paste("hello", myname, "! This is", R.Version()$version.string)
  #)

  ##### simple example #####
  # creating a data sample
  df1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'), conv = c(1, 0, 0), conv_null = c(0, 1, 1))

  # calculating the model
  mod1 <- markov_model(df1,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',
                       out_more = TRUE)

  # extracting the results of attribution
  df_res1 <- mod1$result

  list(
    message = paste(df_res1)
  )
}
