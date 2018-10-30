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
  mod1 <- ChannelAttribution :: markov_model(df1,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',
                       out_more = TRUE)

  # extracting the results of attribution
  df_res1 <- mod1$result

  list(
    message = paste(df_res1)
  )

  # extracting a transition matrix
  df_trans1 <- mod1$transition_matrix
  df_trans1 <- reshape2 :: dcast(df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

  ### plotting the Markov graph ###
  df_trans <- mod1$transition_matrix

  # adding dummies in order to plot the graph
  df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                         channel_to = c('(start)', '(conversion)', '(null)'),
                         transition_probability = c(0, 1, 1))
  df_trans <- rbind(df_trans, df_dummy)

  # ordering channels
  df_trans$channel_from <- factor(df_trans$channel_from,
                                  levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
  df_trans$channel_to <- factor(df_trans$channel_to,
                                levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
  df_trans <- reshape2 :: dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

  # creating the markovchain object
  trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                         nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                         dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))
  trans_matrix[is.na(trans_matrix)] <- 0
  trans_matrix1 <- new(markovchain :: "markovchain", transitionMatrix = trans_matrix)

  # plotting the graph
  list(
    message = paste(trans_matrix1)
  )
}
