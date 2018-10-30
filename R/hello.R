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
  df1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'),
                    conv = c(2, 0,1),
                   conv_null = c(0, 1, 0))

  #read_csv <- read.csv("C:\\Users\\user\\Documents\\inputFile.csv", stringsAsFactors = FALSE, header = TRUE)
  #df1 <- tbl_df(read_csv)
  df1 <- aggregate(x = df1[2:3], by = list(df1$path), FUN = sum)
  names(df1) <- c("path", "conv", "conv_null")
  df1$path <- gsub(" > ",">",df1$path)
  df1$path <- gsub(" ","_",df1$path)
  df1$path <- gsub(">"," > ",df1$path)


  # calculating the model
  mod1 <- markov_model(df1,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',
                       out_more = TRUE)

  # extracting the results of attribution
  df_res1 <- mod1$result
  df_res1$total_conversions <- df_res1$total_conversions/sum(df_res1$total_conversions)

  list(
    message = paste(df_res1$total_conversions)
  )
}
