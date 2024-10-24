if (system.file(package='janitor')=="") {install.packages('janitor')}

library(janitor)

# custom fuction to convert df into a matrix/table to be processed into chisq.test() function
df_to_chisq <- function(df) {
  # establish relevant lists to be filled
  data_rows <- c()
  col_names <- c()
  row_names <- c()
  # loop through column names
  for (i in colnames(df)) {
    # find all numeric data
    if (class(df[[i]])=='integer'|class(df[[i]])=='numeric') {
      # create comprehensive list of values
      data_rows <- c(data_rows, df[[i]])
      row_names <- c(row_names, i)
    } else {
      # identify and save row names as columns
      col_names <- df[[i]]
    }
  }
  # convert data list into a matric
  df_to_mat <- matrix(data_rows, ncol = length(col_names), byrow = TRUE)
  # add column names to matrix
  colnames(df_to_mat) <- col_names
  # and rownames to matrix
  rownames(df_to_mat) <- row_names
  print(df_to_mat)
  # convert matrix to table and run chi sqr on it
  results <- chisq.test(as.table(df_to_mat))
  return(results)
}