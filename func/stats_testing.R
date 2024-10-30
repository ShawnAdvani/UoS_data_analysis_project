if (system.file(package='janitor')=="") {install.packages('janitor')}
library(janitor)
library(glue)

df_significance_testing <- function(df, dv, index) {
  print('Finding significant columns...')
  affective_columns <- c()
  if (class(df[[dv]])=='factor') {
    print('Dependent variable is categorical')
    for (i in colnames(df)) {
      if (i==index|i==dv|i=='dv') {
        next
      }
      if (i=='HUQ010') {
        print(i)
      }
      print(glue('Running test with {i} column...'))
      df_i <- df[c(dv, i)]
      colnames(df_i)[1:2] <- c('dv', 'iv')
      df_i <- df_i %>% filter(iv!='Missing')
      results <- df_run_chi_squared(df_i)
      if (class(results)!="htest") {
        next
      } else if (results$p.value<0.05) {
        print(results)
        affective_columns <- c(affective_columns, i)
      } else {
        next
      }
      Sys.sleep(.5)
    }
  } else if (class(df[[dv]])=='integer'|class(df[[dv]])=='numeric') {
    print('Dependent variable is numerical')
    for (i in colnames(df)) {
      if (i==index|i==dv|i=='dv') {
        next
      }
      print(glue('Running test with {i} column...'))
      if (identical(sort(unique(df[[i]])), c("Missing", "No", "Yes"))) {
        df_i <- df[c(dv, i)]
        colnames(df_i)[1:2] <- c('dv', 'iv')
        df_i <- df_i %>% filter(iv!='Missing')
        results <- df_run_mann_witney(df_i)
        if (class(results)!="htest") {
          print(class(results))
          next
        } else if (results$p.value<0.05) {
          # print(results)
          affective_columns <- c(affective_columns, i)
        }
      } else {
        df_i <- df[c(dv, i)]
        colnames(df_i)[1:2] <- c('dv', 'iv')
        df_i <- df_i %>% filter(iv!='Missing')
        results <- kruskal.test(df_i$iv, df_i$dv)
        if (class(results)!="htest") {
          print(class(results))
          next
        } else if (results$p.value<0.05) {
          print(results)
          affective_columns <- c(affective_columns, i)
        }
      }
    }
  } else {
    print(glue('unsupported dependent variable type: {class(df[[dv]]}'))
  }
  return(affective_columns)
}



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

df_run_mann_witney <- function(df) {
  df$iv <- recode(df$iv, "Yes"=1, 'No'=0)
  return(wilcox.test(df$iv, df$dv))
}

df_run_chi_squared <- function(df) {
  df_table <- table(df$dv, df$iv)
  # score_5s <- sum(as.numeric(df_table)<5)/length(as.numeric(df_table))
  # if (score_5s>.2) {
  #   return(NA) # TODO can add Fishers Exact Test
  # }
  results <- chisq.test(df_table)
  return(results)
}

