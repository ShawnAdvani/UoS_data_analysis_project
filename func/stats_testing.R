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
df_run_mann_witney <- function(df) {
  df$iv <- recode(df$iv, "Yes"=1, 'No'=0)
  return(wilcox.test(df$iv, df$dv))
}

df_run_chi_squared <- function(df) {
  df_table <- table(df$dv, df$iv)
  results <- chisq.test(df_table)
  return(results)
}

