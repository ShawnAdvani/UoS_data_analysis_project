if (system.file(package='janitor')=="") {install.packages('janitor')}
if (system.file(package='foreign')=="") {install.packages('foreign')}
if (system.file(package='MASS')=="") {install.packages('MASS')}
if (system.file(package='Hmisc')=="") {install.packages('Hmisc')}
if (system.file(package='reshape2')=="") {install.packages('reshape2')}

# import dependencies
library(janitor)
library(glue)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

# Function that automatically tests statistical significance based on data type
df_significance_testing <- function(df, dv, index) {
  print('Finding significant columns...')
  # Create empty list to return significant variables
  affective_columns <- c()
  if (class(df[[dv]])=='factor') {  #check for categorical dependent variable
    print('Dependent variable is categorical')
    for (i in colnames(df)) {
      # skips unnecessary columns
      if (i==index|i==dv|i=='dv') {
        next
      }
      print(glue('Running test with {i} column...'))
      # isolate dependent variable
      df_i <- df[c(dv, i)]
      colnames(df_i)[1:2] <- c('dv', 'iv')
      # filter missing data
      df_i <- df_i %>% filter(iv!='Missing')
      # run chi squared test on the data
      results <- df_run_chi_squared(df_i)
      # error filtering
      if (class(results)!="htest") {
        next
      # add columns as significant and skip insigificant columns based on p value
      } else if (results$p.value<0.05) {
        print(results)
        affective_columns <- c(affective_columns, i)
      } else {
        next
      }
    }
  } else if (class(df[[dv]])=='integer'|class(df[[dv]])=='numeric') { # check if dependent variable is numerical
    print('Dependent variable is numerical')
    for (i in colnames(df)) {
      # skip unnecessary columns
      if (i==index|i==dv|i=='dv') {
        next
      }
      print(glue('Running test with {i} column...'))
      if (identical(sort(unique(df[[i]])), c("Missing", "No", "Yes"))) {
        # isolate dependent variable
        df_i <- df[c(dv, i)]
        colnames(df_i)[1:2] <- c('dv', 'iv')
        # filter missing data
        df_i <- df_i %>% filter(iv!='Missing')
        # run mann witney test on the data
        results <- df_run_mann_witney(df_i)
        # error filtering
        if (class(results)!="htest") {
          print(class(results))
          next
        # add columns as significant and skip insigificant columns based on p value
        } else if (results$p.value<0.05) {
          affective_columns <- c(affective_columns, i)
        }
      } else {
        # isolate dependent variable
        df_i <- df[c(dv, i)]
        colnames(df_i)[1:2] <- c('dv', 'iv')
        # filter missing data
        df_i <- df_i %>% filter(iv!='Missing')
        # run kruskal wallis test on the data
        results <- kruskal.test(df_i$iv, df_i$dv)
        # error filtering
        if (class(results)!="htest") {
          print(class(results))
          next
        # add columns as significant and skip insigificant columns based on p value
        } else if (results$p.value<0.05) {
          print(results)
          affective_columns <- c(affective_columns, i)
        }
      }
    }
  } else {
    # Error handling for unsupported data types
    print(glue('unsupported dependent variable type: {class(df[[dv]]}'))
  }
  # return significant columns
  return(affective_columns)
}

# Ordinal logistic statistical testing function
olr_testing <- function(df, iv, dv) {
  # fit the data to the ordinal logistic regression model
  m <- polr(formula = as.formula(glue('{dv} ~ {paste(iv, collapse= "+")}')), data = df, Hess=TRUE)
  # display summary
  print(summary(m))
  # display coefficients
  (ctable <- coef(summary(m)))
  # calculate t statistic of variables
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  # calculate significance of variable 
  (ctable <- cbind(ctable, "p value" = p))
  # calcualte confidence intervals
  (ci <- confint(m))
  # calculate confidence intervals based on coefficients
  exp(coef(m))
  print(exp(cbind(OR = coef(m), ci)))
  
  # calculate logistic scores based on calculations
  (s <- with(df, summary(as.formula(glue('as.numeric({dv}) ~ {paste(iv, collapse= "+")}')), fun=sf)))
  # display and plot impact of logit calculations
  s[, 4] <- s[, 4] - s[, 3]
  s[, 3] <- s[, 3] - s[, 3]
  print(s)
  plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
  return(m)
}

# custom fuctions to format the data properly for the respective tests
df_run_mann_witney <- function(df) {
  # recode value names to numbers
  df$iv <- recode(df$iv, "Yes"=1, 'No'=0)
  return(wilcox.test(df$iv, df$dv))
}

df_run_chi_squared <- function(df) {
  # convert dataframe to table
  df_table <- table(df$dv, df$iv)
  results <- chisq.test(df_table)
  return(results)
}

# function to calculate odds at each variable being greater than or less than value
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

