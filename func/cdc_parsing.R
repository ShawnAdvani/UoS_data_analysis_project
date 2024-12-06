# check if necessary packages are installed
if (system.file(package='rvest')=="") {install.packages('rvest')}
if (system.file(package='xml2')=="") install.packages('xml2')

# import all dependencies
library(haven)
library(rvest)
library(stringr)
library(glue)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

# custom function for parsing data (takes cdc file format xpt and documentation from cdc website)
df_parser <- function(data_file, metadata) {
  # print file name
  print(str_sub(data_file, start = 6, end = -5))
  # load in file
  df_raw <- read_xpt(data_file)
  # clean all rows and columns that only contain NA
  df <- df_raw[rowSums(is.na(df_raw)) != ncol(df_raw)-1, colSums(is.na(df_raw))<nrow(df_raw)]
  # webscrape documentation as meta
  meta <- read_html(metadata)
  # create dataframe with pertinent information
  info <- data.frame(variable=character(), question=character(), data_type=character())
  # loop to parse relevant descriptions for column codes
  for (i in colnames(df)) {
    # get column class
    col_class <- class(df[[i]])
    # get html code containing code description
    title_meta <- meta %>% html_elements(xpath=glue("//*[contains(@id, '{i}')]"))
    # corner case, manage stringe case error in if statement when title_meta is empty
    if (length(title_meta)==0) {
      # fix casing for i to collect metadata properly
      i <- paste(str_sub(i, start=1, end=-2), str_to_lower(str_sub(i, start=-1)), sep='')
      # attempt to collect title_meta again
      title_meta <- meta %>% html_elements(xpath=glue("//*[contains(@id, '{i}')]"))
    }
    # extract text from title_meta
    test_data <- html_text(title_meta)
    # extract final description for column code
    desc <- str_trim(str_split_1(test_data, '-')[2])
    # append column metadata to row
    info[nrow(info) + 1,] = c(i, desc, col_class)
  }
  # print metadata for dataframe
  print(info)
  # codebook_data(info, str_sub(data_file, start = 6))  UNFINISHED FUNCTION DO NOT UNCOMMENT
  return(df)
}

# TODO This was a function to automatically write the codebook based on previous data, I ran out of time and wasn't able to complete it. Though I may add it in the future for personal use
# codebook_data <- function(df, data_name) {
#   lines_to_write <- c()
#   if (!file.exists("data/codebook.txt")) {
#     lines_to_write[[length(lines_to_write)+1]] = "TODO add description"
#     lines_to_write[[length(lines_to_write)+1]] = ""
#   } else if (grep(data_name, read.delim("data/codebook.txt"))) {
#     return()
#   }
#   lines_to_write[[length(lines_to_write)+1]] = data_name
#   for i in 1:nrow(df)
#   
#   # writeLines(lines_to_write, "data/codebook.txt")
#   print(lines_to_write)
# }
