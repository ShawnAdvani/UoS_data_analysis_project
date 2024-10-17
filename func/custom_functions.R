if (system.file(package='rvest')=="") {install.packages('rvest')}
if (system.file(package='xml2')=="") install.packages('xml2')

library(haven)
library(rvest)
library(stringr)
library(glue)

df_parser <- function(data_file, metadata) {
  print(str_sub(data_file, start = 6, end = -5))
  df_raw <- read_xpt(data_file)
  df <- df_raw[,colSums(is.na(df_raw))<nrow(df_raw)]
  meta <- read_html(metadata)
  info <- data.frame(variable=character(), question=character(), data_type=character())
  for (i in colnames(df)) {
    col_class <- class(df[[i]])
    title_meta <- meta %>% html_elements(xpath=glue("//*[contains(@id, '{i}')]"))
    if (length(title_meta)==0) {
      i <- paste(str_sub(i, start=1, end=-2), str_to_lower(str_sub(i, start=-1)), sep='')
      title_meta <- meta %>% html_elements(xpath=glue("//*[contains(@id, '{i}')]"))
    }
    test_data <- html_text(title_meta)
    desc <- str_trim(str_split_1(test_data, '-')[2])
    info[nrow(info) + 1,] = c(i, desc, col_class)
  }
  print(info)
  # codebook_data(info, str_sub(data_file, start = 6))
  return(df)
}

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