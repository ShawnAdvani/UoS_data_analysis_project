library(ggplot2)
source('func/custom_functions.R')

# Load in 2021-2023 data using custom parsing function
dep_df <- df_parser('data/DPQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DPQ_L.htm')
insur_df <- df_parser('data/HIQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HIQ_L.htm')
access_df <- df_parser('data/HUQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HUQ_L.htm')
cond_df <- df_parser('data/MCQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/MCQ_L.htm')

## Depression df calculations
# convert missing, don't know, and refused responses to 0
dep_df[dep_df==7|dep_df==9|is.na(dep_df)] <- 0
# calculate and add to df depression_score (dep_score - 0 to 30)
dep_df$dep_score <- rowSums(dep_df[,-1])
# Plot histogram scores of depression_score
ggplot(dep_df, aes(dep_score)) + geom_histogram()
dep_df # depression score shows to have an exponential distribution

## Insurance df calculations


