library(ggplot2)
library(tidyverse)
library(janitor)
source('func/cdc_parsing.R')
source('func/stats_testing.R')


# Load in 2021-2023 data using custom parsing function
dep_df <- df_parser('data/DPQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DPQ_L.htm')
insur_df <- df_parser('data/HIQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HIQ_L.htm')
access_df <- df_parser('data/HUQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HUQ_L.htm')
cond_df <- df_parser('data/MCQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/MCQ_L.htm')
## Dataframe calculations and cleaning
# convert missing, don't know, and refused responses to 0
dep_df[dep_df==7|dep_df==9|is.na(dep_df)] <- 0
# calculate and add to df depression_score (dep_score - 0 to 27)
# dep_df$dep_score <- rowSums(dep_df[,-1])
dep_df_phq <- dep_df[,-11]
dep_df$dep_score <- rowSums(dep_df_phq[,-1])
# make bins with categories based on Kurt et. al. (1997)
break_points <- c(0,4,9,14,19,27)
dep_df$dep_group <- cut(dep_df$dep_score, breaks=break_points, 
                      include.lowest=1, right=1) 
dep_df$dep_cat <- recode(dep_df$dep_group, 
                         "[0,4]" = "minimal", 
                         "(4,9]" = "mild", 
                         "(9,14]" = "moderate",
                         "(14,19]" = "moderate/severe",
                         "(19,27]" = "severe")

# plot histogram scores of depression_score
ggplot(dep_df, aes(dep_score)) + geom_histogram(breaks=break_points)
dep_df[c('dep_cat','dep_score')] # depression score shows to have an exponential distribution

# replace NA/missing in insur_df to 7 (refused response)
insur_df[["HIQ210"]][is.na(insur_df[["HIQ210"]])|insur_df[["HIQ210"]]==9] <- 7
insur_df[["HIQ011"]][is.na(insur_df[["HIQ011"]])|insur_df[["HIQ011"]]==9] <- 7

ggplot(insur_df %>% filter(HIQ210==1|HIQ210==2), aes(HIQ210)) +
  geom_bar()

# recode general health condition to qeustionnaire categories
# access_df$gen_health <- recode(access_df$HUQ010,
#                         '1'='excellent',
#                         '2'='very_good',
#                         '3'='good',
#                         '4'='fair',
#                         '5'='poor'
# )
# access_df <- access_df %>% select(-HUQ010)
access_df[['HUQ010']][access_df[['HUQ010']]==9] <- 7
access_df[['HUQ090']][access_df[['HUQ090']]==9] <- 7

pie(table(access_df$gen_health))

# cleaning condition data
cond_columns <- c(
  'MCQ010', 'AGQ030', 'MCQ160A', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 
  'MCQ160F', 'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'MCQ220', 'OSQ230'
)
for (i in cond_columns) {
  cond_df[[i]][cond_df[[i]]==9|cond_df[[i]]==7|is.na(cond_df[[i]])] <- 7
}

# Merge other df into depression df
df <- left_join(dep_df[c("SEQN", "dep_cat","dep_score")], 
                insur_df[c("SEQN", "HIQ011", "HIQ210")], "SEQN")
df <- left_join(df, access_df[c("SEQN", "gen_health", "HUQ010", "HUQ090")], "SEQN")
df <- left_join(df, cond_df[c("SEQN", 'MCQ010', 'AGQ030', 'MCQ160A', 'MCQ160B',
                            'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160M',
                            'MCQ160P', 'MCQ160L', 'MCQ550', 'MCQ220', 'OSQ230')], 
                "SEQN")

for (i in colnames(df)) {
  if (length(unique(df[[i]]))==3) {
    df[[i]] <- recode(df[[i]], '1' = 'Yes', '2' = 'No', '7' = 'Missing')
  } else {
    df[[i]] <- recode(df[[i]], '7' = 'Missing')
  }
  print(i)
  print(unique(df[[i]]))
}

sig_columns_score <- df_significance_testing(df %>% select(-dep_cat), 'dep_score', 'SEQN')
sig_columns_cat <- df_significance_testing(df %>% select(-dep_score), 'dep_cat', 'SEQN')

score_df <- df[c('dep_score', sig_columns_score)]
score_df[score_df=='Missing'] <- NA
cat_df <- df[c('dep_cat', sig_columns_cat)]
cat_df[cat_df=='Missing'] <- NA