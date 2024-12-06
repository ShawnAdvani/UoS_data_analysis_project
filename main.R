require(tidyverse)
require(janitor)
require(glue)
source('func/cdc_parsing.R')
source('func/stats_testing.R')
source('func/visualisation.R')

# Load in 2021-2023 data using custom parsing function
dep_df <- df_parser('data/DPQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DPQ_L.htm')
insur_df <- df_parser('data/HIQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HIQ_L.htm')
access_df <- df_parser('data/HUQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HUQ_L.htm')
cond_df <- df_parser('data/MCQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/MCQ_L.htm')

## Dataframe calculations and cleaning
# convert missing, don't know, and refused responses to 0
dep_df[dep_df==7|dep_df==9|is.na(dep_df)] <- 0
# calculate and add to df depression_score (dep_score - 0 to 27)
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
access_df$gen_health <- recode(access_df$HUQ010,
                        '1'='excellent',
                        '2'='very_good',
                        '3'='good',
                        '4'='fair',
                        '5'='poor'
)
pie(table(access_df$gen_health))

# replace NA/missing in access_df to 7 (refused response) 
access_df[['HUQ090']][access_df[['HUQ090']]==9] <- 7

# cleaning condition data
cond_columns <- c(
  'MCQ010', 'AGQ030', 'MCQ160A', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 
  'MCQ160F', 'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'MCQ220', 'OSQ230'
)
for (i in cond_columns) {
  cond_df[[i]][cond_df[[i]]==9|cond_df[[i]]==7|is.na(cond_df[[i]])] <- 7
}

# Merge appropriate variables from other dfs into depression df
df <- left_join(dep_df[c("SEQN", "dep_cat","dep_score")], 
                insur_df[c("SEQN", "HIQ011", "HIQ210")], "SEQN")
df <- left_join(df, access_df[c("SEQN", "gen_health", "HUQ090")], "SEQN")
df <- left_join(df, cond_df[c("SEQN", 'MCQ160A', 'MCQ160B', 'MCQ160C', 'MCQ160D', 
                            'MCQ160E', 'MCQ160F', 'MCQ160M', 'MCQ160P', 'MCQ160L', 
                            'MCQ550', 'MCQ220', 'OSQ230')], "SEQN")

for (i in colnames(df)) {
  # recode bivariate responses to Yes and No
  if (length(unique(df[[i]]))==3) {
    df[[i]] <- recode(df[[i]], '1' = 'Yes', '2' = 'No')
  }
  print(i)
  print(unique(df[[i]]))
}

# run significance on both score and category depression responses
sig_columns_score <- df_significance_testing(subset(df, select = -c(dep_cat)), 'dep_score', 'SEQN')
sig_columns_cat <- df_significance_testing(subset(df, select = -c(dep_score)), 'dep_cat', 'SEQN')

# create score df for analysis
score_df <- df[c('dep_score', sig_columns_score)]
# create category df for analysis (primary use)
cat_df <- df[c('dep_cat', sig_columns_cat)]
cat_df <- na.omit(cat_df)

## LOGISTIC REGRESSION
# create ordinal logistic regression model using a custom function
m <- olr_testing(cat_df, sig_columns_cat, 'dep_cat')

# create dataframe with all variable possibilities
reg_df <<- data.frame(
  HIQ210 = rep(c('Yes', 'No'), 5120),
  HUQ090 = rep(c('Yes', 'No'), 2560, each=2),
  MCQ160A = rep(c('Yes', 'No'), 1280, each=4),
  MCQ160B = rep(c('Yes', 'No'), 640, each=8),
  MCQ160D = rep(c('Yes', 'No'), 320, each=16),
  MCQ160F = rep(c('Yes', 'No'), 160, each=32),
  MCQ160M = rep(c('Yes', 'No'), 80, each=64),
  MCQ160P = rep(c('Yes', 'No'), 40, each=128),
  MCQ160L = rep(c('Yes', 'No'), 20, each=256),
  MCQ550 = rep(c('Yes', 'No'), 10, each=512),
  OSQ230 = rep(c('Yes', 'No'), 5, each=1024),
  gen_health = rep(c("good","very_good","excellent","fair","poor"), each=1024)
)

# calculate probability to each possible variable outcome
reg_df_probs <- cbind(reg_df, predict(m, reg_df, type = "probs"))

# add probabilities to each possible variable outcome in dataframe
lreg_df <- melt(reg_df_probs, id.vars = sig_columns_cat, variable.name = "depression_level", value.name = "Probability", )

# add labels for the filter option
filter_options_labeled <- c(
  'Uninsured Past Year'='HIQ210', 
  'Seen a Mental Health Professional Past Year'='HUQ090', 
  'Arthritis'='MCQ160A', 
  'Congestive Heart Failure'='MCQ160B', 
  'Angina'='MCQ160D', 
  'Stroke'='MCQ160F',
  'Thyroid Problems'='MCQ160M', 
  'COPD/Emphasema/ChB'='MCQ160P', 
  'Liver Condition'='MCQ160L', 
  'Gallstones'='MCQ550',
  'Metal in Body'='OSQ230'
)

# export png version of the script note: this sometimes displays odd in RStudio but displays fine in the output file
graphing_scores(lreg_df %>% group_by(gen_health, depression_level) %>% summarise(Probability=mean(Probability)))

# run function to run app
run_app(lreg_df, sig_columns_cat, filter_options_labeled)

