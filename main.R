library(ggplot2)
library(tidyverse)
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
                      include.lowest=TRUE, right=TRUE) 
dep_df$dep_cat <- recode(dep_df$dep_group, 
                         "[0,4]"="minimal", 
                         "(4,9]" = "mild", 
                         "(9,14]" = "moderate",
                         "(14,19]" = "moderate/severe",
                         "(19,27]" = "severe")

# plot histogram scores of depression_score
ggplot(dep_df, aes(dep_score)) + geom_histogram(breaks=break_points)
dep_df # depression score shows to have an exponential distribution


# replace NA in HIQ210 to 7 (refused response)
insur_df[["HIQ210"]][is.na(insur_df[["HIQ210"]])] <- 7

# recode general health condition to qeustionnaire categories
access_df$gen_health <- recode(access_df$HUQ010,
                        '1'='excellent',
                        '2'='very_good',
                        '3'='good',
                        '4'='fair',
                        '5'='poor'
)

# Merge other df into depression df
df <- left_join(dep_df, insur_df, 'SEQN')
df <- left_join(df, access_df, 'SEQN')
df <- left_join(df, cond_df, 'SEQN')

## Analyse Insurance Data
# summarise insured data against depression category
di_sum <- df %>% group_by(dep_cat) %>% 
  summarise(
    currently_insured=sum(HIQ011==1),
    currently_uninsured=sum(HIQ011==2),
    uninsured_past_year=sum(HIQ210==1), 
    insured_past_year=sum(HIQ210==2)
    )

# Use custom function to process table and run chi squared analysis
df_to_chisq(di_sum[c('dep_cat','currently_insured','currently_uninsured')])  # correlation is insignificant (p=0.0645)
df_to_chisq(di_sum[c('dep_cat','uninsured_past_year','insured_past_year')])  # received warning: Chi-squared approximation may be incorrect

# Run a Wilcoxon rank-sum test analysis on the data
wilcox.test(di_sum$uninsured_past_year, di_sum$insured_past_year)
wilcox.test(di_sum$currently_insured, di_sum$currently_uninsured) # correlation is insignificant (p=0.05556)

# TODO need graphical analysis

## Analysing Hospital Access Data
# Use custom function to process table and run chi squared analysis
chisq.test(table(dh$dep_cat,dh$gen_health))

# run Kruskal-Wallis against categorical response and score
kruskal.test(dep_cat~gen_health, data = dh)

# TODO need graphical analysis
