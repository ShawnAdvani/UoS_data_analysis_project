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
dep_df[dep_df==7|dep_df==9|is.na(dep_df)] <- NA
# calculate and add to df depression_score (dep_score - 0 to 27)
# dep_df$dep_score <- rowSums(dep_df[,-1])
dep_df_phq <- dep_df[,-11]
dep_df$dep_score <- rowSums(dep_df_phq[,-1])
# make bins with categories based on Kurt et. al. (1997)
break_points <- c(0,4,9,14,19,27)
dep_df$dep_group <- cut(dep_df$dep_score, breaks=break_points, 
                      include.lowest=TRUE, right=TRUE) 
dep_df$dep_cat <- recode(dep_df$dep_group, 
                         "[0,4]" = "minimal", 
                         "(4,9]" = "mild", 
                         "(9,14]" = "moderate",
                         "(14,19]" = "moderate/severe",
                         "(19,27]" = "severe")

# plot histogram scores of depression_score
ggplot(dep_df, aes(dep_score)) + geom_histogram(breaks=break_points)
dep_df[c('dep_cat','dep_score')] # depression score shows to have an exponential distribution

# replace NA in HIQ210 to 7 (refused response)
insur_df[["HIQ210"]][is.na(insur_df[["HIQ210"]])] <- 7

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
# process table and run chi squared analysis for general health and mental health
da_men <- df %>% filter(HUQ090==1|HUQ090==2)
chisq.test(table(df$dep_cat,df$gen_health))
chisq.test(table(da_men$dep_cat,da_men$HUQ090))

# run Kruskal-Wallis against categorical response and score
kruskal.test(dep_cat~gen_health, data = df)
kruskal.test(dep_cat~HUQ090, data = da_men)

# TODO need graphical analysis

## Analyse Condition Data
# cleaning condition data
for (i in c(
  'MCQ010', 'AGQ030', 'MCQ160A', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 
  'MCQ160F', 'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'MCQ220', 'OSQ230'
)) {
  df[[i]][df[[i]]==9|df[[i]]==7|is.na(df[[i]])] <- NA
  df[[i]][df[[i]]==2|df[[i]]==0] <- FALSE
  df[[i]][df[[i]]==1] <- TRUE
  
}

# summarise condition data
a <- df %>% group_by(dep_cat) %>% reframe(
  asthma=sum(MCQ010==TRUE), no_astma=sum(MCQ010==FALSE),
  allegies=sum(AGQ030==TRUE), no_allergies=sum(AGQ030==FALSE),
  arthritis=sum(MCQ160A==TRUE), no_arthritis=sum(MCQ160A==FALSE),
  heart_failure=sum(MCQ160B==TRUE), no_heart_failure=sum(MCQ160B==FALSE),
  coranary=sum(MCQ160C==TRUE), no_coronary=sum(MCQ160C==FALSE),
  angina=sum(MCQ160D==TRUE), no_angina=sum(MCQ160D==FALSE),
  heart_attack=sum(MCQ160E==TRUE), no_heart_attack=sum(MCQ160E==FALSE),
  stroke=sum(MCQ160F==TRUE), no_stroke=sum(MCQ160F==FALSE),
  thyroid_p=sum(MCQ160M==TRUE), no_thryoid_p=sum(MCQ160M==FALSE),
  copd=sum(MCQ160P==TRUE), no_copd=sum(MCQ160P==FALSE),
  liver_d=sum(MCQ160L==TRUE), no_liver_d=sum(MCQ160L==FALSE),
  gallstones=sum(MCQ550==TRUE), no_gallstones=sum(MCQ550==FALSE),
  cancer=sum(MCQ220==TRUE), no_cancer=sum(MCQ220==FALSE),
  metal=sum(OSQ230==TRUE), no_metal=sum(OSQ230==FALSE)
)
