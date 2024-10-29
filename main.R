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
access_df$gen_health <- recode(access_df$HUQ010,
                        '1'='excellent',
                        '2'='very_good',
                        '3'='good',
                        '4'='fair',
                        '5'='poor'
)
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
  }
  print(i)
  print(unique(df[[i]]))
}

## Analyse Insurance Data
# summarise insured data against depression category
di_sum <- df %>% group_by(dep_cat) %>% 
  summarise(
    currently_insured=sum(HIQ011=='Yes'),
    currently_uninsured=sum(HIQ011=='No'),
    uninsured_past_year=sum(HIQ210=='Yes'), 
    insured_past_year=sum(HIQ210=='No')
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
da_men <- df %>% filter(HUQ090=='Yes'|HUQ090=='No')
chisq.test(table(df$dep_cat,df$gen_health))
chisq.test(table(da_men$dep_cat,da_men$HUQ090))

# run Kruskal-Wallis against categorical response and score
kruskal.test(dep_cat~gen_health, data = df)
kruskal.test(dep_cat~HUQ090, data = da_men)

# TODO need graphical analysis

## Analyse Condition Data

# summarise condition data
dep_cond <- df %>% group_by(dep_cat) %>% reframe(
  asthma=sum(MCQ010=='Yes'), no_asthma=sum(MCQ010=='No'),
  allegies=sum(AGQ030=='Yes'), no_allergies=sum(AGQ030=='No'),
  arthritis=sum(MCQ160A=='Yes'), no_arthritis=sum(MCQ160A=='No'),
  heart_failure=sum(MCQ160B=='Yes'), no_heart_failure=sum(MCQ160B=='No'),
  coranary=sum(MCQ160C=='Yes'), no_coronary=sum(MCQ160C=='No'),
  angina=sum(MCQ160D=='Yes'), no_angina=sum(MCQ160D=='No'),
  heart_attack=sum(MCQ160E=='Yes'), no_heart_attack=sum(MCQ160E=='No'),
  stroke=sum(MCQ160F=='Yes'), no_stroke=sum(MCQ160F=='No'),
  thyroid_p=sum(MCQ160M=='Yes'), no_thryoid_p=sum(MCQ160M=='No'),
  copd=sum(MCQ160P=='Yes'), no_copd=sum(MCQ160P=='No'),
  liver_d=sum(MCQ160L=='Yes'), no_liver_d=sum(MCQ160L=='No'),
  gallstones=sum(MCQ550=='Yes'), no_gallstones=sum(MCQ550=='No'),
  cancer=sum(MCQ220=='Yes'), no_cancer=sum(MCQ220=='No'),
  metal=sum(OSQ230=='Yes'), no_metal=sum(OSQ230=='No')
)

# Run wilcox tests on all the variables
conditions = c('asthma', 'allegies', 'arthritis', 'heart_failure', 'coranary',
               'angina', 'heart_attack', 'stroke', 'thyroid_p', 'copd', 'liver_d',
               'gallstones', 'cancer', 'metal')
affective_conditions = c()
for (i in conditions) {
  i_test <- wilcox.test(dep_cond[[i]], dep_cond[[glue('no_{i}')]])
  print(i)
  if (i_test$p.value<0.05) {
    print(i_test)
    affective_conditions <- c(affective_conditions, i)
  } else {
    print(i_test$p.value)
  }
}

# TODO need graphical analysis

## Run Ordinal Logistic Regression on Significant Variables


