library(ggplot2)
source('func/cdc_parsing.R')
source('func/stats_testing.R')


# Load in 2021-2023 data using custom parsing function
dep_df <- df_parser('data/DPQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DPQ_L.htm')
insur_df <- df_parser('data/HIQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HIQ_L.htm')
access_df <- df_parser('data/HUQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/HUQ_L.htm')
cond_df <- df_parser('data/MCQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/MCQ_L.htm')

## Depression df calculations
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

## Merging Insurance Dataframes
# merge insurance coverage information into depression df
di <- left_join(dep_df[c("SEQN", "dep_score", "dep_cat")], 
                       dep_insur[c('SEQN', 'HIQ011','HIQ210')], 'SEQN')
# summarise insured data against depression category
di_sum <- di %>% group_by(dep_cat) %>% 
  summarise(
    # currently_insured=sum(HIQ011==1),
    # currently_uninsured=sum(HIQ011==0),
    uninsured_past_year=sum(HIQ210==1), 
    insured_past_year=sum(HIQ210==0)
    )

# Use custom function to process table and run chi squared analysis
df_to_chisq(di_sum)


