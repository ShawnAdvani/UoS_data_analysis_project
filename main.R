if (system.file(package='foreign')=="") {install.packages('foreign')}
if (system.file(package='MASS')=="") {install.packages('MASS')}
if (system.file(package='Hmisc')=="") {install.packages('Hmisc')}
if (system.file(package='reshape2')=="") {install.packages('reshape2')}
if (system.file(package='viridis')=="") {install.packages("viridis")}
if (system.file(package='ggthemes')=="") {install.packages("ggthemes")}
if (system.file(package='ggiraph')=="") {install.packages("ggiraph")}

require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)


library(ggplot2)
library(ggiraph)
library(tidyverse)
library(janitor)
source('func/cdc_parsing.R')
source('func/stats_testing.R')

library(viridis)
library(ggthemes)


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
    df[[i]] <- recode(df[[i]], '1' = 'Yes', '2' = 'No')
  }
  print(i)
  print(unique(df[[i]]))
}

sig_columns_score <- df_significance_testing(subset(df, select = -c(dep_cat)), 'dep_score', 'SEQN')
sig_columns_cat <- df_significance_testing(subset(df, select = -c(dep_score)), 'dep_cat', 'SEQN')

# df[df=='Missing'] <- NA
score_df <- df[c('dep_score', sig_columns_score)]
# score_df[score_df=='Missing'] <- NA
cat_df <- df[c('dep_cat', sig_columns_cat)]
# cat_df[cat_df=='Missing'] <- NA
cat_df <- na.omit(cat_df)

## LOGISTIC REGRESSION
# Statistical testing
m <- polr(formula = dep_cat ~ HIQ210 + HUQ090 + gen_health +  # AGQ030 + gen_health + MCQ010
            MCQ160A + MCQ160B + MCQ160D + MCQ160F + MCQ160M + 
            MCQ160P + MCQ160L + MCQ550 + OSQ230, data = cat_df, Hess=TRUE)
summary(m)

(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))
exp(coef(m))
exp(cbind(OR = coef(m), ci))

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(cat_df, summary(as.numeric(dep_cat) ~ HIQ210 + HUQ090 + gen_health +  # AGQ030 + gen_health + MCQ010
                             MCQ160A + MCQ160B + MCQ160D + MCQ160F + MCQ160M + 
                             MCQ160P + MCQ160L + MCQ550 + OSQ230, fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

# OLR analysis
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

reg_df_probs <- cbind(reg_df, predict(m, reg_df, type = "probs"))

lreg_df <- melt(reg_df_probs, id.vars = c(
  'HIQ210', 'HUQ090', 'MCQ160A', 'MCQ160B', 'MCQ160D', 'MCQ160F',
  'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'OSQ230', 'gen_health'
  ), variable.name = "Level", value.name = "Probability", )
# lreg_df$gen_health <- recode(
#   lreg_df$gen_health, 
#     'excellent'=5,
#     'very_good'=4,
#     'good'=3,
#     'fair'=2,
#     'poor'=1
#   )

graphing_stuff <- function(df, name='TOTAL') {
  graph_df <- df %>% group_by(gen_health, Level) %>% summarise(Probability=mean(Probability))
  svg(glue('figs/{name}svg'), width = 11, height = 8.5)
  output_plot <- ggplot(graph_df, aes(
    x = factor(gen_health, levels = c('excellent', 'very_good', 'good', 'fair', 'poor')), 
    y = Probability, 
    fill = Level,
  )) + geom_bar_interactive(position = 'dodge', stat = 'identity') + labs(
    title = glue('Depression Probability: {name}'),
    x = 'Reported General Health Status',
    y = 'Probability of Outcome (0 to 1)',
    aes(name='Depression Categorical Level')
  ) + 
    # theme_economist() + scale_fill_economist() +
    # theme_wsj() + scale_fill_wsj(palette = "colors6")
    # theme_stata() + scale_fill_stata()
    theme_economist() + 
    scale_fill_viridis(discrete = TRUE, direction = -1, option = "rocket")
  dev.off()
  ggsave(glue('figs/{name}.png'), width = 11, height = 8.5)
  return(output_plot)
}

# svg(glue('figs/{name}.png'), width = 11, height = 8.5)

graphing_stuff(lreg_df)

filter_options <- c('HIQ210', 'HUQ090', 'MCQ160A', 'MCQ160B', 'MCQ160D', 'MCQ160F',
                    'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'OSQ230')

for (i in filter_options) {
  graph_df_filtered <- lreg_df[lreg_df[[i]]=='Yes',]
  for (j in filter_options) {
    if (j==i) {next}
    graph_df_filtered <- graph_df_filtered[graph_df_filtered[[j]]=='No',]
  }
  print(graph_df_filtered)
  graphing_stuff(graph_df_filtered, i)
}

graph_df_filtered <- lreg_df
for (i in filter_options) {
  graph_df_filtered <- graph_df_filtered[graph_df_filtered[[i]]=='No',]
}
print(graph_df_filtered)
graphing_stuff(graph_df_filtered, 'NONE')


# TODO add interactive element to combine into one plot, rename condition names