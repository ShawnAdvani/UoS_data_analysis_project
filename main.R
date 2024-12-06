if (system.file(package='foreign')=="") {install.packages('foreign')}
if (system.file(package='MASS')=="") {install.packages('MASS')}
if (system.file(package='Hmisc')=="") {install.packages('Hmisc')}
if (system.file(package='reshape2')=="") {install.packages('reshape2')}
if (system.file(package='viridis')=="") {install.packages("viridis")}
if (system.file(package='ggthemes')=="") {install.packages("ggthemes")}
if (system.file(package='ggiraph')=="") {install.packages("ggiraph")}
if (system.file(package='shiny')=="") {install.packages("shiny")}
if (system.file(package='shinythemes')=="") {install.packages("shinythemes")}

require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)

require(ggplot2)
require(ggiraph)
require(shiny)
require(shinythemes)
require(tidyverse)
require(janitor)
source('func/cdc_parsing.R')
source('func/stats_testing.R')

require(viridis)
require(ggthemes)

# Load in 2021-2023 data using custom parsing function
dep_df <- df_parser('data/DPQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DPQ_L.htm')
insur_df <- df_parser('data/HIQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HIQ_L.htm')
access_df <- df_parser('data/HUQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/HUQ_L.htm')
cond_df <- df_parser('data/MCQ_L.XPT', 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/MCQ_L.htm')
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
# access_df[['HUQ010']][access_df[['HUQ010']]==9] <- 7
# access_df[['HUQ090']][access_df[['HUQ090']]==9] <- 7

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
df <- left_join(df, access_df[c("SEQN", "gen_health", "HUQ090")], "SEQN")
df <- left_join(df, cond_df[c("SEQN", 'MCQ010', 'AGQ030', 'MCQ160A', 'MCQ160B',
                            'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160M',
                            'MCQ160P', 'MCQ160L', 'MCQ550', 'MCQ220', 'OSQ230')], 
                "SEQN")

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
# Statistical testing

# fit the data to the ordinal logistic regression model
m <- polr(formula = dep_cat ~ HIQ210 + HUQ090 + gen_health + AGQ030 + MCQ010 +
            MCQ160A + MCQ160B + MCQ160D + MCQ160F + MCQ160M + 
            MCQ160P + MCQ160L + MCQ550 + OSQ230, data = cat_df, Hess=TRUE)
# display summary
summary(m)
# display coefficients
(ctable <- coef(summary(m)))
# calculate t statistic of variables
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# calculate significance of variable 
(ctable <- cbind(ctable, "p value" = p))
# calcualte confidence intervals
(ci <- confint(m))
# calculate confidence interfals based on coefficients
exp(coef(m))
exp(cbind(OR = coef(m), ci))

# function to calculate odds at each value being greater than or less than value
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

# calculate logistic scores based on calculations
(s <- with(cat_df, summary(as.numeric(dep_cat) ~ HIQ210 + HUQ090 + gen_health + AGQ030 + MCQ010 +
                             MCQ160A + MCQ160B + MCQ160D + MCQ160F + MCQ160M + 
                             MCQ160P + MCQ160L + MCQ550 + OSQ230, fun=sf)))
# display and plot impact of logit calculations
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

# OLR analysis
# create dataframe with all variable possibilities
reg_df <<- data.frame(
  HIQ210 = rep(c('Yes', 'No'), 20480),
  HUQ090 = rep(c('Yes', 'No'), 10240, each=2),
  MCQ160A = rep(c('Yes', 'No'), 5120, each=4),
  MCQ160B = rep(c('Yes', 'No'), 2560, each=8),
  MCQ160D = rep(c('Yes', 'No'), 1280, each=16),
  MCQ160F = rep(c('Yes', 'No'), 640, each=32),
  MCQ160M = rep(c('Yes', 'No'), 320, each=64),
  MCQ160P = rep(c('Yes', 'No'), 160, each=128),
  MCQ160L = rep(c('Yes', 'No'), 80, each=256),
  MCQ550 = rep(c('Yes', 'No'), 40, each=512),
  OSQ230 = rep(c('Yes', 'No'), 20, each=1024),
  AGQ030 = rep(c('Yes', 'No'), 10, each=2048),
  MCQ010 = rep(c('Yes', 'No'), 5, each=4096),
  gen_health = rep(c("good","very_good","excellent","fair","poor"), each=4096)
)

# calculate probability to each possible variable outcome
reg_df_probs <- cbind(reg_df, predict(m, reg_df, type = "probs"))

# add probabilities to each possible variable outcome in dataframe
lreg_df <- melt(reg_df_probs, id.vars = c(
  'HIQ210', 'HUQ090', 'MCQ160A', 'MCQ160B', 'MCQ160D', 'MCQ160F', 'AGQ030', 'MCQ010',
  'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'OSQ230', 'gen_health'
  ), variable.name = "Level", value.name = "Probability", )

# graping function based on probability dataframe
graphing_stuff <- function(df=lreg_df, name='') {
  output_plot <- ggplot(df, mapping=aes(
    # set general health to the categorical x value
    x = factor(gen_health, levels = c(
      'Excellent'='excellent', 
      'Very Good'='very_good', 
      'Good'='good', 
      'Fair'='fair', 
      'Pool'='poor'
      )), 
    # set the probability to the y variable
    y = Probability, 
    # set fill to be the depression level, making a seperate bar for each
    fill = Level,
    # create tooltop to display the probability percent when hovered over
    tooltip = glue('Probability: {round(Probability, 4)*100}%'), 
    # assign interactive element to probability variable
    data_id = Probability
  # create the interactive bar chart
  )) + geom_bar_interactive(position = 'dodge', stat = 'identity') + labs(
    # labeling
    title = 'Depression Probability',
    x = 'Self Reported General Health Status',
    y = 'Probability of Outcome (0 to 1)',
    # set hover to focus on mouse cursor
    hover_nearest = TRUE,
    aes(name='Depression Categorical Level')
  # scale the probabilities as percentages
  ) + scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    # add theaming
    theme_economist() + 
    scale_fill_viridis(discrete = TRUE, direction = -1, option = "rocket")
  # export interactive plot element 
  interactive_plot <- ggiraph(ggobj=output_plot, width_svg = 11, height_svg = 8.5)
  # TODO htmltools::save_html(interactive_plot, glue('figs/{name}.html'))
  return(interactive_plot)
}

# add filter option TODO to be remmoved
filter_options <- c('HIQ210', 'HUQ090', 'MCQ160A', 'MCQ160B', 'MCQ160D', 'MCQ160F', 
                    'AGQ030', 'MCQ010', 'MCQ160M', 'MCQ160P', 'MCQ160L', 'MCQ550', 'OSQ230')
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

# create ui element for shiny chart
ui <- fluidPage(
  # add theming to page
  theme = shinytheme("flatly"),
  sidebarLayout( 
    sidebarPanel(
      # create check box filter option
      checkboxGroupInput("cols", "Select Conditions:", 
                         choices = filter_options_labeled, selected = filter_options
    # create table element with relevant data
    ), tableOutput(outputId = 'table')), 
    mainPanel(
      # create interactive graph element
      girafeOutput(outputId = "interactivePlot", width = '100%', height = NULL)
    )
  ),
)

# add server element to shiny plot for filtering
server <- function(input, output) {
  # create a filterig function
  filtered_data <- reactive({
    # create a new dataframe for filtering the output
    graphing_df <- lreg_df
    # loop through the filter options
    for (i in filter_options) {
      # assign checked variables to "yes" result and unchecked to "no" result
      if (i %in% input$cols){
        graphing_df <- graphing_df[graphing_df[[i]]=='Yes',]
      } else {
        graphing_df <- graphing_df[graphing_df[[i]]=='No',]
      }
    }
    # pivot the graph to group the ppropriate format
    graphing_df %>% group_by(gen_health, Level) %>% summarise(Probability=mean(Probability))
  })
  # assign the interactive plot to the appropriate css tag
  output$interactivePlot <- renderGirafe({
    graphing_df <- data.frame(filtered_data())
    graphing_stuff(graphing_df)
  })
  # assign the table to the appropriate css tag
  output$table <- renderTable({
    table_df <- filtered_data()
    table_df$Probability <- scales::percent(table_df$Probability, accuracy = 0.01)
    table_df
  })
}

# run the shiny app as a local webpage
shinyApp(ui = ui, server = server)

# TODO clean up remaining files, move custom functions to folders, write markdown and cookbook, export shiny as html

