if (system.file(package='viridis')=="") {install.packages("viridis")}
if (system.file(package='ggthemes')=="") {install.packages("ggthemes")}
if (system.file(package='ggiraph')=="") {install.packages("ggiraph")}
if (system.file(package='shiny')=="") {install.packages("shiny")}
if (system.file(package='shinythemes')=="") {install.packages("shinythemes")}

# import all dependencies
require(ggplot2)
require(ggiraph)
require(shiny)
require(shinythemes)
require(viridis)
require(ggthemes)

# function to initialise graphing and implementation of shiny elements
run_app <- function(df, filter_options, filter_options_labeled) {
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
      graphing_df <- df
      # loop through the filter options
      for (i in filter_options) {
        # assign checked variables to "yes" result and unchecked to "no" result
        if (i == 'gen_health') {
          next
        } else if (i %in% input$cols){
          graphing_df <- graphing_df[graphing_df[[i]]=='Yes',]
        } else {
          graphing_df <- graphing_df[graphing_df[[i]]=='No',]
        }
      }
      # pivot the graph to group the appropriate format
      graphing_df %>% group_by(gen_health, depression_level) %>% summarise(Probability=mean(Probability))
    })
    # assign the interactive plot to the appropriate css tag
    output$interactivePlot <- renderGirafe({
      graphing_df <- data.frame(filtered_data())
      graphing_scores(graphing_df, name='')
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
}

# graphing function based on probability dataframe
graphing_scores <- function(df=df, name='base') {
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
    fill = depression_level,
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
    aes(name='Depression Categorical depression_level')
    # scale the probabilities as percentages
  ) + scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    # add theaming
    theme_economist() + 
    scale_fill_viridis(discrete = TRUE, direction = -1, option = "rocket")
  # export interactive plot element 
  interactive_plot <- ggiraph(ggobj=output_plot, width_svg = 11, height_svg = 8.5)
  if (name != '') {
    htmltools::save_html(interactive_plot, glue('figs/{name}.html'))
  }
  return(interactive_plot)
}