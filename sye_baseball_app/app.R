library(shiny)
library(tidyverse)
library(nnet)

batting_data <- read_csv("batting_indicators.csv")
balanced_batting_data <- read_csv("balanced_batting_data.csv")

# LOADING THE INITIAL MODEL 

init_model <- multinom(hit_outcome_factor~ 
                         swing_length_zscore +
                         bat_speed_zscore +
                         release_speed_zscore +
                         launch_angle +
                         bat_speed_zscore:swing_length_zscore,
                       data = balanced_batting_data)

saveRDS(init_model, "initial_model.rds")


ui <- fluidPage(
  
  # TITLE
  titlePanel("Baseball Shiny App"),
  
  div(
    style = "background-color: #f9f9f9; border-left: 5px solid #AFD0BF; padding: 15px; margin-top: 10px; border-radius: 5px;",
    tags$p(
      "This application allows you to explore various baseball statistics for different players. ",
      br(),
      br(), 
      "You can select a player and view distributions of their hitting statistics such as launch angle, bat speed, swing length, and launch speed. ",
      br(),
      br(), 
      "Additionally, summary statistics will be provided to help analyze each player's performance."
    )),
  
  div(
    style = "background-color: #f9f9f9; border-left: 5px solid #AFD0BF; padding: 15px; margin-top: 10px; border-radius: 5px;",
    tags$p(
      tags$b("Key Variables Used:"),
      br(),
      br(),
      tags$b("Launch Angle:"), "Vertical launch angle of the batted ball as tracked by Statcast. Refers to the angle at which the ball leaves the bat",
      br(),
      tags$b("Bat Speed:"),"Measurement of how fast the bat is moving when the player makes contact with the ball",
      br(),
      tags$b("Swing Length:"),"Total amount of feet the bat traveled during the swing (the total distance traveled by the barrel of the bat in x/y/z space)"
    )
  ),
  
  selectizeInput("player_name",
                 label = "Select or search for a Player:",
                 choices = unique(batting_data$player_name),
                 selected = NULL
  ),
  
  div(
    style = "background-color: #f9f9f9; border-left: 5px solid #AFD0BF; padding: 15px; margin-top: 10px; border-radius: 5px;",
    tags$p("This section allows you to explore the relationship between the statisics for the batter's hitting mechanics.
             Select different variables to view either the distribution for the specific player, or a scatterplot to see what
             tends to happen to one of the variables while the other increases. You can also incorporate a third dimension, the size of
             the dot, to see how the level of the third varaible affects the other two variables.")
  ),
  
  # create distribution of launch speed, bat speed, swing length, launch angle, break down of hits? 
  fluidRow(
    
    column(4,
           selectInput("x_var",
                       "X Variable (required):",
                       choices = c("bat_speed",
                                   "swing_length",
                                   "launch_angle"
                       )
           )
    ),
    column(4,
           selectInput("y_var",
                       "Y Variable (optional):",
                       choices = c("None",
                                   "bat_speed",
                                   "swing_length",
                                   "launch_angle")
           ),
    ),
    column(4,
           selectInput("size_var",
                       "Size Variable (optional):",
                       choices = c("None", "bat_speed",
                                   "swing_length",
                                   "launch_angle")
           )
    )
  ),
  
  # diving and creating the plot and summary stats 
  fluidRow(
    
    column(
      6,
      plotOutput("stat_density_plot")
    ),
    
    column(
      6,
      tableOutput("summary_stats")
    ),
    
    column(6,
           tableOutput("season_stats")
    )
  ),
  
  titlePanel("Exploring Season Statistics and Predictions"),
  
  div(
    style = "background-color: #f9f9f9; border-left: 5px solid #AFD0BF; padding: 15px; margin-top: 10px; border-radius: 5px;",
    tags$p(
      "Another portion of this project involved model developement. 
        A model was created to analyze the effect that bat speed, launch angle, swing length, and the picther's release speed have
        on the probability of a batter hitting a single, double or triple, or a homerun.",
      br(),
      br(),
      "The sliders are set to the selected player's median vlaue for the 2023-2024 season.",
      br(),
      br(), 
      "Changing the values of the sliders represent hypothetical changes to a player's swing, and the model uses those changes to make
        new predictions to how the individual player might perform for the season."
    )),
  
  # INPUT FOR INITIAL MODEL 
  # want to input real numbers and then turn them into z scores , add interaction, and plug into model 
  fluidRow(
    column(4,
           sliderInput("swing_length", "Input for Swing Length:", min = 1, max = 15, value = 1, step = .1),
           plotOutput("sl_box", height = "100px")
    ),
    column(4,
           sliderInput("bat_speed", "Input for Bat Speed:", min = 10, max = 100, value = 10, step = .1),
           plotOutput("bs_box", height = "100px")
    ),
    column(4,
           sliderInput("launch_angle", "Input for Launch Angle:", min = 0, max = 90, value = 0, step = .1),
           plotOutput("la_box", height = "100px")
    )
  ),
  
  
  # submit button 
  actionButton("submit_button", "Make Predictions"),
  verbatimTextOutput("prediction"),
  plotOutput("comparison_plot")
  
)


server <- function(input, output, session) {

  
  # FILTERING THE DATA BASED ON THE SELECTED PLAYER 
  filtered_data <- reactive ({
    req(input$player_name)
    batting_data %>%
      filter(
        # filter data based on player name 
        player_name == input$player_name
      )
  })
  
  
  # MAKING THE DENSITY PLOT FOR THE SELECTED VARIABLE
  output$stat_density_plot <- renderPlot({
    
    data_filtered <- filtered_data()
    # getting inputs 
    x_var <- input$x_var 
    y_var <- input$y_var
    size_var <- input$size_var
    
    # histogram - only the x variable 
    if(y_var == "None" & size_var == "None"){
      stat_mean <- mean(data_filtered[[x_var]], na.rm = TRUE)
      # creating plot 
      ggplot(data_filtered, aes_string(x = x_var)) +
        geom_density(alpha = 0.6, fill = "#AFD0BF") + 
        labs(title = paste("Density Plot of",
                           x_var, "for",
                           input$batter), 
             x = x_var, 
             y = "Density") +
        geom_vline(aes(xintercept = stat_mean),
                   color = "blue",
                   linetype = "dashed",
                   size = 1) +
        theme_minimal()
      
    }else if (y_var != "None" & size_var == "None"){
      ggplot(data = data_filtered,
             mapping = aes_string(x = x_var,
                                  y = y_var)) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatterplot: ", x_var, " vs ", y_var),
             x = x_var,
             y = y_var) +
        theme_minimal()
    } else {
      ggplot(data = data_filtered,
             mapping = aes_string(x = x_var,
                                  y = y_var,
                                  size = size_var)) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatterplot: ", x_var, " vs ", y_var),
             x = x_var,
             y = y_var) +
        theme_minimal()
    } 
    
  })
  
  # MAKING TABLE OF SUMMARY STATS 
  output$summary_stats <- renderTable({
    data_filtered <- filtered_data()
    selected_stats <- input$stat
    
    if (length(selected_stats) == 0) {
      return(NULL)
    }
    
    summary_stats <- data_filtered %>%
      select(all_of(selected_stats)) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      group_by(Variable) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        Median = median(Value, na.rm = TRUE),
        SD = sd(Value, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE),
        .groups = "drop"
      )
    return(summary_stats)
  })
  
  # MAKING SEASON STATS TABLE
  output$season_stats <- renderTable({
    data_filtered <- filtered_data()
    
    season_stats <- data.frame(
      "Hit Type" = c("Single", "Double", "Triple", "Homerun"),
      Value = c(
        sum(data_filtered$events == "single"),
        sum(data_filtered$events == "double"),
        sum(data_filtered$events == "triple"),
        sum(data_filtered$events == "home_run")
      )
    )
    return(season_stats)
  })
  
  
  # SETTING VALUE OF SLIDER TO BE MEDIAN OF PLAYER SELECTED 
  observe({
    req(input$player_name)  
    
    data_filtered <- filtered_data()
    
    # Calculate medians
    median_la <- median(data_filtered$launch_angle, na.rm = TRUE)
    median_sl <- median(data_filtered$swing_length, na.rm = TRUE)
    median_bs <- median(data_filtered$bat_speed, na.rm = TRUE)
    
    # Update slider inputs with medians
    updateSliderInput(session, "swing_length", value = median_sl)
    updateSliderInput(session, "bat_speed", value = median_bs)
    updateSliderInput(session, "launch_angle", value = median_la)
  })
  
  
  # MAKING PREDICTIONS
  observeEvent(input$submit_button, {
    
    data_filtered = filtered_data()
    
    swing_length_input <- as.numeric(input$swing_length)
    bat_speed_input <- as.numeric(input$bat_speed)
    release_speed_input <- as.numeric(input$release_speed)
    launch_angle_input <- as.numeric(input$launch_angle)
    
    
    # means of columns 
    swing_length_mean <- mean(data_filtered$swing_length)
    bat_speed_mean <- mean(data_filtered$bat_speed)
    release_speed_mean <- mean(data_filtered$release_speed)
    
    # standard deviations 
    swing_length_sd <- sd(data_filtered$swing_length)
    bat_speed_sd <- sd(data_filtered$bat_speed)
    release_speed_sd <- sd(data_filtered$release_speed)
    
    # calculate z scores 
    swing_length_zscore = (swing_length_input - swing_length_mean) / swing_length_sd
    bat_speed_zscore = (bat_speed_input - bat_speed_mean) / bat_speed_sd
    release_speed_zscore = 0
    # (release_speed_input - release_speed_mean) / release_speed_sd
    
    # Create the interaction term
    interaction_term <- bat_speed_zscore * swing_length_zscore
    
    # data 
    input_data <- data.frame(
      swing_length_zscore = swing_length_zscore,
      bat_speed_zscore = bat_speed_zscore,
      release_speed_zscore = release_speed_zscore,
      launch_angle = launch_angle_input,
      `bat_speed_zscore:swing_length_zscore` = interaction_term
    )
    
    # load model 
    initial_model <- readRDS("initial_model.rds")
    
    # predict 
    prediction <- predict(initial_model, newdata = input_data, type = "class")
    
    # getting actual outcomes 
    data_filtered <- data_filtered %>%
      mutate(
        hit_outcome = case_when(
          events == "single" ~ 1,
          events %in% c("double", "triple") ~ 2,
          events == "home_run" ~ 3
        )
      )
    
    # actual season hit outcomes
    actual_outcomes <- data_filtered %>%
      group_by(hit_outcome) %>%
      mutate(Count = n(),
             Type = "Actual Season Statistics") %>%
      rename(Hit = hit_outcome)
    
    # what the model predicted for each hit
    prediction2 <- predict(initial_model, newdata = data_filtered, type = "class")
    predicted_outcomes <- data.frame(Hit = prediction2,
                                     Type = "Original Model Prediction")
    
    
    # sum of predicted outcomes 
    sum_pred_outcomes <- predicted_outcomes %>%
      group_by(Hit) %>%
      summarize(Count = n(), .groups = "drop") %>%
      mutate(Type = "Original Model Prediction")
    
    
    # using input tweaked data 
    tweaked_df <- data_filtered %>%
      mutate(
        swing_length = swing_length + swing_length_input,
        bat_speed = bat_speed + bat_speed_input,
        launch_angle = launch_angle + launch_angle_input
      ) %>%
      mutate(
        swing_length_zscore = (swing_length - mean(swing_length, na.rm = TRUE)) / sd(swing_length, na.rm = TRUE),
        bat_speed_zscore = (bat_speed - mean(bat_speed, na.rm = TRUE)) / sd(bat_speed, na.rm = TRUE),
        launch_angle_zscore = (launch_angle - mean(launch_angle, na.rm = TRUE)) / sd(launch_angle, na.rm = TRUE)
      )
    
    prediction_tweaked <- predict(initial_model, newdata = tweaked_df, type = "class")
    tweaked_predicted_outcomes <- data.frame(Hit = prediction_tweaked,
                                             Type = "Altered Model Prediction")
    
    # sum 
    sum_new_outcomes <- tweaked_predicted_outcomes %>%
      group_by(Hit) %>%
      summarize(Count = n(), .groups = "drop") %>%
      mutate(Type = "Altered Model Prediction")
    
    # combining 
    actual_outcomes$Hit <- factor(actual_outcomes$Hit)
    sum_pred_outcomes$Hit <- factor(sum_pred_outcomes$Hit)
    sum_new_outcomes$Hit <- factor(sum_new_outcomes$Hit)
    
    combined_df <- bind_rows(actual_outcomes, sum_pred_outcomes, sum_new_outcomes)
    combined_df$Type <- factor(combined_df$Type, levels = c("Actual Season Statistics",
                                                            "Original Model Prediction",
                                                            "Altered Model Prediction"))
    
    
    output$comparison_plot <- renderPlot({
      ggplot(data = combined_df,
             mapping = aes(x = Hit, y = Count, fill = Hit)) +
        facet_wrap(~ Type) +
        geom_col(position = "dodge") +
        labs(title = "Comparing Actual Season Statistics to Model Predicted Statistics",
             x = "Hit Outcome",
             y = "Count") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 15, face = "bold"),
          panel.border = element_rect(color = "black",
                                      fill = NA)
        )
    })
  })
  
  # MAKING THE BOC PLOTS FOR THE SLIDERS 
  
  # swing length 
  output$sl_box <- renderPlot({
    data_filtered <- filtered_data()
    ggplot(data = data_filtered,
           mapping = aes(x = swing_length)) +
      geom_boxplot(fill = "#AFD0BF") +
      labs(title = "Boxplot for Swing Length") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.y = element_blank()
      )
  })
  # bat speed
  output$bs_box <- renderPlot({
    data_filtered <- filtered_data()
    ggplot(data = data_filtered,
           mapping = aes(x = bat_speed)) +
      geom_boxplot(fill = "#AFD0BF") +
      labs(title = "Boxplot for Bat Speed") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.y = element_blank()
      )
  })
  # launch angle 
  output$la_box <- renderPlot({
    data_filtered <- filtered_data()
    ggplot(data = data_filtered,
           mapping = aes(x = launch_angle)) +
      geom_boxplot(fill = "#AFD0BF") +
      labs(title = "Boxplot for Launch Angle") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 13, face = "bold") ,
        axis.text.y = element_blank()
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)