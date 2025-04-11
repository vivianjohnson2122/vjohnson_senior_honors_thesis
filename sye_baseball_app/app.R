
library(shiny)
library(tidyverse)
library(nnet)

batting_data <- read_csv("batting_indicators.csv")

# LOADING THE INITIAL MODEL 

init_model <- multinom(hit_outcome ~ 
                         swing_length_zscore +
                         bat_speed_zscore +
                         release_speed_zscore +
                         launch_angle +
                         bat_speed_zscore:swing_length_zscore,
                       data = batting_data)

saveRDS(init_model, "initial_model.rds")


ui <- fluidPage(
  
    # TITLE
    titlePanel("Baseball Shiny App"),
    
    tags$p(
      "This application allows you to explore various baseball statistics for different players. ",
      br(),
      "You can select a player and view distributions of their hitting statistics such as launch angle, bat speed, swing length, and launch speed. ",
      br(),
      "Additionally, summary statistics will be provided to help analyze each player's performance.",
      br(),
      br(),  # Adds space before the bullet points
      tags$strong("Key Variables Used:"),
      br(),
      tags$ul(
        tags$li("Launch Angle: Vertical launch angle of the batted ball as tracked by Statcast. Refers to the angle at which the ball leaves the bat"),
        tags$li("Bat Speed: Measurement of how fast the bat is moving when the player makes contact with the ball"),
        tags$li("Swing Length: Total amount of feet the bat traveled during the swing (the total distance traveled by the barrel of the bat in x/y/z space)"),
      )
    ),
    
    selectizeInput("player_name",
                label = "Select a Player:",
                choices = unique(batting_data$player_name),
                selected = NULL
                ),
    
    # create distribution of launch speed, bat speed, swing length, launch angle, break down of hits? 
    checkboxGroupInput("stat", 
                label = "Select a Statistic to Plot",
                choices = c("launch_angle",
                            "bat_speed",
                            "swing_length"), 
                selected = NULL), 
    
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
    
    titlePanel("Model Development"),
    
    tags$p(
      "Another portion of this project involved model developement. A model was created to analyze the effect that bat speed, launch angle, swing length, and the picther's release speed have on the probability of a batter hitting a single, double or triple, or a homerun."
    ),
    
    # INPUT FOR INITIAL MODEL 
    # want to input real numbers and then turn them into z scores , add interaction, and plug into model 
    sliderInput("swing_length", "Input for Swing Length:", min = 1, max = 15, value = 1, step = .1),
    sliderInput("bat_speed", "Input for Bat Speed:", min = 10, max = 100, value = 10, step = .1),
    sliderInput("launch_angle", "Input for Launch Angle:", min = 0, max = 90, value = 0, step = .1),
    
    # submit button 
    actionButton("submit_button", "Make Predictions"),
    verbatimTextOutput("prediction")

)


server <- function(input, output, session) {
    
    # SELECTING A PLAYER 
    output$selected_value <- renderPrint({
      paste("You selected:", input$player_name)
    })
    
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
      selected_stats <- input$stat
      
      if(length(selected_stats) == 1){
        # histogram of distribution 
        stat_col <- selected_stats[1]

        stat_mean <- mean(data_filtered[[stat_col]], na.rm = TRUE)
        # creating plot 
        ggplot(data_filtered, aes_string(x = stat_col)) +
          geom_density(alpha = 0.6, fill = "lightblue") + 
          labs(title = paste("Density Plot of",
                             stat_col, "for",
                             input$batter), 
               x = stat_col, 
               y = "Density") +
          geom_vline(aes(xintercept = stat_mean),
                     color = "blue",
                     linetype = "dashed",
                     size = 1) +
          theme_minimal()
      
      }else if(length(selected_stats) == 2){
        x <- selected_stats[1]
        y <- selected_stats[2]

          # scatter plot 
        ggplot(data = data_filtered,
               mapping = aes_string(x = x,
                             y = y)) +
          geom_point(alpha = 0.6) +
          labs(title = paste("Scatterplot: ", x, " vs ", y),
               x = x,
               y = y) +
          theme_minimal()
        
      } else if(length(selected_stats) == 3){
        x <- selected_stats[1]
        y <- selected_stats[2]
        z <- selected_stats[3]
          # 3 d 
        ggplot(data = data_filtered,
               mapping = aes_string(x = x,
                             y = y,
                             size = z)) +
          geom_point(alpha = 0.6) +
          labs(title = paste("Scatterplot: ", x, " vs ", y),
               x = x,
               y = y) +
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
  
    
    # SETTING VALUE OF SLIDER TO BE AVERAGE OF PLAYER SELECTED 
    observe({
      
      # getting filtered data and means 
      data_filtered = filtered_data()
      
      mean_la <- mean(data_filtered$launch_angle, na.rm = TRUE)
      mean_sl <- mean(data_filtered$swing_length, na.rm = TRUE)
      mean_bs <- mean(data_filtered$bat_speed, na.rm = TRUE)
      
      # updating values 
      updateSliderInput(
        session,
        "swing_length",
        value = mean_sl
      )
      
      updateSliderInput(
        session,
        "bat_speed",
        value = mean_bs
      )
      
      updateSliderInput(
        session,
        "launch_angle",
        value = mean_la
      )
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
      
      output$prediction <- renderText({
        paste(
          "predicted hit outcome: ", prediction
        )
      })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
