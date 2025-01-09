# Install the packages mentioned in line 2 to install the following packages required to run the project.
#install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "wordcloud", "tm", "reshape2", "shinycssloaders"))

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(wordcloud)
library(tm)
library(reshape2)
library(shinycssloaders)
# Read the CSV file
tweets_data <- read.csv("covid19_tweets.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        font-weight:40px;
        padding: 10px 0;
        background-image: url('https://hygger.io/wp-content/uploads/2018/03/b15e54f0ca9ff7c12fe69563e4e8b3a4-scaled.jpg'); /* Background image */
        background-size: cover; /* Fit the entire image within the container */
        background-repeat: no-repeat; /* Prevent image from repeating */
        background-position: center;
        text-align: center;
        color: #001f3f;
        z-index:-1;
          box-shadow: 10px 10px 5px lightblue;
        border:2px solid red;
      }
       /* Styling for tabsetPanel */
      #custom-tabset{
      background-color: #f8f9fa; /* Background color for tab content */
          box-shadow: 10px 10px 5px lightblue;
 /* Border color for tab content */
        border-radius: 0 0 10px 10px; /* Rounded bottom corners for tab content */
        padding: 10px; 
        margin-bottom:10px;
      }
      .main-title {
        font-family:'Times New Roman'
        font-size: 30px; /* Font size */
        font-weight: bolder; /* Bold font weight */
        color: #yellow; /* Text color */
        background-image: url('https://as2.ftcdn.net/v2/jpg/03/49/64/63/1000_F_349646381_8MvLKpRfrlLs2j5NlD3gPUNuhzHqKHkJ.jpg'); /* Background image */
        background-size: cover; /* Fit the entire image within the container */
        background-repeat: no-repeat; /* Prevent image from repeating */
        background-position: center;
        padding: 20px; /* Padding */
        text-align: center; /* Center align text */
        border-radius: 10px; /* Rounded corners */
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); /* Box shadow */
        margin-bottom: 20px; /* Bottom margin */
      }
      h5:hover{
      transform:scale(1.5);
      cursor:pointer;
      background-color:white;
      color:black;
      }
    "))
  ),
  div(class = "main-title", titlePanel("COVID-19 Tweets Analysis")),
  
  mainPanel(
    tabsetPanel(
      id = "custom-tabset",  # Unique ID for tabsetPanel
      tabPanel("Scatter Plot", withSpinner(plotOutput("scatter_plot"))),
      tabPanel("Geospatial Analysis", withSpinner(plotlyOutput("geospatial_plot"))),
      tabPanel("Sentiment Distribution", withSpinner(plotOutput("sentiment_barplot"))),
      tabPanel("Time Series Analysis", withSpinner(plotOutput("time_series_plot"))),
      tabPanel("Word Cloud",withSpinner(plotOutput("wordcloud_plot")), height="600px"),
      tabPanel("Correlation Heatmap", withSpinner(plotOutput("correlation_heatmap"))),
      tabPanel("Pie Chart of User Verified Status", withSpinner(plotOutput("verified_pie_chart"))),
      tabPanel("Box Plot", withSpinner(plotOutput("box_plot"))),
      tabPanel("Line Chart", withSpinner(plotOutput("line_chart"))),
      tags$div(class = "footer",
               h5("2110030317 SAYYED SAMEER BASIR")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Perform data manipulation and create scatter plot
  output$scatter_plot <- renderPlot({
    tweets <- tweets_data
    
    # Filter out tweets with 0 followers or 0 favorites (optional)
    tweets <- tweets %>%
      filter(user_followers > 0, user_favourites > 0)
    
    # Create a scatter plot
    ggplot(tweets, aes(x = user_followers, y = user_favourites, color = user_verified)) +
      geom_point(size = 3) +
      labs(title = "Relationship between Followers and Favorites",
           x = "Number of Followers",
           y = "Number of Favorites") +
      scale_color_manual(values = c("blue", "red")) +
      theme_minimal()
  })
  
  # Perform data manipulation and create word cloud
  output$wordcloud_plot <- renderPlot({
    tweets <- tweets_data
    
    # Combine text from all tweets
    text_corpus <- Corpus(VectorSource(tweets$text))
    
    # Clean text (remove punctuation, convert to lowercase, remove stopwords)
    text_corpus <- tm_map(text_corpus, content_transformer(tolower))
    text_corpus <- tm_map(text_corpus, removePunctuation)
    text_corpus <- tm_map(text_corpus, removeNumbers)
    text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))
    
    # Convert corpus to a character vector
    text <- unlist(sapply(text_corpus, as.character))
    
    # Generate word cloud
    wordcloud(text, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  })
  # Perform sentiment analysis and create interactive bar plot
  output$sentiment_barplot <- renderPlot({
    tweets <- tweets_data
    
    # Perform sentiment analysis (for demonstration, let's assume random sentiment scores)
    set.seed(123)
    tweets$sentiment <- sample(c("Positive", "Negative", "Neutral"), nrow(tweets), replace = TRUE)
    
    # Calculate sentiment distribution
    sentiment_counts <- table(tweets$sentiment)
    sentiment_data <- data.frame(sentiment = names(sentiment_counts), count = as.numeric(sentiment_counts))
    
    # Create a bar plot
    ggplot(data = sentiment_data, aes(x = sentiment, y = count, fill = sentiment)) +
      geom_bar(stat = "identity") +
      labs(title = "Sentiment Distribution of COVID-19 Tweets",
           x = "Sentiment",
           y = "Count") +
      scale_fill_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "blue")) +
      theme_minimal() +
      geom_text(aes(label = count), vjust = -0.5, size = 4, color = "black", position = position_dodge(width = 0.9))+  # Add count labels
      theme(text = element_text(size = 12))
  })
  # Perform time series analysis and create time series plot
  output$time_series_plot <- renderPlot({
    tweets <- tweets_data
    
    # Convert date column to datetime format
    tweets$date <- as.POSIXct(tweets$date, format = "%Y-%m-%d %H:%M:%S")
    
    # Perform sentiment analysis (for demonstration, let's assume random sentiment scores)
    set.seed(123)
    tweets$sentiment <- sample(c("Positive", "Negative", "Neutral"), nrow(tweets), replace = TRUE)
    
    # Calculate sentiment score for each day
    daily_sentiment <- tweets %>%
      group_by(date) %>%
      summarize(sentiment_score = mean(ifelse(sentiment == "Positive", 1, ifelse(sentiment == "Negative", -1, 0))))
    
    # Create time series plot
    ggplot(daily_sentiment, aes(x = date, y = sentiment_score, color = sentiment_score)) +
      geom_line() +
      labs(title = "Sentiment Score over Time",
           x = "Date",
           y = "Sentiment Score",
           color = "Sentiment Score") +
      scale_color_gradient(low = "blue", high = "red") +  # Add color gradient
      theme_minimal()
  })
  # Perform geospatial analysis and create map plot
  output$geospatial_plot <- renderPlotly({
    # Filter tweets with sufficiently detailed user_location data
    tweets_with_location <- tweets_data %>%
      filter(!is.na(user_location))
    
    # Perform sentiment analysis (for demonstration, let's assume random sentiment scores)
    set.seed(123)
    tweets_with_location$sentiment <- sample(c("Positive", "Negative", "Neutral"), nrow(tweets_with_location), replace = TRUE)
    
    # Aggregate sentiment scores by location
    sentiment_by_location <- tweets_with_location %>%
      group_by(user_location) %>%
      summarize(sentiment_score = mean(ifelse(sentiment == "Positive", 1, ifelse(sentiment == "Negative", -1, 0))))
    
    # Create map plot
    plot_ly(data = sentiment_by_location, 
            type = 'scattergeo', 
            locationmode = 'country names', 
            locations = ~user_location, 
            text = ~paste("Location: ", user_location, "<br>", "Sentiment Score: ", sentiment_score), 
            marker = list(color = ~sentiment_score, colorscale = 'Viridis'), 
            color = ~sentiment_score) %>%
      layout(title = 'Sentiment Distribution by Location', geo = list(showframe = FALSE, projection = list(type = 'natural earth')))
  })
    # Perform correlation analysis and create correlation heatmap
    output$correlation_heatmap <- renderPlot({
      # Select numerical fields for correlation analysis
      numerical_fields <- tweets_data[, c("user_followers", "user_friends", "user_favourites")]
      
      # Calculate correlation matrix
      correlation_matrix <- cor(numerical_fields)
      
      # Melt correlation matrix for visualization
      melted_correlation <- melt(correlation_matrix)
      
      # Plot heatmap
      ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal() +
        labs(title = "Correlation Heatmap of Numerical Fields",
             x = "Variables",
             y = "Variables")
    })
    # Bar Chart of User Followers
    output$followers_bar_chart <- renderPlot({
      # Calculate dynamic scale for x-axis
      max_followers <- max(tweets_data$user_followers)
      scale <- ifelse(max_followers > 10000, "log", "continuous")
      
      # Create a clear and colorful bar chart of user followers
      ggplot(tweets_data, aes(x = user_followers, fill = user_verified)) +
        geom_bar() +
        scale_fill_manual(values = c("blue", "red"), labels = c("Unverified", "Verified")) +  # Color by user verification status
        labs(title = "Distribution of User Followers",
             x = "User Followers",
             y = "Count") +
        theme_minimal() +  # Minimal theme for clarity
        theme(legend.position = "top", 
              axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
              axis.title = element_text(size = 12),  # Increase font size of axis titles
              axis.text = element_text(size = 10),  # Increase font size of axis labels
              plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins to ensure all details are visible
        scale_x_continuous(trans = scale)  # Set scale for x-axis
    })
    # Box Plot
    output$box_plot <- renderPlot({
      # Select applicable fields for the box plot
      numerical_fields <- tweets_data[, c("user_followers", "user_friends", "user_favourites")]
      
      # Create box plot
      boxplot(numerical_fields, 
              main = "Box Plot of User Attributes", 
              xlab = "User Attributes",
              ylab = "Values",
              ylim = c(0, 10000),  # Adjust y-axis limits for better clarity
              col = c("blue", "green", "red"),  # Customize box plot colors
              border = c("black", "black", "black"),  # Set border color of boxes
              notch = TRUE)  # Add notches to box plot for confidence intervals
    })
    # Line Chart
    output$line_chart <- renderPlot({
      # Select relevant fields for the line chart (e.g., date and user_followers)
      line_data <- tweets_data[, c("date", "user_followers")]
      
      # Convert date column to datetime format
      line_data$date <- as.POSIXct(line_data$date, format = "%Y-%m-%d %H:%M:%S")
      
      # Sort data by date
      line_data <- line_data[order(line_data$date), ]
      
      # Create line chart with clear and colorful aesthetics
      ggplot(line_data, aes(x = date, y = user_followers, color = "User Followers")) +
        geom_line() +
        labs(title = "User Followers Over Time",
             x = "Date",
             y = "User Followers",
             color = "Legend Title") +
        theme_minimal() +  # Minimal theme for clarity
        theme(plot.title = element_text(size = 16),  # Increase font size of title
              axis.text = element_text(size = 12),  # Increase font size of axis labels
              legend.title = element_text(size = 12),  # Increase font size of legend title
              legend.text = element_text(size = 10),  # Increase font size of legend text
              panel.background = element_rect(fill = "lightblue"),  # Set background color
              panel.grid.major = element_blank(),  # Remove major grid lines
              panel.grid.minor = element_blank())  # Remove minor grid lines
    })
    # Pie Chart of User Verified Status
    output$verified_pie_chart <- renderPlot({
      ggplot(tweets_data, aes(x = "", fill = user_verified)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Distribution of User Verified Status")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
