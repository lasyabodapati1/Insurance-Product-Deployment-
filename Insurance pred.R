#1 Load & Explore the Data set in R
#install the required libraries
install.packages("tidyverse") #data viz data manipulation
install.packages("caret") #model building 
install.packages("randomForest") #for machine learning 

#call the libraries for the project
library(tidyverse)
library(caret)
library(randomForest)

#read the datasets 
train <- read_csv("C:\\Users\\lasya\\Downloads\\archive (2)\\train.csv")
test <- read_csv("C:\\Users\\lasya\\Downloads\\archive (2)\\test.csv")

# view the structure of the data
str(train)
summary(train)

#2. Data Cleaning & Prep
#convert the categorical variables to factors 
train$is_claim <- as.factor(train$is_claim)
train$fuel_type <- as.factor(train$fuel_type)
train$segment <- as.factor(train$segment)
train$transmission_type <- as.factor(train$transmission_type)

#handle missing values 
sum(is.na(train))
#comment - there are no missing values in the data. 

#3. Exploratory Data Analysis (EDA)
#visaualizing claim distribution 
ggplot(train, aes(x = is_claim)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Distribution of Claims")

#analyze claim rates by car age 
ggplot(train, aes(x = age_of_car, fill = is_claim)) + 
  geom_histogram(binwidth = 1, position = "dodge") +
  ggtitle("Claim Rate by Car Age")

#analyze claim rate by Fuel type 
ggplot(train, aes(x = fuel_type, fill = is_claim)) + 
  geom_bar(position = "dodge") +
  ggtitle("Claim Rate by Fuel Type")

#4. 
#split the data into train and test
set.seed(123)
train_index <- createDataPartition(train$is_claim, p = 0.8, list = FALSE)
train_set <- train[train_index, ]
test_set <- train[-train_index, ]

#training a random forest
rf_model <- randomForest(is_claim ~ age_of_car + policy_tenure + ncap_rating + fuel_type,
                         data = train_set, ntree = 100)

#evaluate model performance 
predictions <- predict(rf_model, test_set)
confusionMatrix(predictions, test_set$is_claim)

#5. Dashboard 
install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Insurance Policy Deployment Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("segment", "Select Segment:", choices = unique(train$segment))
    ),
    mainPanel(
      plotOutput("claim_plot")
    )
  )
)

server <- function(input, output) {
  output$claim_plot <- renderPlot({
    filtered_data <- train %>% filter(segment == input$segment)
    ggplot(filtered_data, aes(x = age_of_car, fill = is_claim)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      ggtitle(paste("Claim Rate for Segment:", input$segment))
  })
}

shinyApp(ui = ui, server = server)

