library(shiny)
library(bslib)
library(vetiver)
library(tibble)
library(ggplot2)
library(dplyr)

api_url <- "http://127.0.0.1:8080/predict"

# Used for Visualizations and Metrics

FM_Housing <- read.csv("FM_Housing_2018_2022_clean.csv") %>%
  mutate(Start.Date = as.Date(Start.Date, format = "%Y-%M-%d"),
         Sold.Date = as.Date(Sold.Date, format = "%Y-%M-%d"),
         Property.Type = as.factor(Property.Type),
         Book.Section = as.factor(Book.Section),
         City = as.factor(City),
         State.Province = as.factor(State.Province),
         County = as.factor(County),
         Style = as.factor(Style),
         Subdivision = as.factor(Subdivision),
         Laundry.Location = as.factor(Laundry.Location),
         High.School = as.factor(High.School)) %>%
  filter(Year.Built >= 1800 & Year.Built <= 2022)

housing_model <- lm(Sold.Price ~ Sold.Date+Book.Section+Postal.Code+Total.SqFt.+Year.Built+Style+Total.Bedrooms+Total.Bathrooms+Garage.Stalls, data = FM_Housing)
model_summary <- summary(housing_model)


# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("FM Housing Shiny Dashboard"),

    # Side-Navigation for the Three Pages: Predictions, Visualizations, and Metrics: https://shiny.posit.co/r/getstarted/build-an-app/customizing-ui/tabs.html
    navset_pill_list(
      # Nav Panel for Sold Price Predictions using the Vetiver API
      nav_panel(
        "Predictions",
        layout_columns(
          card(
            h2("House Parameters"),
            dateInput("Sold.Date",
                      "Sold Date",
                      min = as.Date("2018-01-01",format="%Y-%m-%d"),
                      max = as.Date("2022-12-31",format="%Y-%m-%d"),
                      value = as.Date("2020-03-13",format="%Y-%m-%d")
            ),
            selectInput("Book.Section",
                        "Booking Section",
                        c("Duplex","Lake Property","Single Family Residence","Townhouse","Twinhomes"),
                        selected = "Single Family Residence"
            ),
            selectInput("Postal.Code",
                        "Zip Code",
                        c("56560","58078","58102","58103","58104"),
                        selected = "58103"
            ),
            sliderInput("Total.SqFt.",
                        "Total House Square Footage",
                        min = 480,
                        max = 10693,
                        value = 2300,
                        step = 50
            ),
            sliderInput("Year.Built",
                        "Construction Year",
                        min = 1850,
                        max = 2022,
                        value = 1998,
                        step = 1
            ),
            selectInput("Style",
                        "House Style",
                        c("1 Story","2 Story","3 Level","3 Story","4 Level","Bi Level","None")
            ),
            sliderInput("Total.Bedrooms",
                        "Number of Bedrooms",
                        min = 0,
                        max = 9,
                        value = 3,
                        step = 1
            ),
            sliderInput("Total.Bathrooms",
                        "Number of Bathrooms",
                        min = 0,
                        max = 9,
                        value = 2,
                        step = 1
            ),
            sliderInput("Garage.Stalls",
                        "Number of Garage Stalls",
                        min = 0,
                        max = 3,
                        value = 2,
                        step = 1
            ),
          ),
          #verbatimTextOutput("vals"),
          card(
            h2("Predicted House Sold Price ($USD)"),
            # Get model predictions
            actionButton(
              "predict",
              "Predict"
            ),
            textOutput("pred")
          ),
        )
      ),
      # Nav Panel to hold static graphics
      nav_panel(
        "Visualizations",
        plotOutput("total.sqft.histogram"),
        sliderInput("sqft_bins",
                    "Number of bins:",
                    min = 10,
                    max = 200,
                    value = 50),
        layout_columns(
          card(
            plotOutput("bed_histogram")
          ),
          card(
            plotOutput("bath_histogram")
          ),
          card(
            plotOutput("garage_histogram")
          )
        )
      ),
      # Nav Panel to hold static model performance metrics
      nav_panel(
        "Metrics",
        h2("Model Statistics"),
        verbatimTextOutput("model_metrics")
      )
    ), # End Navset Pill List
    
) #end UI setup

# Define server logic
server <- function(input, output) {
  # Input params
  vals <- reactive(
    tibble(
      Sold.Date = input$Sold.Date,
      Book.Section = input$Book.Section,
      Postal.Code = input$Postal.Code,
      Total.SqFt. = input$Total.SqFt.,
      Year.Built = input$Year.Built,
      Style = input$Style,
      Total.Bedrooms = input$Total.Bedrooms,
      Total.Bathrooms = input$Total.Bathrooms,
      Garage.Stalls = input$Garage.Stalls
    )
  )
  
  # Metrics Tibble to Print
  metrics_tible <- tibble(
    Formula = as.character(model_summary$call[2]),
    R.Squared = model_summary$r.squared,
    Adjusted_R.Squared = model_summary$adj.r.squared
  )
  
  # Fetch prediction from API
  pred <- eventReactive(
    input$predict,
    httr2::request(api_url) |>
      httr2::req_body_json(vals()) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |> print(),
    ignoreInit = TRUE
  )
  # pred <- eventReactive(
  #   input$predict,
  #   predict(vetiver_endpoint(api_url),vals()),
  #   ignoreInit = TRUE
  #   )
  
  # Render to UI
  output$pred <- renderText(paste0("$",round(pred()$.pred[[1]],2)))
  output$vals <- renderPrint(vals())
  output$total.sqft.histogram <- renderPlot({
    ggplot(data = FM_Housing, aes(x = Total.SqFt.)) +
      geom_histogram(bins = input$sqft_bins) +
      labs(x = "Total Square Footage",
           y = "Number of Homes",
           title = "Spread of Home Total Square-Footages")
  })
  output$bed_histogram <- renderPlot({
    ggplot(data = FM_Housing, aes(x = Total.Bedrooms)) +
      geom_histogram(bins = 10) +
      labs(x = "Number of Bedrooms",
           y = "Number of Homes",
           title = "Bedrooms")
  })
  output$bath_histogram <- renderPlot({
    ggplot(data = FM_Housing, aes(x = Total.Bathrooms)) +
      geom_histogram(bins = 10) +
      labs(x = "Number of Bathrooms",
           y = "Number of Homes",
           title = "Bathrooms")
  })
  output$garage_histogram <- renderPlot({
    ggplot(data = FM_Housing, aes(x = Garage.Stalls)) +
      geom_histogram(bins = 11) +
      labs(x = "Number of Garage Stalls",
           y = "Number of Homes",
           title = "Garage Stalls")
  })
  output$model_metrics <- renderPrint(metrics_tible)
}

# Run the application 
shinyApp(ui = ui, server = server)
