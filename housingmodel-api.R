# Load in Libraries
library(dplyr)
library(dbplyr)
library(vetiver)
library(pins)
library(plumber)

# Legacy DuckDB Setup

# con <- DBI::dbConnect(
#   duckdb::duckdb(), 
#   dbdir = "my-db.duckdb"
# )
# df <- dplyr::tbl(con, "penguins")

# Get Data

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
#FM_Listings <- read.csv("FM_Housing_ActiveListings_clean.csv")

summary(FM_Housing)

## Define Model and Fit
housing_model <- lm(Sold.Price ~ Sold.Date+Book.Section+Postal.Code+Total.SqFt.+Year.Built+Style+Total.Bedrooms+Total.Bathrooms+Garage.Stalls, data = FM_Housing)
summary(housing_model)

## Turn into Vetiver Model - allows you to make models into an API
vet <- vetiver_model(housing_model, model_name='FMHousing_Model')

## Save to Board
model_board <- board_folder("/data/model", versioned = TRUE)
model_board %>% vetiver_pin_write(vet)

## Turn model into API - R runs the API as its primary objective, so select "Source as Background Job" to run it in the background
pr() %>%
  vetiver_api(vet) %>%
  pr_run(port = 8080)

#DBI::dbDisconnect(con)
