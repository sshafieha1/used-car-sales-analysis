library(tidyverse)
library(dplyr)
library(tidyr)
library(tidymodels)
library(ggplot2)

cat("Loading car_sales_past90days_FINAL.csv...\n")
car_sales <- read_csv("car_sales_past90days_FINAL.csv", show_col_types = FALSE)

# Ensure necessary column types for KNN imputation
car_sales <- car_sales %>%
  mutate(
    price = as.numeric(price),
    miles = as.numeric(miles),
    dom_active = as.numeric(dom_active),
    dos_active = as.numeric(dos_active)
  )

#----------------------------DATA CLEANING-------------------------------------
cat("Preparing and running KNN imputation recipe...\n")

# 1. Define the recipe using ALL columns
car_recipe <- recipe(price ~ ., data = car_sales) %>%
  # We MUST tell R not to use IDs/dates as predictors (they are unique/metadata)
  update_role(
    id, vin, last_seen_at, first_seen_at, last_seen_date, first_seen_date, 
    make, year, body_type, exterior_color, seller_type,
    days_on_market, sold_month, sold_week, sold_dow, sold_quarter,
    new_role = "ID"
  ) %>%
  # Step A: Fix missing miles using relevant predictors
  step_impute_knn(miles, neighbors = 5, impute_with = imp_vars(dom_active, dos_active)) %>%
  # Step B: Fix missing prices using the fixed miles
  step_impute_knn(price, neighbors = 5, impute_with = imp_vars(miles, dom_active, dos_active))

# 2. Process the data
car_sales_final <- car_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% # 'new_data = NULL' returns the original data with fixes
  # 3. Categorize using CA affordability benchmarks
  mutate(affordability_tier = case_when(
    price < 30000 ~ "Affordable",        # CA avg used price is ~$29k-$31k
    price >= 30000 & price <= 45000 ~ "Mid-Range", # Popular SUVs and Entry-Luxury
    price > 45000 ~ "Luxury",            # High-end & outliers
    TRUE ~ "Unknown"
  ))

# 4. Save the cleaned and imputed dataset
cat("Saving cleaned dataset back to car_sales_past90days_FINAL.csv...\n")
write_csv(car_sales_final, "car_sales_past90days_FINAL.csv")

cat("Cleaned dataset saved successfully!\n")
cat("Missing values count after cleaning:\n")
print(colSums(is.na(car_sales_final)))