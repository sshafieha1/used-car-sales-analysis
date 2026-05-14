library(tidyverse)
library(dplyr)
library(tidyr)
library(tidymodels)
library(ggplot2)
car_sales <- read_csv("car_sales_past90days_4k.csv")

#----------------------------DATA CLEANING-------------------------------------

# 1. Define the recipe using ALL columns (the "." does this)
car_recipe <- recipe(price ~ ., data = car_sales) %>%
  # We MUST tell R not to use 'id' or 'vin' as predictors (they are unique/random)
  update_role(id, vin, last_seen_at, first_seen_at, last_seen_date, first_seen_date, new_role = "ID") %>%
  
  # Step A: Fix the 81 missing miles using relevant predictors
  step_impute_knn(miles, neighbors = 5, impute_with = imp_vars(dom_active, dos_active, seller_type)) %>%
  
  # Step B: Fix the 276 missing prices using the fixed miles
  step_impute_knn(price, neighbors = 5, impute_with = imp_vars(miles, dom_active, dos_active, seller_type))

# 2. Process the data
car_sales_final <- car_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% # 'new_data = NULL' returns the original data with fixes
  
  # 3. Categorize using CA affordability benchmarks
  # Sources: Edmunds Used Car Report & iSeeCars California Trends
  mutate(affordability_tier = case_when(
    price < 30000 ~ "Affordable",        # CA avg used price is ~$29k-$31k
    price >= 30000 & price <= 45000 ~ "Mid-Range", # Popular SUVs and Entry-Luxury
    price > 45000 ~ "Luxury",            # High-end & outliers (up to your $500k car)
    TRUE ~ "Unknown"
  ))

names(car_sales_final)
car_sales_final %>% select(vin, price, affordability_tier) %>% filter (price > 400000)
colSums(is.na(car_sales_final))

#------------------------------VIZUALIZATIONS---------------------------------------

ggplot(car_sales_final, aes(x = miles, y = price, color = affordability_tier)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + # Trend line for 90-day depreciation
  labs(title = "Predicted Price Decay by Mileage",
       subtitle = "California Market Trends (May - August 2026)") +
  theme_minimal()

ggplot(car_sales_final, aes(x = affordability_tier, y = dom_active, fill = affordability_tier)) +
  geom_boxplot() +
  labs(title = "Inventory Supply: Days on Market by Tier",
       y = "Active Days on Market") +
  theme_classic()

names(car_sales_final)


#------------------------------------------------------------------------------------