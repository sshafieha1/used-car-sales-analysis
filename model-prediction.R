library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggplot2)
library(patchwork)

# 1. Load the cleaned data
car_sales <- read_csv("car_sales_past90days_FINAL.csv")

# 2. EDA - Visualizing Interactions
# Interaction: Price vs Miles by Affordability Tier
p1 <- ggplot(car_sales, aes(x = miles, y = price, color = affordability_tier)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Price vs Mileage by Tier", x = "Mileage", y = "Price", color = "Tier") +
  theme_minimal()

# Interaction: Price over Time
p2 <- ggplot(car_sales, aes(x = as.Date(last_seen_date), y = price, color = affordability_tier)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Price Trends Over Time", x = "Date", y = "Price") +
  theme_minimal()

# Interaction: Days on Market vs Price
p3 <- ggplot(car_sales, aes(x = dom_active, y = price, color = affordability_tier)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Price vs Days on Market", x = "Days on Market", y = "Price") +
  theme_minimal()

# Boxplot of Price by Tier
p4 <- ggplot(car_sales, aes(x = affordability_tier, y = price, fill = affordability_tier)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Price Distribution by Tier", x = "Tier", y = "Price") +
  theme_minimal() +
  theme(legend.position = "none")

# Save EDA plots individually
ggsave("eda_plot1.png", p1, width = 8, height = 6)
ggsave("eda_plot2.png", p2, width = 8, height = 6)
ggsave("eda_plot3.png", p3, width = 8, height = 6)
ggsave("eda_plot4.png", p4, width = 8, height = 6)


# 3. Model Preparation
start_date <- min(car_sales$last_seen_date)
car_model_data <- car_sales %>%
  mutate(
    days_since_start = as.numeric(difftime(last_seen_date, start_date, units = "days")),
    seller_type = as.factor(seller_type),
    affordability_tier = as.factor(affordability_tier)
  ) %>%
  select(price, miles, dom_active, dos_active, seller_type, affordability_tier, days_since_start, sold_month, sold_week)

# Remove zero-variance predictors manually
car_model_data <- car_model_data %>%
  select(where(~ n_distinct(.) > 1))

# 4. Data Splitting
set.seed(123)
car_split <- initial_split(car_model_data, prop = 0.8, strata = affordability_tier)
train_data <- training(car_split)
test_data  <- testing(car_split)

# 5. Recipe and Model Specification
car_recipe <- recipe(price ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Random Forest Specification (using ranger)
rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# 6. Workflow and Tuning
car_workflow <- workflow() %>%
  add_recipe(car_recipe) %>%
  add_model(rf_spec)

car_folds <- vfold_cv(train_data, v = 5, strata = affordability_tier)

# Simple grid for RF
rf_grid <- grid_regular(
  mtry(range = c(2, 5)),
  min_n(range = c(2, 10)),
  levels = 4
)

cat("Starting model tuning...\n")
tune_results <- tune_grid(
  car_workflow,
  resamples = car_folds,
  grid = rf_grid
)

# 7. Finalize Model
best_params <- select_best(tune_results, metric = "rmse")
final_wf <- finalize_workflow(car_workflow, best_params)
final_fit <- last_fit(final_wf, car_split)

# 8. Evaluation
metrics <- collect_metrics(final_fit)
predictions <- collect_predictions(final_fit)

print(metrics)

# 9. Future Prediction (Next 90 Days)
last_day <- max(car_model_data$days_since_start, na.rm = TRUE)
last_miles <- median(train_data$miles, na.rm = TRUE)
future_days <- seq(last_day + 1, last_day + 90)
categories <- levels(train_data$affordability_tier)

future_grid <- expand_grid(
  days_since_start = future_days,
  affordability_tier = categories
) %>%
  mutate(
    # Simulate realistic depreciation: assume car is driven ~40 miles per day
    miles = last_miles + (days_since_start - last_day) * 40,
    dom_active = median(train_data$dom_active, na.rm = TRUE) + (days_since_start - last_day),
    dos_active = median(train_data$dos_active, na.rm = TRUE)
  )

# Calculate sold_month and sold_week for future dates
start_date_obj <- as.Date(start_date)
future_grid <- future_grid %>%
  mutate(
    date = start_date_obj + days_since_start,
    sold_month = month(date),
    sold_week = week(date)
  )

# Handle seller_type if it still exists in predictors
if ("seller_type" %in% names(train_data)) {
  future_grid$seller_type <- levels(train_data$seller_type)[1]
}

# Predict future prices
final_model <- extract_workflow(final_fit)
future_preds <- predict(final_model, future_grid) %>%
  bind_cols(future_grid)

# 10. Visualization of Predictions
p5 <- ggplot(future_preds, aes(x = date, y = .pred, color = affordability_tier)) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "lm", linetype = "dashed", alpha = 0.1, formula = y ~ x) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "90-Day Price Forecast by Category",
       subtitle = "Random Forest Prediction (Median Vehicle Attributes)",
       x = "Future Date", y = "Predicted Price",
       color = "Tier") +
  theme_minimal()

ggsave("future_forecast.png", p5, width = 10, height = 6)

# Save results
saveRDS(final_model, "car_price_model.rds")
write_csv(future_preds, "car_sales_predictions_90days.csv")

cat("\nModel training and forecasting complete.\n")
cat("RMSE:", metrics %>% filter(.metric == "rmse") %>% pull(.estimate), "\n")
cat("Rsq:", metrics %>% filter(.metric == "rsq") %>% pull(.estimate), "\n")
