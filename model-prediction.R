library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
# NOTE: MASS is intentionally NOT loaded — it shadows dplyr::select().
# Box-Cox is handled via step_log() inside tidymodels recipes.

# ── 0. Configuration ─────────────────────────────────────────────────────────
set.seed(123)
RSCRIPT_PATH <- "E:/Program Files/R/R-4.4.1/bin/Rscript.exe"

# ── 1. Load & Pre-clean Data ─────────────────────────────────────────────────
cat("=== LOADING DATA ===\n")
car_raw <- read_csv("car_sales_past90days_FINAL.csv", show_col_types = FALSE)
cat(sprintf("Raw rows: %d | Cols: %d\n", nrow(car_raw), ncol(car_raw)))

car_raw <- car_raw %>%
  mutate(
    last_seen_date  = as.Date(last_seen_date),
    first_seen_date = as.Date(first_seen_date),
    year            = as.numeric(year),
    price           = as.numeric(price),
    miles           = as.numeric(miles),
    dom_active      = as.numeric(dom_active),
    dos_active      = as.numeric(dos_active),
    make            = as.factor(toupper(trimws(make))),
    body_type       = as.factor(trimws(body_type))
  )

# ── 2. Data Filtering / Quality Fence ────────────────────────────────────────
cat("\n=== APPLYING DATA QUALITY FILTERS ===\n")

# seller_type is 100% 'dealer' — zero-variance, drop it
# dom_active has outlier of 988 days — cap at 365 (1 year)
# price < $3k are near-scrap vehicles — exclude from model
# price > $100k are exotic (29 rows) — insufficient for per-tier model, report separately

car_clean <- car_raw %>%
  filter(
    !is.na(price), !is.na(miles), !is.na(year), !is.na(make), !is.na(body_type),
    price >= 3000,                   # Remove near-scrap listings
    miles >= 1,                      # Remove zero-mile anomalies (test drives etc.)
    year  >= 1990                    # Safety guard (min in data is already 1990)
  ) %>%
  mutate(
    dom_active = pmin(dom_active, 365)   # Cap DOM at 1 year (outlier control)
  ) %>%
  dplyr::select(-any_of(c(
    "seller_type",     # Zero-variance — 100% dealer
    "exterior_color",  # 6.3% missing, not predictive of price
    "id",              # Unique row ID, not a feature
    "last_seen_at",    # Unix timestamp (Date col already parsed)
    "first_seen_at",   # Unix timestamp (Date col already parsed)
    "sold_dow",        # Redundant with sold_week
    "sold_quarter",    # Redundant with sold_month
    "affordability_tier" # Old categorization — will recalculate below
  )))

cat(sprintf("After filtering: %d rows\n", nrow(car_clean)))

# ── 3. Revised Categorization ─────────────────────────────────────────────────
# Based on actual data distribution (see audit):
#   <$15k  → Budget      (~500 rows)
#   15-30k → Affordable  (~1,400 rows)
#   30-50k → Mid-Range   (~1,050 rows)
#   50-100k→ Premium     (~600 rows)
#   >100k  → Exotic      (29 rows — excluded from per-tier ML models)
#
# This avoids the "classic vs Maybach" problem because NO car in this dataset
# is older than 1990, so price alone is a valid discriminator here.
# The MAPE metric (not RMSE) ensures the Budget tier is evaluated fairly.

cat("\n=== TIER DISTRIBUTION (REVISED) ===\n")
car_clean <- car_clean %>%
  mutate(
    tier = case_when(
      price < 15000              ~ "Budget",
      price >= 15000 & price < 30000 ~ "Affordable",
      price >= 30000 & price < 50000 ~ "Mid-Range",
      price >= 50000 & price < 100000 ~ "Premium",
      price >= 100000            ~ "Exotic",
      TRUE                       ~ "Unknown"
    ),
    tier = factor(tier, levels = c("Budget", "Affordable", "Mid-Range", "Premium", "Exotic"))
  )
print(table(car_clean$tier))

# Exotic: report separately, do not train on (too few rows)
exotic_df <- car_clean %>% filter(tier == "Exotic")
cat(sprintf("\nExotic listings (price > $100k): %d rows — excluded from per-tier ML models\n", nrow(exotic_df)))
print(exotic_df %>% dplyr::select(year, make, body_type, price, miles) %>% arrange(desc(price)) %>% head(10))

# Working dataset: Budget through Premium only
car_model <- car_clean %>%
  filter(tier != "Exotic") %>%
  mutate(
    days_since_start = as.numeric(difftime(last_seen_date, min(last_seen_date), units = "days"))
  )

cat(sprintf("\nModeling dataset: %d rows\n", nrow(car_model)))

# ── 4. Box-Cox Transformation Check ──────────────────────────────────────────
cat("\n=== BOX-COX TRANSFORMATION CHECK ===\n")
# Visual: right-skewed price distribution
p_hist_raw <- ggplot(car_model, aes(x = price)) +
  geom_histogram(bins = 60, fill = "#2C3E50", alpha = 0.8) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Raw Price Distribution (Right-Skewed)", x = "Price", y = "Count") +
  theme_minimal()

# Log-transform (robust approximation of Box-Cox lambda≈0)
p_hist_log <- ggplot(car_model, aes(x = log(price))) +
  geom_histogram(bins = 60, fill = "#2C3E50", alpha = 0.8) +
  labs(title = "Log(Price) Distribution (Normalized)", x = "log(Price)", y = "Count") +
  theme_minimal()

ggsave("eda_boxcox_check.png", p_hist_raw + p_hist_log, width = 12, height = 5)
cat("Saved eda_boxcox_check.png\n")

# ── 5. EDA Plots (Updated) ────────────────────────────────────────────────────
cat("\n=== GENERATING EDA PLOTS ===\n")

p1 <- ggplot(car_model, aes(x = miles, y = price, color = tier)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~tier, scales = "free_y", nrow = 2) +
  labs(title = "Price vs Mileage by Tier", x = "Mileage", y = "Price") +
  theme_minimal() + theme(legend.position = "none")

p2 <- ggplot(car_model, aes(x = last_seen_date, y = price, color = tier)) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Price Trends Over Time", x = "Date", y = "Price", color = "Tier") +
  theme_minimal()

p3 <- ggplot(car_model, aes(x = dom_active, y = price, color = tier)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Price vs Days on Market", x = "Days on Market", y = "Price") +
  theme_minimal() + theme(legend.position = "none")

p4 <- ggplot(car_model, aes(x = tier, y = price, fill = tier)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.8) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Price Distribution by Tier (Revised)", x = "Tier", y = "Price") +
  theme_minimal() + theme(legend.position = "none")

ggsave("eda_plot1.png", p1, width = 10, height = 7)
ggsave("eda_plot2.png", p2, width = 8, height = 6)
ggsave("eda_plot3.png", p3, width = 8, height = 6)
ggsave("eda_plot4.png", p4, width = 8, height = 6)
cat("EDA plots saved.\n")

# ── 6. Helper: Metrics Function ───────────────────────────────────────────────
compute_metrics <- function(actual, predicted, tier_name) {
  rmse_val  <- sqrt(mean((actual - predicted)^2))
  mae_val   <- mean(abs(actual - predicted))
  mape_val  <- mean(abs((actual - predicted) / actual)) * 100
  rsq_val   <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  cat(sprintf(
    "\n--- %s ---\n  RMSE:  $%s\n  MAE:   $%s\n  MAPE:  %.2f%%\n  R²:    %.4f\n",
    tier_name,
    format(round(rmse_val), big.mark = ","),
    format(round(mae_val), big.mark = ","),
    mape_val,
    rsq_val
  ))
  
  tibble(
    tier = tier_name,
    rmse = rmse_val,
    mae  = mae_val,
    mape = mape_val,
    rsq  = rsq_val
  )
}

# ── 7. Per-Tier Model Training ─────────────────────────────────────────────────
cat("\n=== TRAINING PER-TIER MODELS ===\n")

tiers_to_model <- c("Budget", "Affordable", "Mid-Range", "Premium")
all_metrics    <- list()
all_preds      <- list()
all_models     <- list()

# Simplified body type (collapse long NHTSA names)
simplify_body <- function(b) {
  case_when(
    str_detect(b, "SUV|Multi-Purpose")  ~ "SUV",
    str_detect(b, "Sedan|Saloon")       ~ "Sedan",
    str_detect(b, "Pickup")             ~ "Pickup",
    str_detect(b, "Hatchback|Liftback|Notchback") ~ "Hatchback",
    str_detect(b, "Coupe")              ~ "Coupe",
    str_detect(b, "Convertible|Cabriolet") ~ "Convertible",
    str_detect(b, "Van|Minivan|Cargo")  ~ "Van",
    str_detect(b, "Wagon")             ~ "Wagon",
    str_detect(b, "Crossover|CUV")    ~ "SUV",  # treat CUV as SUV
    TRUE                               ~ "Other"
  )
}

for (tier_name in tiers_to_model) {
  cat(sprintf("\n>>> Training model for: %s\n", tier_name))
  
  tier_data <- car_model %>%
    filter(tier == tier_name) %>%
    mutate(
      body_simple = as.factor(simplify_body(as.character(body_type)))
    ) %>%
    dplyr::select(price, year, make, body_simple, miles, dom_active, dos_active,
           days_since_start, sold_month, sold_week) %>%
    filter(complete.cases(.))
  
  cat(sprintf("  Rows for this tier: %d\n", nrow(tier_data)))
  
  if (nrow(tier_data) < 100) {
    cat(sprintf("  SKIPPING: Too few rows (%d) for reliable training.\n", nrow(tier_data)))
    next
  }
  
  # Train/test split: 80/20
  tier_split <- initial_split(tier_data, prop = 0.8)
  train_t    <- training(tier_split)
  test_t     <- testing(tier_split)
  
  # Recipe with Box-Cox on price (step_log is a stable proxy for lambda≈0)
  tier_recipe <- recipe(price ~ ., data = train_t) %>%
    step_log(price, base = exp(1), skip = TRUE)  %>%  # Box-Cox (log transform, lambda→0); skip=TRUE so predict() doesn't look for price in new_data
    step_novel(all_nominal_predictors())  %>%
    step_other(make, threshold = 0.02)   %>%    # Collapse makes with <2% share
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())            %>%
    step_normalize(all_numeric_predictors(), -all_outcomes())
  
  rf_spec <- rand_forest(
    mtry  = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("regression")
  
  wf <- workflow() %>%
    add_recipe(tier_recipe) %>%
    add_model(rf_spec)
  
  folds <- vfold_cv(train_t, v = 5)
  
  # Adaptive mtry range based on number of predictors
  n_pred <- ncol(train_t) - 1
  rf_grid <- grid_regular(
    mtry(range = c(3, min(15, n_pred))),
    min_n(range = c(2, 10)),
    levels = 3
  )
  
  tune_res <- tune_grid(wf, resamples = folds, grid = rf_grid,
                        metrics = metric_set(rmse, rsq))
  
  best_params <- select_best(tune_res, metric = "rmse")
  final_wf    <- finalize_workflow(wf, best_params)
  final_fit   <- last_fit(final_wf, tier_split)
  
  # Back-transform predictions from log-space to dollar scale.
  # Note: with skip=TRUE, the test set's `price` column is raw dollars (not log-transformed),
  # so we only exp() the model's .pred output, NOT the actual price.
  raw_preds <- collect_predictions(final_fit) %>%
    mutate(
      pred_price   = exp(.pred),   # .pred is in log-space → convert back to dollars
      actual_price = price         # price is already in raw dollars (skip=TRUE bypassed the log step on test data)
    )
  
  # Metrics
  m <- compute_metrics(raw_preds$actual_price, raw_preds$pred_price, tier_name)
  all_metrics[[tier_name]] <- m
  
  # Save predictions with metadata
  all_preds[[tier_name]] <- raw_preds %>%
    mutate(tier = tier_name)
  
  # Save model
  all_models[[tier_name]] <- extract_workflow(final_fit)
}

# ── 8. Combined Metrics Table ─────────────────────────────────────────────────
cat("\n=== OVERALL MODEL PERFORMANCE SUMMARY ===\n")
metrics_df <- bind_rows(all_metrics)
print(metrics_df %>% mutate(
  rmse = dollar(round(rmse)),
  mae  = dollar(round(mae)),
  mape = sprintf("%.2f%%", mape),
  rsq  = sprintf("%.4f", rsq)
))

# ── 9. Classification F-Score (Tier Prediction Accuracy) ────────────────────
cat("\n=== CLASSIFICATION F-SCORE (Can the model identify the correct tier?) ===\n")
# Secondary check: train a classification RF on tier label
# Uses all data except Exotic
class_data <- car_model %>%
  mutate(
    body_simple = as.factor(simplify_body(as.character(body_type)))
  ) %>%
  dplyr::select(tier, year, make, body_simple, miles, dom_active, sold_month) %>%
  filter(complete.cases(.))

class_split  <- initial_split(class_data, prop = 0.8, strata = tier)
class_train  <- training(class_split)
class_test   <- testing(class_split)

class_recipe <- recipe(tier ~ ., data = class_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_other(make, threshold = 0.02)  %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

class_spec <- rand_forest(trees = 300, mtry = 8, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("classification")

class_wf <- workflow() %>%
  add_recipe(class_recipe) %>%
  add_model(class_spec)

class_fit    <- fit(class_wf, class_train)
class_preds  <- predict(class_fit, class_test) %>%
  bind_cols(class_test %>% dplyr::select(tier))

cat("\nConfusion Matrix:\n")
cm <- table(Predicted = class_preds$.pred_class, Actual = class_preds$tier)
print(cm)

# Per-class F-score
precision_recall_f1 <- function(cm, cls) {
  tp <- cm[cls, cls]
  fp <- sum(cm[cls, ]) - tp
  fn <- sum(cm[, cls]) - tp
  prec <- if (tp + fp == 0) 0 else tp / (tp + fp)
  rec  <- if (tp + fn == 0) 0 else tp / (tp + fn)
  f1   <- if (prec + rec == 0) 0 else 2 * prec * rec / (prec + rec)
  tibble(Tier = cls, Precision = round(prec, 3), Recall = round(rec, 3), F1 = round(f1, 3))
}

f_scores <- bind_rows(lapply(rownames(cm), function(cls) precision_recall_f1(cm, cls)))
cat("\nPer-Tier F-Scores:\n")
print(f_scores)
overall_acc <- sum(diag(cm)) / sum(cm)
cat(sprintf("\nOverall Classification Accuracy: %.2f%%\n", overall_acc * 100))

# ── 10. Spot-Check: Manually Entered Test Cases ───────────────────────────────
cat("\n=== SPOT-CHECK: MANUAL TEST CASES ===\n")
# Format: (year, make, body_simple, miles, dom_active, dos_active, sold_month, sold_week, days_since_start)
# Expected approximate prices are from market knowledge

spot_cases <- tribble(
  ~label,                              ~year, ~make,         ~body_simple, ~miles,  ~dom_active, ~dos_active, ~sold_month, ~sold_week, ~days_since_start,
  "2018 Toyota Camry (120k mi)",       2018,  "TOYOTA",       "Sedan",     120000,  30,          30,          5,           20,         85,
  "2022 BMW M5 (15k mi)",              2022,  "BMW",          "Sedan",     15000,   10,          10,          5,           20,         85,
  "2015 Honda Accord (80k mi)",        2015,  "HONDA",        "Sedan",     80000,   45,          45,          5,           20,         85,
  "2024 Ford F-150 (8k mi)",           2024,  "FORD",         "Pickup",    8000,    15,          15,          5,           20,         85,
  "2020 Mercedes C300 (35k mi)",       2020,  "MERCEDES-BENZ","Sedan",     35000,   20,          20,          5,           20,         85,
  "2012 Chevy Silverado (150k mi)",    2012,  "CHEVROLET",    "Pickup",    150000,  60,          60,          5,           20,         85,
  "2023 Porsche Cayenne (10k mi)",     2023,  "PORSCHE",      "SUV",       10000,   7,           7,           5,           20,         85,
  "2019 Nissan Altima (65k mi)",       2019,  "NISSAN",       "Sedan",     65000,   25,          25,          5,           20,         85
) %>%
  mutate(
    make        = as.factor(make),
    body_simple = as.factor(body_simple)
  )

cat("\n  Case                               | Predicted Price | Tier Used\n")
cat("  -----------------------------------|-----------------|----------\n")

for (i in seq_len(nrow(spot_cases))) {
  row <- spot_cases[i, ]
  
  # Determine expected tier
  # (We run through each tier model and use the one that matches)
  expected_tier <- case_when(
    row$miles > 100000 | (row$year < 2015 & row$miles > 80000) ~ "Budget",
    row$make %in% c("PORSCHE", "BENTLEY", "ROLLS-ROYCE", "FERRARI", "LAMBORGHINI") ~ "Premium",
    row$make %in% c("BMW", "MERCEDES-BENZ", "AUDI") & row$year >= 2020 ~ "Mid-Range",
    TRUE ~ "Affordable"
  )
  
  # Refine based on mileage/year combos
  expected_tier <- case_when(
    row$year >= 2022 & row$make %in% c("BMW", "MERCEDES-BENZ") ~ "Mid-Range",
    row$year >= 2023 & row$make %in% c("PORSCHE") ~ "Premium",
    TRUE ~ expected_tier
  )
  
  if (expected_tier %in% names(all_models)) {
    pred_log  <- predict(all_models[[expected_tier]], new_data = row)$.pred
    pred_price <- exp(pred_log)
    cat(sprintf("  %-36s | %s | %s\n",
                row$label,
                dollar(round(pred_price, -2)),
                expected_tier))
  } else {
    cat(sprintf("  %-36s | No model for tier: %s\n", row$label, expected_tier))
  }
}

# ── 11. 90-Day Forecast (Per Tier) ───────────────────────────────────────────
cat("\n=== GENERATING 90-DAY FORECAST ===\n")

last_day     <- max(car_model$days_since_start, na.rm = TRUE)
start_date   <- min(car_model$last_seen_date, na.rm = TRUE)
future_days  <- seq(last_day + 1, last_day + 90)

# Mode helper
get_mode <- function(v) {
  uv <- na.omit(unique(v))
  uv[which.max(tabulate(match(v, uv)))]
}

future_preds_list <- list()

for (tier_name in tiers_to_model) {
  if (!tier_name %in% names(all_models)) next
  
  tier_data <- car_model %>% filter(tier == tier_name)
  
  last_miles <- median(tier_data$miles, na.rm = TRUE)
  
  fg <- tibble(days_since_start = future_days) %>%
    mutate(
      date        = start_date + days_since_start,
      miles       = last_miles + (days_since_start - last_day) * 40,
      dom_active  = median(tier_data$dom_active, na.rm = TRUE) + (days_since_start - last_day),
      dos_active  = median(tier_data$dos_active, na.rm = TRUE),
      year        = as.numeric(median(tier_data$year, na.rm = TRUE)),
      make        = factor(get_mode(tier_data$make), levels = levels(tier_data$make)),
      body_simple = factor(simplify_body(get_mode(as.character(tier_data$body_type)))),
      sold_month  = month(date),
      sold_week   = week(date),
      tier        = tier_name
    )
  
  preds_log <- predict(all_models[[tier_name]], new_data = fg)$.pred
  fg$.pred  <- exp(preds_log)
  
  future_preds_list[[tier_name]] <- fg
}

future_preds <- bind_rows(future_preds_list)

p_forecast <- ggplot(future_preds, aes(x = date, y = .pred, color = tier)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(method = "lm", linetype = "dashed", alpha = 0.08, formula = y ~ x) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c(
    "Budget"     = "#E74C3C",
    "Affordable" = "#E67E22",
    "Mid-Range"  = "#3498DB",
    "Premium"    = "#2ECC71"
  )) +
  labs(
    title    = "90-Day Price Forecast by Market Tier",
    subtitle = sprintf("Per-tier Random Forest models | Data as of %s",
                       format(start_date + last_day, "%B %d, %Y")),
    x        = "Date",
    y        = "Predicted Price",
    color    = "Tier"
  ) +
  theme_minimal(base_size = 13)

ggsave("future_forecast.png", p_forecast, width = 11, height = 6)
cat("Saved future_forecast.png\n")

# ── 12. Save Outputs ─────────────────────────────────────────────────────────
cat("\n=== SAVING OUTPUTS ===\n")
saveRDS(all_models, "car_price_models_per_tier.rds")
write_csv(future_preds, "car_sales_predictions_90days.csv")
write_csv(metrics_df,   "model_metrics.csv")

cat("\n====================================================\n")
cat(" MODEL TRAINING & FORECASTING COMPLETE\n")
cat("====================================================\n")
cat("Files written:\n")
cat("  car_price_models_per_tier.rds\n")
cat("  car_sales_predictions_90days.csv\n")
cat("  model_metrics.csv\n")
cat("  future_forecast.png\n")
cat("  eda_plot1-4.png\n")
cat("  eda_boxcox_check.png\n")
