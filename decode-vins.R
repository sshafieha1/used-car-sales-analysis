library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# 1. Load the existing data
cat("Loading car_sales_past90days_FINAL.csv...\n")
car_sales <- read_csv("car_sales_past90days_FINAL.csv", show_col_types = FALSE)

# Ensure required columns exist in the dataframe
if (!"make" %in% names(car_sales)) car_sales$make <- NA_character_
if (!"year" %in% names(car_sales)) car_sales$year <- NA_real_
if (!"body_type" %in% names(car_sales)) car_sales$body_type <- NA_character_

# 2. Extract unique VINs that need decoding (to save API calls and avoid redundancy)
needs_decoding <- car_sales %>%
  filter(is.na(make) | is.na(year) | is.na(body_type))

vins <- unique(na.omit(needs_decoding$vin))
cat(sprintf("Found %d unique VINs that need decoding.\n", length(vins)))

if (length(vins) > 0) {
  # 3. Prepare batches of 50 (NHTSA limit)
  chunks <- split(vins, ceiling(seq_along(vins) / 50))
  results_list <- list()
  
  cat("Contacting NHTSA Free API...\n")
  
  # 4. Loop through batches
  for (i in seq_along(chunks)) {
    vin_str <- paste(chunks[[i]], collapse = ";")
    
    res <- tryCatch({
      POST(
        url = "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVINValuesBatch/",
        body = list(format = "json", DATA = vin_str),
        encode = "form",
        timeout(10)
      )
    }, error = function(e) NULL)
    
    if (!is.null(res) && status_code(res) == 200) {
      parsed <- content(res, as = "text", encoding = "UTF-8") %>% fromJSON()
      if (!is.null(parsed$Results)) {
        # Extract only the columns we care about
        batch_df <- parsed$Results %>% 
          select(VIN, Make, ModelYear, BodyClass)
        results_list[[i]] <- batch_df
      }
    }
    
    if (i %% 10 == 0) cat(sprintf("  Processed %d / %d batches...\n", i, length(chunks)))
    Sys.sleep(0.3) # Be polite to the public API
  }
  
  # 5. Combine and clean the results
  cat("Combining results...\n")
  vin_data <- bind_rows(results_list) %>%
    rename(
      vin = VIN,
      make_new = Make,
      year_new = ModelYear,
      body_type_new = BodyClass
    ) %>%
    mutate(
      year_new = as.numeric(year_new),
      make_new = na_if(make_new, ""),
      body_type_new = na_if(body_type_new, "")
    ) %>%
    distinct(vin, .keep_all = TRUE)
  
  # 6. Merge back into the main dataset using coalesce
  cat("Merging newly decoded details with original dataset...\n")
  car_sales_enriched <- car_sales %>%
    left_join(vin_data, by = "vin") %>%
    mutate(
      make      = coalesce(make, make_new),
      year      = coalesce(year, year_new),
      body_type = coalesce(body_type, body_type_new)
    ) %>%
    select(-make_new, -year_new, -body_type_new)
  
  # 7. Save
  write_csv(car_sales_enriched, "car_sales_past90days_FINAL.csv")
  cat("\nDone! Vehicle details added and saved back to car_sales_past90days_FINAL.csv.\n")
} else {
  cat("\nAll listings already have decoded vehicle details. No calls made to NHTSA.\n")
}
cat("You can now re-run model-prediction.R!\n")
