library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

API_KEY <- Sys.getenv("MARKETCHECK_API_KEY", unset = "mc_live_ku40M6pXuvG910QgjqeabbBUKezYzy2Y")
BASE_URL <- "https://api.marketcheck.com/v2/search/car/recents"

# ── Geographic Constraints (Respecting 100 Mile Radius Restriction) ───
# If ZIP_CODE is specified, we query listings within the RADIUS around that ZIP code.
# If ZIP_CODE is NULL, we fallback to querying the entire state specified in STATE.
ZIP_CODE <- NULL   # e.g., "93106" for Santa Barbara (or NULL for statewide CA)
RADIUS   <- 100    # Max radius allowed on trial tier is 100
STATE    <- "CA"   # Fallback state if ZIP_CODE is NULL

# ── 1. Determine date range for fetching (Auto-incremental update) ───
today      <- Sys.Date()
file_name  <- "car_sales_past90days_FINAL.csv"

# Check if there is an existing dataset to find the last update date
if (file.exists(file_name)) {
  existing_df <- readr::read_csv(file_name, show_col_types = FALSE)
  if ("last_seen_date" %in% names(existing_df) && nrow(existing_df) > 0) {
    # Extract the max date in existing data
    max_date <- as.Date(max(existing_df$last_seen_date, na.rm = TRUE))
    # Fetch from the day after the last seen date
    start_date <- max_date + 1
    cat(sprintf("Detected existing dataset '%s' with records up to %s.\n", file_name, max_date))
  } else {
    start_date <- today - 7
    cat(sprintf("Existing dataset '%s' has no records or lacks 'last_seen_date'. Defaulting start to 7 days ago.\n", file_name))
  }
} else {
  # Fallback to car_sales_past90days_4k.csv if FINAL.csv is missing
  alt_file <- "car_sales_past90days_4k.csv"
  if (file.exists(alt_file)) {
    existing_df <- readr::read_csv(alt_file, show_col_types = FALSE)
    if ("last_seen_date" %in% names(existing_df) && nrow(existing_df) > 0) {
      max_date <- as.Date(max(existing_df$last_seen_date, na.rm = TRUE))
      start_date <- max_date + 1
      cat(sprintf("Detected alternative dataset '%s' with records up to %s.\n", alt_file, max_date))
    } else {
      start_date <- today - 7
    }
  } else {
    # Default catch-up start date (or 7 days ago if first run)
    start_date <- as.Date("2026-05-22")
    cat(sprintf("No existing datasets found. Starting fresh from default date: %s\n", start_date))
  }
}

# Override start_date if needed (e.g. if you want to force a specific start date like May 22, 2026)
# start_date <- as.Date("2026-05-22")

cat(sprintf("Target fetch range: from %s to %s\n", start_date, today))

if (start_date >= today) {
  cat("Dataset is already up-to-date. No new listings to fetch.\n")
  windows <- tibble()
} else {
  # Create daily fetch windows (from Day D to Day D+1) to ensure the range is non-empty
  window_starts <- seq.Date(start_date, today - 1, by = "day")
  windows <- tibble(
    from     = window_starts,
    to       = window_starts + 1,
    from_str = format(from, "%Y%m%d"),
    to_str   = format(to,   "%Y%m%d")
  )
  cat(sprintf("Created %d windows (1 day per window)\n", nrow(windows)))
}

# ── 2. Single-call fetcher ──────────────────────────────────────────────────
fetch_window <- function(from_str, to_str, start_offset = 0, rows = 50) {
  
  query_params <- list(
    api_key         = API_KEY,
    car_type        = "used",
    sold            = "true",
    last_seen_range = paste0(from_str, "-", to_str),
    sort_by         = "last_seen",
    sort_order      = "desc",
    rows            = rows,
    start           = start_offset
  )
  
  if (!is.null(ZIP_CODE)) {
    query_params$zip    <- ZIP_CODE
    query_params$radius <- RADIUS
  } else {
    query_params$state  <- STATE
  }
  
  resp <- GET(
    url   = BASE_URL,
    query = query_params,
    add_headers(Accept = "application/json")
  )
  
  stop_for_status(resp)
  content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
}

# ── 3. Loop: Fetch all windows day-by-day ─────────────────────────
all_listings <- list()
call_count   <- 0
total_rows   <- 0

if (nrow(windows) > 0) {
  for (i in seq_len(nrow(windows))) {
    
    cat(sprintf("\nWindow %02d: %s → %s\n", i, windows$from_str[i], windows$to_str[i]))
    
    # We fetch page 0 and 1 (up to 100 rows per day)
    for (page in 0:1) {
      
      result <- tryCatch(
        fetch_window(
          windows$from_str[i],
          windows$to_str[i],
          start_offset = page * 50
        ),
        error = function(e) {
          cat(sprintf("  ERROR on page %d: %s\n", page, e$message))
          NULL
        }
      )
      
      call_count <- call_count + 1
      
      if (!is.null(result) && !is.null(result$listings) && length(result$listings) > 0) {
        n <- nrow(result$listings)
        total_rows <- total_rows + n
        all_listings[[length(all_listings) + 1]] <- result$listings
        cat(sprintf("  Page %d: +%d rows (window total available: %s)\n",
                    page, n, result$num_found))
      } else {
        cat(sprintf("  Page %d: no results\n", page))
      }
      
      Sys.sleep(0.3)  # Polite throttle (respecting 5 calls per second)
    }
    
    cat(sprintf("  Running total: %d rows | %d calls used\n", total_rows, call_count))
  }
}

cat(sprintf("\n=== Done: %d rows fetched across %d API calls ===\n", total_rows, call_count))

# ── 4. Combine into one data frame ─────────────────────────────────────────
if (length(all_listings) > 0) {
  df <- bind_rows(all_listings)
  
  # Drop exact duplicates (safety net in case of window overlap)
  df <- df |> distinct(vin, last_seen_at, .keep_all = TRUE)
  cat(sprintf("After dedup: %d rows\n", nrow(df)))
  
  # ── 5. Select & clean key columns ──────────────────────────────────────────
  cols_of_interest <- c(
    "id", "vin", "year", "make", "model", "trim",
    "price", "miles",
    "last_seen_at",
    "first_seen_at",
    "dom_active",
    "dos_active",
    "body_type", "fuel_type", "drivetrain", "transmission",
    "exterior_color", "seller_type",
    "city", "state", "zip"
  )
  
  cols_available <- intersect(cols_of_interest, names(df))
  df_clean <- df |> select(all_of(cols_available))
  
  # Convert unix timestamps → readable dates
  df_clean <- df_clean |>
    mutate(
      last_seen_date  = as.POSIXct(last_seen_at,  origin = "1970-01-01"),
      first_seen_date = as.POSIXct(first_seen_at, origin = "1970-01-01"),
      days_on_market  = as.numeric(difftime(last_seen_date, first_seen_date, units = "days"))
    )
  
  # Add time-based features useful for forecasting
  df_clean <- df_clean |>
    mutate(
      sold_month   = month(last_seen_date),
      sold_week    = week(last_seen_date),
      sold_dow     = wday(last_seen_date, label = TRUE),
      sold_quarter = quarter(last_seen_date)
    )
  
  glimpse(df_clean)
  
  # ── 6. Save or Append ───────────────────────────────────────────────────────
  if (file.exists(file_name)) {
    old_df <- readr::read_csv(file_name, show_col_types = FALSE)
    combined_df <- dplyr::bind_rows(old_df, df_clean)
    combined_df <- combined_df |> dplyr::distinct(vin, last_seen_at, .keep_all = TRUE)
    cat(sprintf("\nAppended to '%s'. New total: %d rows\n", file_name, nrow(combined_df)))
    readr::write_csv(combined_df, file_name)
  } else {
    readr::write_csv(df_clean, file_name)
    cat(sprintf("\nSaved to %s\n", file_name))
  }
} else {
  cat("\nNo new records to combine or save.\n")
}