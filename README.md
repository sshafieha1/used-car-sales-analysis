# Car Price Forecasting Portfolio Application

This application fetches real-world used car sales data from the Marketcheck API and uses a K-Nearest Neighbors (KNN) model to forecast price trends over 90 days.

## Features
- **Daily Automated Updates:** Powered by GitHub Actions to fetch fresh sales data and regenerate the dashboard.
- **Predictive Analytics:** Implements a KNN regression model (`kknn`) tuned based on statistical analysis of over 100,000 listings.
- **Interactive Dashboard:** Built with R, Plotly, and HTMLtools for a premium, mobile-responsive experience.
- **Statistical Report:** Integrated analysis explaining model selection (comparing KNN, Random Forest, etc.).

## How it works
1. **Data:** Fetches "Sold" listings from Marketcheck API.
2. **Model:** A KNN model calculates vehicle valuation based on mileage, age, and build features.
3. **Forecast:** Predicts the "depreciation curve" by simulating vehicle usage 90 days into the future.
4. **Deployment:** Hosted on Vercel as a static site.

## Setup
1. Open the `used-car-sales-analysis` folder.
2. Fill in your Marketcheck API keys in `keys.env`.
3. Run `Rscript setup.R` to install dependencies.
4. Run `Rscript main.R` to generate the initial dashboard.
5. Set up GitHub Secrets for automated daily updates.
