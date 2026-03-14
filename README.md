# Car Price Explorer — Shiny for R

An interactive dashboard for exploring a dataset of 300 global cars, built with **Shiny for R**. Filter by brand, fuel type, body type, transmission, and price range to see how these attributes relate to car pricing and performance.

> **Live app:** https://019cee05-e185-84e3-382c-2356fd7faa8d.share.connect.posit.cloud/

------------------------------------------------------------------------

## App Features

| Component | Description |
|---------------------------------|---------------------------------------|
| **4 input controls** | Brand, Fuel Type, Body Type, Transmission dropdowns + Price Range slider |
| **Reactive calc** | `filtered_cars()` — a reactive dataframe that drives all outputs |
| **3 value boxes** | Cars matching filters · Average price · Average horsepower |
| **4 plots** | Price vs HP scatter · Avg price by brand bar chart · Price distribution by fuel type boxplot · Car count by body type |
| **Data table** | Sortable, filterable table of all matching records |

------------------------------------------------------------------------

## Run Locally

### 1. Install R packages

Open R or RStudio and run:

``` r
install.packages(c(
  "shiny",
  "bslib",
  "bsicons",
  "dplyr",
  "ggplot2",
  "DT",
  "scales"
))
```

### 2. Clone the repo

``` bash
git clone https://github.com/tanav2202/shiny-r-car-price-pred.git
cd shiny-r-car-price-pred
```

### 3. Launch the app

``` r
shiny::runApp("app.R")
```

Or from the terminal:

``` bash
Rscript -e "shiny::runApp('app.R')"
```

------------------------------------------------------------------------

## Dataset

`global_cars_enhanced.csv` — 300 cars with attributes including brand, body type, fuel type, transmission, engine displacement (CC), horsepower, fuel efficiency, price (USD), manufacturing country, and derived features (age category, efficiency score, HP-per-CC ratio).
