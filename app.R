library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
cars_raw <- read.csv("global_cars_enhanced.csv", stringsAsFactors = FALSE)

brands      <- sort(unique(cars_raw$Brand))
fuel_types  <- sort(unique(cars_raw$Fuel_Type))
body_types  <- sort(unique(cars_raw$Body_Type))
price_min   <- floor(min(cars_raw$Price_USD))
price_max   <- ceiling(max(cars_raw$Price_USD))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Car Price Explorer",
  theme = bs_theme(preset = "flatly", primary = "#2c6fad"),
  fillable = FALSE,

  sidebar = sidebar(
    width = 260,
    title = "Filters",

    selectInput(
      "brand", "Brand",
      choices  = c("All", brands),
      selected = "All"
    ),
    selectInput(
      "fuel_type", "Fuel Type",
      choices  = c("All", fuel_types),
      selected = "All"
    ),
    selectInput(
      "body_type", "Body Type",
      choices  = c("All", body_types),
      selected = "All"
    ),
    selectInput(
      "transmission", "Transmission",
      choices  = c("All", sort(unique(cars_raw$Transmission))),
      selected = "All"
    ),
    sliderInput(
      "price_range", "Price Range (USD)",
      min   = price_min,
      max   = price_max,
      value = c(price_min, price_max),
      step  = 1000,
      pre   = "$",
      sep   = ","
    ),
    hr(),
    helpText("Data: 300 global cars across multiple brands and markets.")
  ),

  # ── Value boxes ─────────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title    = "Cars Matching Filters",
      value    = textOutput("n_cars"),
      showcase = bsicons::bs_icon("car-front-fill"),
      theme    = "primary"
    ),
    value_box(
      title    = "Average Price",
      value    = textOutput("avg_price"),
      showcase = bsicons::bs_icon("currency-dollar"),
      theme    = "success"
    ),
    value_box(
      title    = "Average Horsepower",
      value    = textOutput("avg_hp"),
      showcase = bsicons::bs_icon("speedometer2"),
      theme    = "info"
    )
  ),

  # ── Plots row ────────────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header("Price vs. Horsepower"),
      plotOutput("price_hp_plot", height = "340px")
    ),
    card(
      full_screen = TRUE,
      card_header("Average Price by Brand"),
      plotOutput("brand_price_plot", height = "340px")
    )
  ),

  # ── Second plots row ─────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Price Distribution by Fuel Type"),
      plotOutput("fuel_box_plot", height = "300px")
    ),
    card(
      full_screen = TRUE,
      card_header("Car Count by Body Type"),
      plotOutput("body_bar_plot", height = "300px")
    )
  ),

  # ── Data table ───────────────────────────────────────────────────────────────
  card(
    full_screen = TRUE,
    card_header("Filtered Car Data"),
    DTOutput("car_table")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive calc: filtered dataset ─────────────────────────────────────────
  filtered_cars <- reactive({
    df <- cars_raw
    if (input$brand != "All")        df <- filter(df, Brand        == input$brand)
    if (input$fuel_type != "All")    df <- filter(df, Fuel_Type    == input$fuel_type)
    if (input$body_type != "All")    df <- filter(df, Body_Type    == input$body_type)
    if (input$transmission != "All") df <- filter(df, Transmission == input$transmission)
    df <- filter(df, Price_USD >= input$price_range[1],
                     Price_USD <= input$price_range[2])
    df
  })

  # ── Value box outputs ────────────────────────────────────────────────────────
  output$n_cars <- renderText({
    nrow(filtered_cars())
  })

  output$avg_price <- renderText({
    df <- filtered_cars()
    if (nrow(df) == 0) return("N/A")
    dollar(round(mean(df$Price_USD), 0))
  })

  output$avg_hp <- renderText({
    df <- filtered_cars()
    if (nrow(df) == 0) return("N/A")
    paste0(round(mean(df$Horsepower), 1), " hp")
  })

  # ── Scatter: Price vs Horsepower ─────────────────────────────────────────────
  output$price_hp_plot <- renderPlot({
    df <- filtered_cars()
    validate(need(nrow(df) > 0, "No data matches the selected filters."))

    ggplot(df, aes(x = Horsepower, y = Price_USD, color = Fuel_Type)) +
      geom_point(alpha = 0.75, size = 2.5) +
      scale_y_continuous(labels = dollar_format()) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x     = "Horsepower (hp)",
        y     = "Price (USD)",
        color = "Fuel Type"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # ── Bar: Average Price by Brand ──────────────────────────────────────────────
  output$brand_price_plot <- renderPlot({
    df <- filtered_cars()
    validate(need(nrow(df) > 0, "No data matches the selected filters."))

    df %>%
      group_by(Brand) %>%
      summarise(Avg_Price = mean(Price_USD), .groups = "drop") %>%
      ggplot(aes(x = reorder(Brand, Avg_Price), y = Avg_Price)) +
      geom_col(fill = "#2c6fad", width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Brand", y = "Avg Price (USD)") +
      theme_minimal(base_size = 13)
  })

  # ── Box: Price Distribution by Fuel Type ─────────────────────────────────────
  output$fuel_box_plot <- renderPlot({
    df <- filtered_cars()
    validate(need(nrow(df) > 0, "No data matches the selected filters."))

    ggplot(df, aes(x = reorder(Fuel_Type, Price_USD, median), y = Price_USD, fill = Fuel_Type)) +
      geom_boxplot(outlier.shape = 21, outlier.alpha = 0.6, show.legend = FALSE) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Fuel Type", y = "Price (USD)") +
      theme_minimal(base_size = 13)
  })

  # ── Bar: Count by Body Type ───────────────────────────────────────────────────
  output$body_bar_plot <- renderPlot({
    df <- filtered_cars()
    validate(need(nrow(df) > 0, "No data matches the selected filters."))

    df %>%
      count(Body_Type) %>%
      ggplot(aes(x = reorder(Body_Type, n), y = n, fill = Body_Type)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Body Type", y = "Count") +
      theme_minimal(base_size = 13)
  })

  # ── Data table ───────────────────────────────────────────────────────────────
  output$car_table <- renderDT({
    df <- filtered_cars() %>%
      select(
        Brand, `Year` = Manufacture_Year, `Body Type` = Body_Type,
        `Fuel Type` = Fuel_Type, Transmission,
        `Engine (CC)` = Engine_CC, `HP` = Horsepower,
        `Mileage (km/l)` = Mileage_km_per_l,
        `Price (USD)` = Price_USD, Country = Manufacturing_Country
      )

    datatable(
      df,
      rownames  = FALSE,
      filter    = "top",
      options   = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "lfrtip"
      )
    ) %>%
      formatCurrency("Price (USD)", currency = "$", digits = 0)
  })
}

shinyApp(ui, server)
