# ---- Loading Libraries ----
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(here)  

# ---- Initialize Paths ----
log_dir <- here::here("Logs_Life_Expectancy")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
docs_dir <- here::here("docs")
if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)

today_str <- format(Sys.Date(), "%Y%m%d")
logfile_path <- file.path(log_dir, paste0("log_", today_str, ".txt"))
log_file <- file.path(log_dir, "log.txt")

# ---- Define the API endpoint ---- 
api_url <- "https://cystatdb.cystat.gov.cy/api/v1/el/8.CYSTAT-DB/Population/Births%20and%20Deaths/1830070G.px"

# ---- Retrieve Metadata ----
metadata <- httr::GET(api_url)
httr::stop_for_status(metadata, task = "fetch metadata")
metadata_json <- httr::content(metadata, as = "parsed")

year_dimension <- metadata_json$variables[[1]]
gender <- metadata_json$variables[[2]]

year_codes <- year_dimension$values
codes <- gender$values

# ---- Prepare and Send Data Query ----
query_body <- list(
  query = list(
    list(code = year_dimension$code, selection = list(filter = "item", values = year_codes)),
    list(code = gender$code, selection = list(filter = "item", values = codes))
  ),
  response = list(format = "json")
)

# Fetch data in JSON format
response <- httr::POST(api_url, body = query_body, encode = "json")
httr::stop_for_status(response, task = "fetch data")
data_json <- httr::content(response, as = "parsed", simplifyDataFrame = TRUE)
values <- data_json$data

if (is.null(data_json$data)) stop("No data returned from API.")
data_values <- data_json$data

# ---- Build Data Frame ----
df <- data.frame(
  year_code = sapply(values$key, function(k) k[1]),
  category_code = sapply(values$key, function(k) k[2]),
  value = sapply(values$values, function(v) {
    if (is.null(v) || is.na(v) || v == "..") return(NA_real_)
    as.numeric(v)
  }),
  stringsAsFactors = FALSE
)

# ---- Data Transformation ----
year_labels <- setNames(year_dimension$valueTexts, year_dimension$values)
category_labels <- setNames(gender$valueTexts, gender$values)

df$year <- as.integer(year_labels[df$year_code])
df$category <- category_labels[df$category_code]

max_year <- max(df$year, na.rm = TRUE)

data_wide <- df %>%
  filter(year >= (max_year - 9)) %>%
  select(year, category, value) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  arrange(year)

# ---- Logging: Compare Data Points to Previous Run ----

# Count number of records in the current dataset
current_data_points <- nrow(df)

# Retrieve last logged data point count
latest_logged_count <- function(log_file) {
  if (!file.exists(log_file)) return(0)
  
  log_lines <- readLines(log_file, warn = FALSE)
  row_lines <- grep("Total rows:", log_lines, value = TRUE)
  if (length(row_lines) == 0) return(0)
  
  last_line <- row_lines[length(row_lines)]
  match <- regmatches(last_line, regexpr("\\d+", last_line))
  
  if (length(match) == 0) return(0)
  
  as.integer(match)
}

# Determine whether to regenerate the widget
last_saved_count <- latest_logged_count(log_file)

update_status <- if (current_data_points > last_saved_count) {
  "Widget updated with new data"
} else {
  "No new data"
}

# ---- Generate Visualization (Only if Updated) ----
if (update_status == "Widget updated with new data") {
  n <- nrow(df)
  
  p <- plotly::plot_ly() %>%
    plotly::add_trace(
      x = data_wide$Άντρες,
      y = data_wide$year,
      type = 'bar',
      name = 'Άντρες',
      orientation = 'h',
      marker = list(color = 'blue'),
      text = data_wide$Άντρες,
      textposition = 'outside',
      hoverinfo = 'x+y+name',
      hovertemplate = 'Έτος: %{y}<br>Προσδ. Ζωής (Άντρες): %{x}<extra></extra>',
      hoverlabel = list(bgcolor = 'white', font = list(color = 'black'))
    ) %>%
    plotly::add_trace(
      x = data_wide$Γυναίκες,
      y = data_wide$year,
      type = 'bar',
      name = 'Γυναίκες',
      orientation = 'h',
      marker = list(color = '#D1006C'),
      text = data_wide$Γυναίκες,
      textposition = 'outside',
      hoverinfo = 'x+y+name',
      hovertemplate = 'Έτος: %{y}<br>Προσδ. Ζωής (Γυναίκες): %{x}<extra></extra>',
      hoverlabel = list(bgcolor = 'white', font = list(color = 'black'))
    ) %>%
    layout(
      xaxis = list(title = 'Προσδοκώμενη Διάρκεια Ζωής (έτη)', range = c(70, 86), dtick = 2),
      yaxis = list(title = 'Έτος', tickvals = data_wide$year, ticktext = data_wide$year),
      barmode = 'group',
      template = "plotly_white",
      showlegend = TRUE
    )
  
  print(p)

  # Save the widget as an HTML file  
  output_path <- file.path(log_dir, paste0("life_expectancy_", today_str, ".html"))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(p, output_path, selfcontained = TRUE)
  message("Widget saved to ", output_path)
  
  output_path <- file.path(docs_dir, paste0("life_expectancy.html"))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(p, output_path, selfcontained = TRUE)
  message("Widget saved to ", output_path)
}

# ---- Write to Log File ----
# Log current row count and update status
log_con <- file(log_file, open = "at")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat(format(Sys.time()), " | Total number of rows: ", current_data_points, "\n")
cat(format(Sys.time()), " | ", update_status, "\n")

sink(type = "message")
sink(type = "output")
close(log_con)

