# ---- Load Required Libraries ----
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(here)
library(sodium)  
library(lubridate)

# ---- Setup Directories ----
log_dir <- file.path(here::here("logs"), "logs_population")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

docs_dir <- here::here("docs")
if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)

today_str <- format(Sys.Date(), "%Y%m%d")
csv_log_path <- file.path(log_dir, "logs_population.csv")  

# PART 1: POPULATION PLOT
# ---- Population API Info ----
pop_api_url <- "https://cystatdb.cystat.gov.cy:443/api/v1/el/8.CYSTAT-DB/Population/Population/1820010G.px"
pop_metadata <- httr::GET(pop_api_url)
httr::stop_for_status(pop_metadata)
pop_meta_json <- httr::content(pop_metadata, as = "parsed")

# ---- Extract Variables ----
variables <- pop_meta_json$variables
gender_var <- variables[[which(sapply(variables, function(v) v$code) == "ΦΥΛΟ")]]
age_var    <- variables[[which(sapply(variables, function(v) v$code) == "ΗΛΙΚΙΑ")]]
year_var   <- variables[[which(sapply(variables, function(v) v$code) == "ΕΤΟΣ")]]

gender_code <- "0"   
age_code <- "0"     
year_codes <- year_var$values

# ---- Build Query and Get Data ----
pop_query <- list(
  query = list(
    list(code = gender_var$code, selection = list(filter = "item", values = list(gender_code))),
    list(code = age_var$code, selection = list(filter = "item", values = list(age_code))),
    list(code = year_var$code, selection = list(filter = "item", values = year_codes))
  ),
  response = list(format = "json")
)

# --- Fetch data ---
pop_response <- httr::POST(pop_api_url, body = pop_query, encode = "json")
httr::stop_for_status(pop_response)
pop_data <- httr::content(pop_response, as = "parsed", simplifyDataFrame = TRUE)

df_pop <- data.frame(
  year_code = sapply(pop_data$data$key, function(k) k[[3]]),
  value = sapply(pop_data$data$values, function(v) {
    if (is.null(v) || is.na(v) || v == "..") return(NA_real_)
    as.numeric(gsub(",", ".", v))
  }),
  stringsAsFactors = FALSE
)

# --- Prepare data frame ---
year_labels <- setNames(year_var$valueTexts, year_var$values)
df_pop$year <- year_labels[df_pop$year_code]

df_pop <- df_pop %>%
  select(year, value) %>%
  arrange(year)

# ---- Create Population Widget ----
population_widget <- plotly::plot_ly() %>%
  add_trace(
    data = df_pop,
    x = ~as.numeric(year),
    y = ~value,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'orange', width = 3)
  ) %>%
  layout(
    title = "Πληθυσμός στις Περιοχές που Ελέγχει το Κράτος (χιλιάδες)",
    xaxis = list(
      title = "Έτος",
      range = c(2000, max(as.numeric(df_pop$year), na.rm = TRUE))
    ),
    yaxis = list(title = ""),
    hovermode = "x unified"
  ) %>%
  config(displayModeBar = FALSE)

# Save standalone population widget
output_path <- file.path(docs_dir, "population.html")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(population_widget, output_path, selfcontained = TRUE)
message("✅ Standalone population widget saved to: ", output_path)

# PART 2: LIFE EXPECTANCY PLOT
# ---- Life Expectancy API Info ----
life_api_url <- "https://cystatdb.cystat.gov.cy:443/api/v1/el/8.CYSTAT-DB/Population/Deaths/1830226G.px"
life_metadata <- httr::GET(life_api_url)
httr::stop_for_status(life_metadata)
life_meta_json <- httr::content(life_metadata, as = "parsed")

# ---- Extract Variables ----
year_dim <- life_meta_json$variables[[1]]
gender_dim <- life_meta_json$variables[[2]]

# ---- Build Query and Get Data ----
query_life <- list(
  query = list(
    list(code = year_dim$code, selection = list(filter = "item", values = year_dim$values)),
    list(code = gender_dim$code, selection = list(filter = "item", values = gender_dim$values))
  ),
  response = list(format = "json")
)

# --- Fetch data ---
life_response <- httr::POST(life_api_url, body = query_life, encode = "json")
httr::stop_for_status(life_response)
life_data <- httr::content(life_response, as = "parsed", simplifyDataFrame = TRUE)

# --- Prepare data frame ---
df_life <- data.frame(
  year_code = sapply(life_data$data$key, function(k) k[1]),
  gender_code = sapply(life_data$data$key, function(k) k[2]),
  value = sapply(life_data$data$values, function(v) {
    if (is.null(v) || is.na(v) || v == "..") return(NA_real_)
    as.numeric(v)
  }),
  stringsAsFactors = FALSE
)

year_labels_life <- setNames(year_dim$valueTexts, year_dim$values)
gender_labels_life <- setNames(gender_dim$valueTexts, gender_dim$values)

df_life$year <- as.integer(year_labels_life[df_life$year_code])
df_life$gender <- gender_labels_life[df_life$gender_code]

max_year <- max(df_life$year, na.rm = TRUE)

df_life_wide <- df_life %>%
  filter(year >= max_year - 9) %>%
  select(year, gender, value) %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  arrange(year)

# ---- Create Life Expectancy Widget ----
life_expectancy_widget <- plotly::plot_ly() %>%
  add_trace(
    x = df_life_wide$Άντρες,
    y = df_life_wide$year,
    type = 'bar',
    name = 'Άντρες',
    orientation = 'h',
    marker = list(color = 'blue'),
    text = df_life_wide$Άντρες,
    textposition = 'outside'
  ) %>%
  add_trace(
    x = df_life_wide$Γυναίκες,
    y = df_life_wide$year,
    type = 'bar',
    name = 'Γυναίκες',
    orientation = 'h',
    marker = list(color = '#D1006C'),
    text = df_life_wide$Γυναίκες,
    textposition = 'outside'
  ) %>%
  layout(
    title = "Προσδοκώμενη Διάρκεια Ζωής στη Γέννηση (χρόνια)",
    xaxis = list(title = 'Έτη', range = c(70, 86), dtick = 2),
    yaxis = list(title = 'Έτος', tickvals = df_life_wide$year),
    barmode = 'group',
    template = "plotly_white"
  ) %>%
  config(displayModeBar = FALSE)

# Save standalone life expectancy widget
life_output_path <- file.path(docs_dir, "life_expectancy.html")
dir.create(dirname(life_output_path), showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(life_expectancy_widget, life_output_path, selfcontained = TRUE)
message("✅ Standalone life expectancy widget saved to: ", life_output_path)

# ----Combine Widgets ----
population_widget <- population_widget %>% layout(width = 600, height = 400)
life_expectancy_widget <- life_expectancy_widget %>% layout(width = 600, height = 400)

# Create side-by-side layout 
combined_html <- tagList(
  tags$div(
    style = "display: flex; gap: 0px; justify-content: center;",
    tags$div(style = "flex: 1;", population_widget),
    tags$div(style = "flex: 1;", life_expectancy_widget)
  )
)

combined_path <- file.path(docs_dir, "combined_graphs.html")
htmltools::save_html(combined_html, file = combined_path)
message("✅ Combined widget saved to: ", combined_path)

# --- Compute hash of combined data ---
combined_data <- list(df_pop = df_pop, df_life_wide = df_life_wide)
combined_raw <- serialize(combined_data, connection = NULL)
combined_hash <- sodium::bin2hex(sodium::hash(combined_raw))

update_status <- if (file.exists(csv_log_path)) {
  previous_lines <- readLines(csv_log_path)
  last_line <- tail(previous_lines, 1)
  last_hash <- strsplit(last_line, "\t")[[1]][2]
  if (!is.null(last_hash) && last_hash == combined_hash) "UNCHANGED" else "CHANGED"
} else {
  "CHANGED"
}

# Logging block 
if (!file.exists(csv_log_path)) {
  writeLines("timestamp\tcombined_hash\tstatus", csv_log_path)
}

log_con <- file(csv_log_path, open = "at")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat(
  format(Sys.time(), "%d/%m/%Y %H:%M"), "\t",
  combined_hash, "\t",
  update_status, "\n",
  sep = ""
)

sink(type = "message")
sink(type = "output")
close(log_con)



