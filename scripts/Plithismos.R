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

csv_log_path <- file.path(log_dir, "logs_population.csv")

# ---- Population API ----
pop_api_url <- "https://cystatdb23px.cystat.gov.cy/api/v1/el/8.CYSTAT-DB/Population/Population/1820010G.px"
pop_meta <- httr::content(httr::GET(pop_api_url), as = "parsed")
year_var <- pop_meta$variables[[1]]
period_var <- pop_meta$variables[[2]]
pop_type_var <- pop_meta$variables[[3]]

pop_query <- list(
  query = list(
    list(code = year_var$code, selection = list(filter = "item", values = year_var$values)),
    list(code = period_var$code, selection = list(filter = "item", values = list("0"))),
    list(code = pop_type_var$code, selection = list(filter = "item", values = list("0")))
  ),
  response = list(format = "json")
)

pop_data <- httr::content(httr::POST(pop_api_url, body = pop_query, encode = "json"), as = "parsed", simplifyDataFrame = TRUE)
df_pop <- data.frame(
  year_code = sapply(pop_data$data$key, function(k) k[[1]]),
  value = sapply(pop_data$data$values, function(v) ifelse(is.null(v) || v == "..", NA, as.numeric(gsub(",", ".", v))))
)
year_labels <- setNames(year_var$valueTexts, year_var$values)
df_pop$year <- year_labels[df_pop$year_code]
df_pop <- df_pop %>% select(year, value) %>% arrange(year)

# ---- Life Expectancy API ----
life_api_url <- "https://cystatdb.cystat.gov.cy:443/api/v1/el/8.CYSTAT-DB/Population/Deaths/1830226G.px"
life_meta <- httr::content(httr::GET(life_api_url), as = "parsed")
year_dim <- life_meta$variables[[1]]
gender_dim <- life_meta$variables[[2]]

query_life <- list(
  query = list(
    list(code = year_dim$code, selection = list(filter = "item", values = year_dim$values)),
    list(code = gender_dim$code, selection = list(filter = "item", values = gender_dim$values))
  ),
  response = list(format = "json")
)

life_data <- httr::content(httr::POST(life_api_url, body = query_life, encode = "json"), as = "parsed", simplifyDataFrame = TRUE)
df_life <- data.frame(
  year_code = sapply(life_data$data$key, function(k) k[1]),
  gender_code = sapply(life_data$data$key, function(k) k[2]),
  value = sapply(life_data$data$values, function(v) ifelse(is.null(v) || v == "..", NA, as.numeric(v)))
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

# ---- Compute Hash ----
combined_data <- list(df_pop = df_pop, df_life_wide = df_life_wide)
combined_raw <- serialize(combined_data, connection = NULL)
combined_hash <- sodium::bin2hex(sodium::hash(combined_raw))

# ---- Compare Hashes ----
last_hash <- NA
if (file.exists(csv_log_path)) {
  hash_log <- read.delim(csv_log_path, sep = "\t", stringsAsFactors = FALSE)
  if ("combined_hash" %in% names(hash_log) && nrow(hash_log) > 0) {
    last_hash <- tail(hash_log$combined_hash, 1)
  }
}
                       
update_status <- if (!is.na(last_hash) && last_hash == combined_hash) "UNCHANGED" else "CHANGED"

# ---- Only Save If Changed ----
if (update_status == "CHANGED") {
  # Population Plot
  population_widget <- plot_ly(df_pop) %>%
    add_trace(
      x = ~as.numeric(year), y = ~value,
      type = 'scatter', mode = 'lines',
      line = list(color = 'orange', width = 3)
    ) %>%
    layout(
      title = "Πληθυσμός στις Περιοχές που Ελέγχει το Κράτος (χιλιάδες)",
      xaxis = list(title = "Έτος", range = c(2000, max(as.numeric(df_pop$year), na.rm = TRUE))),
      yaxis = list(title = ""),
      hovermode = "x unified"
    ) %>%
    config(displayModeBar = FALSE)
  
  # Life Expectancy Plot
  life_expectancy_widget <- plot_ly() %>%
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
      yaxis = list(title = 'Έτος'),
      barmode = 'group',
      template = "plotly_white"
    ) %>%
    config(displayModeBar = FALSE)
  
  # Save widgets
  htmlwidgets::saveWidget(population_widget, file.path(docs_dir, "population.html"), selfcontained = TRUE)
  htmlwidgets::saveWidget(life_expectancy_widget, file.path(docs_dir, "life_expectancy.html"), selfcontained = TRUE)
  
  # Combined layout
  combined_html <- tagList(
    tags$div(
      style = "display: flex; gap: 0px; justify-content: center;",
      tags$div(style = "flex: 1;", population_widget),
      tags$div(style = "flex: 1;", life_expectancy_widget)
    )
  )
  
  htmltools::save_html(combined_html, file = file.path(docs_dir, "combined_graphs.html"))
  message("✅ Widgets updated and saved.")
}

# ---- Logging ----
if (!file.exists(csv_log_path)) {
  writeLines("timestamp\tcombined_hash\tstatus", csv_log_path)
}
                       
log_entry <- paste(format(Sys.time(), "%d/%m/%Y %H:%M"), combined_hash, update_status, sep = "\t")
write(log_entry, file = csv_log_path, append = TRUE)
