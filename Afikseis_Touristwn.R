install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "plotly", "htmlwidgets", "here"))

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(here)

# --- Setup paths ---
log_dir <- here("Logs_Afikseis_Touristwn")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

today_str <- format(Sys.Date(), "%Y%m%d")
logfile_path <- file.path(log_dir, paste0("log_", today_str, ".txt"))
log_file <- file.path(log_dir, "log.txt")

# --- API URL ---
api_url <- "https://cystatdb.cystat.gov.cy:443/api/v1/el/8.CYSTAT-DB/Tourism/Tourists/Monthly/2021012G.px"

# --- Fetch metadata ---
metadata <- GET(api_url)
stop_for_status(metadata, task = "fetch metadata")
metadata_json <- content(metadata, as = "parsed")

year_dimension <- metadata_json$variables[[1]]
values <- metadata_json$variables[[2]]

year_codes <- year_dimension$values
measure_code_arithmos <- values$values[which(values$valueTexts == "Αριθμός")]
measure_code_change <- values$values[which(values$valueTexts == "Ετήσια μεταβολή (%)")]

# --- Query body ---
query_body <- list(
  query = list(
    list(
      code = year_dimension$code,
      selection = list(
        filter = "item",
        values = year_codes
      )
    ),
    list(
      code = values$code,
      selection = list(
        filter = "item",
        values = list(measure_code_arithmos[[1]], measure_code_change[[1]])
      )
    )
  ),
  response = list(format = "json")
)

# --- Fetch data ---
response <- POST(api_url, body = query_body, encode = "json")
stop_for_status(response, task = "fetch data")
data_json <- content(response, as = "parsed", simplifyDataFrame = TRUE)

if (is.null(data_json$data)) stop("No data returned from API.")
data_values <- data_json$data

# --- Prepare data frame ---
df <- data.frame(
  year_code = sapply(data_values$key, function(k) k[1]),
  category_code = sapply(data_values$key, function(k) k[2]),
  value = sapply(data_values$values, function(v) {
    if (is.null(v) || is.na(v) || v == "..") return(NA_real_)
    suppressWarnings(as.numeric(v))
  }),
  stringsAsFactors = FALSE
)

year_labels <- setNames(year_dimension$valueTexts, year_dimension$values)
measure_labels <- setNames(values$valueTexts, values$values)

df$year <- year_labels[as.character(df$year_code)]
df$measure <- measure_labels[as.character(df$category_code)]
df <- df[, c("year", "measure", "value")]

df <- df %>%
  mutate(
    value = as.numeric(value),
    ΜΗΝΑΣ = as.Date(paste0(substr(year, 1, 4), "-", substr(year, 6, 7), "-01"))
  ) %>%
  arrange(ΜΗΝΑΣ)

df_arithmos <- df %>% filter(measure == "Αριθμός")
df_change <- df %>% filter(measure == "Ετήσια μεταβολή (%)")

# --- Count current data ---
current_data_points <- nrow(df_arithmos)

# --- Function to get last saved count ---
get_latest_logged_count <- function(log_file_path) {
  if (!file.exists(log_file_path)) return(0)
  log_lines <- readLines(log_file_path, warn = FALSE)
  row_lines <- grep("Total number of rows:", log_lines, value = TRUE)
  if (length(row_lines) == 0) return(0)
  last_line <- row_lines[length(row_lines)]
  match <- regmatches(last_line, regexpr("\\d+", last_line))
  as.integer(match)
}

last_saved_count <- get_latest_logged_count(log_file)
update_status <- if (current_data_points > last_saved_count) {
  "Widget updated with new data"
} else {
  "No new data"
}

# --- If new data, generate plot ---
if (update_status == "Widget updated with new data") {
  df_filtered <- df_arithmos
  n <- nrow(df_filtered)
  df_initial <- if (n >= 50) df_filtered[(n - 49):n, ] else df_filtered
  initial_start <- as.character(df_initial$ΜΗΝΑΣ[1])
  initial_end <- as.character(df_initial$ΜΗΝΑΣ[nrow(df_initial)])

  fig <- plot_ly() %>%
    add_trace(
      data = df_arithmos,
      x = ~ΜΗΝΑΣ,
      y = ~value,
      type = 'scatter',
      mode = 'lines+markers',
      name = "Αριθμός",
      line = list(color = '#1f77b4'),
      visible = TRUE
    ) %>%
    add_trace(
      data = df_change,
      x = ~ΜΗΝΑΣ,
      y = ~value,
      type = 'scatter',
      mode = 'lines+markers',
      name = "Ετήσια μεταβολή (%)",
      line = list(color = 'red'),
      marker = list(color = 'red'),
      visible = FALSE
    ) %>%
    layout(
      updatemenus = list(
        list(
          type = "dropdown",
          active = 0,
          buttons = list(
            list(label = "Αριθμός", method = "update",
                 args = list(list(visible = c(TRUE, FALSE)),
                             list(yaxis = list(title = "Αριθμός Αφίξεων")))),
            list(label = "Ετήσια μεταβολή (%)", method = "update",
                 args = list(list(visible = c(FALSE, TRUE)),
                             list(yaxis = list(title = "Ετήσια μεταβολή (%)"))))
          )
        )
      ),
      xaxis = list(
        title = "Μήνας",
        range = c(initial_start, initial_end),
        fixedrange = FALSE,
        tickangle = -45,
        tickformat = "%b %Y",
        dtick = "M4"
      ),
      yaxis = list(title = "Αριθμός Αφίξεων")
    )

  # Ensure output folder exists and save widget
  output_path <- file.path(log_dir, paste0("tourists_", today_str, ".html"))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  saveWidget(fig, output_path, selfcontained = TRUE)
  message("Widget saved to ", output_path)
}

# --- Logging ---
log_con <- file(logfile_path, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat(format(Sys.time()), " | Total number of rows: ", current_data_points, "\n")
cat(format(Sys.time()), " | ", update_status, "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
