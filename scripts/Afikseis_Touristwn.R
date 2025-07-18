# ---- Load Required Libraries ----
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(here)
library(sodium)

# --- Setup paths ---
log_path <- file.path(here::here("logs"), "logs_tourists")
if (!dir.exists(log_path)) dir.create(log_path, recursive = TRUE)

docs_dir <- here::here("docs")
if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)

today_str <- format(Sys.Date(), "%Y%m%d")
csv_log_path <- file.path(log_path, "log_tourists.csv")

# --- API URL ---
api_url <- "https://cystatdb.cystat.gov.cy:443/api/v1/el/8.CYSTAT-DB/Tourism/Tourists/Monthly/2021012G.px"

# --- Fetch metadata ---
metadata <- httr::GET(api_url)
httr::stop_for_status(metadata, task = "fetch metadata")
metadata_json <- httr::content(metadata, as = "parsed")

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
      selection = list(filter = "item", values = year_codes)
    ),
    list(
      code = values$code,
      selection = list(filter = "item", values = list(measure_code_arithmos[[1]], measure_code_change[[1]]))
    )
  ),
  response = list(format = "json")
)

# --- Fetch data ---
response <- httr::POST(api_url, body = query_body, encode = "json")
httr::stop_for_status(response, task = "fetch data")
data_json <- httr::content(response, as = "parsed", simplifyDataFrame = TRUE)

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

# --- Combine both filtered data frames for hashing ---
df_combined <- bind_rows(df_arithmos, df_change) %>%
  arrange(year, measure, value)  

# --- Compute hash of combined data ---
df_raw <- serialize(df_combined, connection = NULL)
current_hash <- sodium::bin2hex(sodium::hash(df_raw))

# --- Read last saved hash from CSV ---
last_hash <- NA
if (file.exists(csv_log_path)) {
  hash_log <- read.csv(csv_log_path, stringsAsFactors = FALSE)
  if (nrow(hash_log) > 0) {
    last_hash <- tail(hash_log$hash, 1)
  }
}

# --- Compare hashes ---
update_status <- if (file.exists(csv_log_path)) {
  previous_lines <- readLines(csv_log_path)
  last_line <- tail(previous_lines, 1)
  last_hash <- strsplit(last_line, "\t")[[1]][2]
  if (!is.null(last_hash) && last_hash == current_hash) "UNCHANGED" else "CHANGED"
} else {
  "CHANGED"
}

# --- Save widget if changed ---
if (update_status == "CHANGED") {
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
  
  output_path <- file.path(docs_dir, paste0("tourists.html"))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(fig, output_path, selfcontained = TRUE)
  message("Widget saved to ", output_path)
}

# Logging block                   
log_con <- file(csv_log_path, open = "at")  
sink(log_con, type = "output")
sink(log_con, type = "message")

cat(
  format(Sys.time(), "%d/%m/%Y %H:%M"), "\t",
  current_hash, "\t",
  update_status, "\n",
  sep = ""
)

sink(type = "message")
sink(type = "output")
close(log_con)
