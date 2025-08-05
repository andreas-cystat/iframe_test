output_path <- file.path(docs_dir, paste0("tourists.html"))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  if (update_status == "CHANGED" || !file.exists(output_path)) {
    htmlwidgets::saveWidget(fig, output_path, selfcontained = TRUE)
    message("Widget saved to ", output_path)
  }
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
