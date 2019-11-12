get_single_trial_windows <- function(chunks, index, brush_data) {
  chunks %>%
    slice(index) %>%
    merge(brush_data)
}

update_stored_data <- function(stored_data, single_trial_windows) {
  rbind(stored_data,
        single_trial_windows,
        stringsAsFactors = FALSE)
}

clear_saved_window_data <- function(chunks, index, stored_data) {
  chunk_info <- chunks %>%
    slice(index)
  
  anti_join(stored_data, chunk_info)
}

get_chunk_title <- function(trials, index) {
  str <- ""
  
  if (!is.null(trials)) {
    for (name in colnames(trials)) {
      str <- paste0(str, name, ":", trials[[index, name]], " ")
    }
  }
  str
}

empty_window_data <- tibble(xmin = numeric(0), xmax = numeric(0), window_name = character(0))