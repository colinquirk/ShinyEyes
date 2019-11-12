get_single_chunk_windows <- function(chunks, index, brush_data) {
  chunks %>%
    slice(index) %>%
    merge(brush_data)
}

update_stored_data <- function(stored_data, single_chunk_windows) {
  data <- rbind(stored_data,
                single_chunk_windows,
                stringsAsFactors = FALSE)

  distinct(data)
}

clear_saved_window_data <- function(chunks, index, stored_data) {
  chunk_info <- chunks %>%
    slice(index)
  
  anti_join(stored_data, chunk_info)
}

get_chunk_title <- function(chunks, index) {
  str <- ""
  
  if (!is.null(chunks)) {
    for (name in colnames(chunks)) {
      str <- paste0(str, name, ":", chunks[[index, name]], " ")
    }
  }
  str
}

empty_window_data <- tibble(xmin = numeric(0), xmax = numeric(0), window_name = character(0))