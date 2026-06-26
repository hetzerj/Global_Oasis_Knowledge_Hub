get_zenodo_data <- function(record_id, cache_dir, is_sandbox = TRUE) {
  
  # Create cache directory if it does not already exist
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Files required for the knowledge hub data workflow
  required_files <- c(
    "expanded_works_nodes.parquet",
    "expanded_works_edges.parquet",
    "matched_reviewed_refs.parquet"
  )
  
  local_files <- file.path(cache_dir, required_files)
  missing_files <- required_files[!file.exists(local_files)]
  
  # Use local cache if all required files are already available
  if (length(missing_files) == 0) {
    message("Using cached data from: ", cache_dir)
    return(invisible(TRUE))
  }
  
  message("Downloading missing files from Zenodo record: ", record_id)
  
  # Select Zenodo or Zenodo sandbox API endpoint
  base_url <- if (is_sandbox) {
    "https://sandbox.zenodo.org/api/records/"
  } else {
    "https://zenodo.org/api/records/"
  }
  
  record_url <- paste0(base_url, record_id)
  
  # Read Zenodo record metadata
  record <- jsonlite::fromJSON(record_url)
  available_files <- record$files$key
  
  # Check that all required files are available on Zenodo
  missing_on_zenodo <- setdiff(required_files, available_files)
  
  if (length(missing_on_zenodo) > 0) {
    stop(
      "The following required files are missing on Zenodo: ",
      paste(missing_on_zenodo, collapse = ", ")
    )
  }
  
  files_to_download <- required_files[required_files %in% missing_files]
  
  # Download only files that are not already present in the cache
  for (file_name in files_to_download) {
    
    file_info <- record$files[record$files$key == file_name, ]
    download_url <- file_info$links$self
    target_file <- file.path(cache_dir, file_name)
    
    message("Downloading: ", file_name)
    
    utils::download.file(
      url = download_url,
      destfile = target_file,
      mode = "wb",
      quiet = FALSE
    )
  }
  
  # Final check to ensure all required files are now available locally
  missing_after_download <- required_files[
    !file.exists(file.path(cache_dir, required_files))
  ]
  
  if (length(missing_after_download) > 0) {
    stop(
      "Download finished, but the following files are still missing: ",
      paste(missing_after_download, collapse = ", ")
    )
  }
  
  message("All required Zenodo files are available.")
  return(invisible(TRUE))
}


load_zenodo_data <- function(cache_dir) {
  
  # Create cache directory if needed
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Zenodo sandbox record containing the required parquet files
  zenodo_record_id <- "501166"
  
  get_zenodo_data(
    record_id = zenodo_record_id,
    cache_dir = cache_dir,
    is_sandbox = TRUE
  )
  
  # Read cached parquet files
  nodes_df <- arrow::read_parquet(
    file.path(cache_dir, "expanded_works_nodes.parquet")
  )
  
  edges_df <- arrow::read_parquet(
    file.path(cache_dir, "expanded_works_edges.parquet")
  )
  
  seedworks <- arrow::read_parquet(
    file.path(cache_dir, "matched_reviewed_refs.parquet")
  )
  
  # Harmonise OpenAlex identifier column name
  nodes_df <- nodes_df %>%
    dplyr::rename(OpenAlex_ID_short = oa_ID)
  
  # Add graph node information to the reviewed seed works
  global_nodes <- seedworks %>%
    dplyr::left_join(
      nodes_df %>% dplyr::select(-Year, -Title, -Authors),
      by = "OpenAlex_ID_short"
    )
  
  # Remove entries without OpenAlex identifier
  global_nodes <- global_nodes[
    !is.na(global_nodes$OpenAlex_ID_short),
    ,
    drop = FALSE
  ]
  
  return(list(
    nodes_df = nodes_df,
    edges_df = edges_df,
    seedworks = seedworks,
    global_nodes = global_nodes
  ))
}