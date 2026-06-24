
get_zenodo_data <- function(doi, cache_dir, is_sandbox = FALSE) {
  # if already downloaded in this running instance, reuse
  rds_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
  
  required_files <- c(
    "expanded_works_nodes.parquet",
    "expanded_works_edges.parquet",
    "matched_reviewed_refs.parquet"
  )
  
  missing_files <- required_files[
    !file.path(cache_dir, required_files) %in% rds_files
  ]
  
  if (length(missing_files) == 0) {
    message("Using cached data: ", rds_files[1])
  } else {
      message("Downloading data from Zenodo: ", doi)
    
      if (length(rds_files) > 0) {
        file.remove(rds_files)
      }
    
      download_zenodo(
        doi = doi,
        path = cache_dir,
        files = list(),
        sandbox = is_sandbox,
        logger = "INFO",
        quiet = FALSE, 
        timeout=600
      )
    
      rds_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
      
      missing_files <- required_files[
        !file.path(cache_dir, required_files) %in% rds_files
      ]
      
      if (length(missing_files) > 0){stop(
        "Download succeeded but the following files are missing: ",
        paste(missing_files, collapse = ", "))}
  }
}

load_zenodo_data <- function(CACHE_DIR) {

  
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

ZENODO_DOI <- "10.5072/zenodo.498995"   

get_zenodo_data(ZENODO_DOI, CACHE_DIR, sandbox = TRUE) 

nodes_df <- arrow::read_parquet(file.path(CACHE_DIR, "expanded_works_nodes.parquet"))
edges_df <- arrow::read_parquet(file.path(CACHE_DIR, "expanded_works_edges.parquet"))
seedworks <- arrow::read_parquet(file.path(CACHE_DIR, "matched_reviewed_refs.parquet"))

global_nodes <- seedworks %>%
  dplyr::left_join(
    nodes_df %>% dplyr::select(-Year, -Title, -Authors),
    by = "oa_ID"
  )

global_nodes <- global_nodes[!is.na(global_nodes$oa_ID), , drop = FALSE]

return(list(
  nodes_df = nodes_df,
  edges_df = edges_df,
  seedworks = seedworks, 
  global_nodes = global_nodes
))

}
