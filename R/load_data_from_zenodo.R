
get_zenodo_data <- function(doi, cache_dir) {
  # if already downloaded in this running instance, reuse
  rds_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(rds_files) > 0) {
    message("Using cached data: ", rds_files[1])
  } else{
    message("Downloading data from Zenodo: ", doi)
    download_zenodo(
      doi = doi,
      path = cache_dir,
      files = list(),     # or list("yourfile.rds") if you want to be strict
      logger = "INFO",
      quiet = FALSE, 
      timeout=600
    )
    
    rds_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(rds_files) == 0) stop("Download succeeded but no .rds found in cache_dir")
  }
}

load_zenodo_data <- function(CACHE_DIR) {

  
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

ZENODO_DOI <- "10.5281/zenodo.18327184"   # your DOI or concept DOI

get_zenodo_data(ZENODO_DOI, CACHE_DIR) 

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