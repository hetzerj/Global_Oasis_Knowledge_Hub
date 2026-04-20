library(ggplot2)
library(plotly)


generate_open_access_pie <- function(nodes_df, output_path, output) {
  if (!file.exists(output_path)) {
    table_open_access <- table(nodes_df$Open_access)
    pie_data <- data.frame(
      category = c("Limited Access", "Open Access"),
      value = as.numeric(table_open_access)
    )
    total_count <- sum(pie_data$value)
    saveRDS(list(pie_data = pie_data, total_count = total_count), output_path)
  }
  
  open_access_data <- readRDS(output_path)
  
  output$open_access_pie <- renderPlotly({
    pie_data <- open_access_data$pie_data
    total_count <- open_access_data$total_count
    hover_text <- paste("See", pie_data$value, "references")
    
    plot_ly(
      pie_data,
      labels = ~category,
      values = ~value,
      type = 'pie',
      textinfo = 'percent+label',
      text = hover_text,
      hoverinfo = 'text',
      textposition = 'outside',
      hole = 0.4,
      textfont = list(size = 18),
      marker = list(
        colors = col_diverging_scale(4)[c(2, 3)],
        line = list(color = "black", width = 1.5)
      ),
      source = "open_access_pie"
    ) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        annotations = list(
          text = paste(total_count, "References", sep = "\n"),
          x = 0.5, y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      ) %>%
      event_register("plotly_click")
  })
}

generate_data_year_barplot <- function(nodes_df, output_path, output) {
 
   if (!file.exists(output_path)) {
     years <- nodes_df$Year
     breaks <- seq(min(years, na.rm = TRUE) - 0.5, max(years, na.rm = TRUE) + 0.5, by = 1)
     
     h <- hist(years, breaks = breaks, plot = FALSE)
     
     saveRDS(list(mids = h$mids, counts = h$counts, breaks = h$breaks), output_path)
    }
  
     year_data <- readRDS(output_path)
     df_plot <- data.frame(
       year  = as.integer(year_data$mids),
       count = year_data$counts
     )
  
  # Replace 0s with NA to avoid log-scale display issues
  df_plot$count[df_plot$count == 0] <- NA
  
  output$data_year_barplot <- renderPlotly({
    plot_ly(
      data = df_plot,
      x = ~year,
      y = ~count,
      type = 'bar',
      marker = list(
        color = col_oasis[2],
        line = list(color = "black", width = 1.2)
      ),
      hoverinfo = "x+y",
      source = "data_year_barplot"   
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Year", tickangle = -45),
        yaxis = list(title = "Number of Publications", type = "log", autorange = TRUE),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      event_register("plotly_click")
  })
  
}

generate_data_type_pie <- function(nodes_df, output_path, output) {
  
  types <- nodes_df$Type
  types <- types[!is.na(types) & types != ""]
  
  table_type <- as.data.frame(table(types), stringsAsFactors = FALSE)
  colnames(table_type) <- c("Var1", "Freq")
  
  colors <- col_diverging_scale(18)
  table_type$colors <- colors[seq_len(nrow(table_type))]
  
  total_entries <- sum(table_type$Freq)
  distinct_count <- nrow(table_type)
  
  saveRDS(
    list(
      pie_data = table_type,
      total_entries = total_entries,
      distinct_count = distinct_count
    ),
    output_path
  )
  
  data_type_data <- readRDS(output_path)
  
  output$data_type_pie <- renderPlotly({
    pie_data <- data_type_data$pie_data
    total_entries <- data_type_data$total_entries
    distinct_count <- data_type_data$distinct_count
    
    plot_ly(
      pie_data,
      labels = ~paste0(
        Var1, ": ", Freq, " (", round(Freq / total_entries * 100, 1), "%)"
      ),
      values = ~Freq,
      type = "pie",
      textinfo = "none",
      hoverinfo = "label+values",
      hole = 0.4,
      height = 400,
      marker = list(
        colors = pie_data$colors,
        line = list(color = "black", width = 1.5)
      ),
      customdata = ~Var1,
      source = "data_type_pie"
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 80, r = 0),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        legend = list(
          x = 0.8, y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 14)
        ),
        annotations = list(
          text = paste0(distinct_count, "\nData types"),
          x = 0.5, y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      ) %>%
      event_register("plotly_click")
  })
}


generate_source_tree <- function(nodes_df, output_path, output) {
  if (!file.exists(output_path)) {
    table_journals <- as.data.frame(table(nodes_df$Source))
    sorted_journals <- table_journals[order(-table_journals$Freq), ]
    top_journals <- head(sorted_journals, 100)  # Top 100 sources
    colnames(top_journals) <- c("source", "count")  
    top_journals$label <- paste0(top_journals$source)

    saveRDS(list(
      treemap_data = top_journals
    ), output_path)
  }

  # Load intermediate data
  data_tree <- readRDS(output_path)
  source_data <- data_tree$treemap_data

  output$source_tree <- renderPlotly({
    plot_ly(
      type = "treemap",
      labels = source_data$label,
      parents = rep("", nrow(source_data)),  # flat hierarchy
      values = source_data$count,
      textinfo = "label",
      marker = list(colors = col_oasis_scale(nrow(source_data))),
      textfont = list(size = 18),
      customdata = source_data$source,
      source = "source_tree"
    ) %>%
      layout(
        margin = list(t = 1, l = 20, r = 20, b = 20),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)", pathbar = list(visible = FALSE) 
      ) %>%
      event_register("plotly_click")
  })

}

generate_treemap_journals <- function(nodes_df, output_path) {

  table_journals <- as.data.frame(table(nodes_df$Source))
  sorted_journals <- table_journals[order(-table_journals$Freq), ]
  top_journals <- head(sorted_journals, 100)
  colnames(top_journals) <- c("Journal", "Count")
  
  png(output_path, width = 1000, height = 500, bg = "transparent")
  
  treemap(
    top_journals,
    index = "Journal",
    vSize = "Count",
    type = "index",
    title = "Top 100 Sources",
    fontsize.labels = 12,
    fontcolor.labels = "black",
    bg.labels = "#FFFFFFA0",  # semi-transparent background for readability
    align.labels = list(c("center", "center")),
    border.col = "white",
    palette = col_oasis_scale(100)
  )
  
  dev.off()
}
