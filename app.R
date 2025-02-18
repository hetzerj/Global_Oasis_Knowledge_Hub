library(shiny)
library(ggplot2)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(countrycode)
library(DT)
library(networkD3)

ui <- fluidPage(
  # general settings and links to scripts -------------------------------------------------------
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
  navbarPage(
    "Global Oasis Knowledge Hub",
    id = "main_tabs",
    
    # tabPanel: Home -------------------------------------------------------
    tabPanel("Home", div(
      class = "container",
      
      tags$img(src = "SNG_Logo_transperant.png", class = "logo"),
      
      div(
        class = "text-box",
        h1("Global Oasis Knowledge Hub"),
        h3(
          "A Senckenberg initiative for leveraging knowledge - supporting science and policy."
        )
      )
    )),
    
    
    
    # tabPanel: About -------------------------------------------------------
    tabPanel(
      "About", 
      div(
        class = "content",
        h2("The Global Oasis Knowledge Hub in Numbers"),
      
        fluidRow(
          column(5, offset=1, 
                 div(style="text-align: justify; margin-top:100px;",
                  h3("OPEN DATA", style="text-align: center"),
                    p("The Global OASIS Knowledge Hub is dedicated to making knowledge about OASIS findable and accessible to everyone. 
                      We provide a",strong("carefully curated set of references,"),"ensuring researchers can easily find what’s truly relevant to their needs. 
                      By exploring millions of entries from the global database OpenAlex, we’ve identified and included key references in this hub. 
                      With many of these being ", strong("open access,")," we make valuable insights easier to discover than ever before."))), 
          column(5, plotlyOutput("openAccessPie"))  # Number of references with percentage open access
                 ),
        
        fluidRow(style="background-color: rgba(160, 177, 203,0.3);",
          column(5, offset=1,
                div(plotlyOutput("dataTypePie"), style="margin-top:10px")), # First column for the third plot
          column(5, div(style = "text-align: justify; margin-top:110px;", 
                  h3("DATA TYPES", style="text-align: center"),
                  p("Our references originate from a ", strong("variety of sources"), 
                    " including articles, reports, book chapters, and datasets with a DOI. In particular, several references are sourced for data sets in GBIF (Global Biodiversity Information Facility). 
                      Please note that while we provide dataset references, we do not hist the datasets themselves.")))
                ),
        
        fluidRow(
          column(5,offset=1, style = "text-align: justify; margin-top:100px;" ,
                div(
                  h3("TEMPORAL COVERAGE", style = "text-align: center;"), 
                   p("We search for all available data—whether it was published a century ago or just last month. This broad temporal coverage ensures that the database captures the richness of past and present research, offering a comprehensive overview. 
                     To ensure the latest information is available we carry out ", tags$strong("monthly updates."),"The last update of this page was on: 01.02.2025"),
                   p(a(href = "https://doi.org/10.5281/zenodo.14203340", "See full updating history", style = "color: #D1B091; text-decoration: none; ")))),
           column(5, 
                 plotlyOutput("dataYearBar"))
              ),
        
        fluidRow(style="background-color: rgba(160, 177, 203,0.3)",
          column(11, offset=1, div(plotlyOutput("barplot_journals"), style="margin-top:100px;"))
              ),
       
        fluidRow( 
          column(5, offset=1, style = "text-align: justify; margin-top:100px;" ,
               div(
                h3("SPATIAL COVERAGE", style = "text-align: center;"), 
                  p("The Global Oasis Knowledge Hub is are committed to representing research from across the globe. Although our collection is diverse, we recognize that research from institutions in the Global South remains underrepresented. 
                    We believe that knowledge knows no boundaries and are determined to change this with the help of our global community. Your contributions can help us achieve this goal. 
                    If you have suggestions for literature from underrepresented regions or are interested in becoming a partner, please don't hesitate to contact us."),
                  p(a(href = "#", "Suggest New Literature", style = "color: #D1B091; text-decoration: none;"), " | ",  a(href = "#", "Become a Partner", style = "color: #D1B091; text-decoration: none;")))),
          column(5, div(
                 plotlyOutput("global_map_institute")), style="margin-top:100px;")
              ),
        
        fluidRow( style="background-color: rgba(160, 177, 203,0.3);",
          column(12, style = "margin-top: 50px;  text-align: center;" ,
                div(
                  h3("GET IN TOUCH WITH US"), 
                    p("You want to learn more about us?Visit our Credits."))),
              )
      )#closing div
    ),#closing tabPanel ABOUT
    
    
    
    # tabPanel: Knowledge Hub -------------------------------------------------------    
tabPanel("Knowledge Hub",
  fluidRow(
    column(8, offset=2,
           div(
             style = "background-color: #333; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
             h3(HTML("<i class='fas fa-search'></i> SEARCH & FILTER"), style="color: white; margin-bottom: 15px;"),
             fluidRow(
               column(3, 
                      selectInput("global_field", "", 
                                  choices = c("Search in all fields", "Title", "Authors", "Year", "Source"),
                                  selected = "Search in all fields")
               ),
               column(5, 
                      textInput("global_search", "", placeholder = "Enter search term")
               )
             )
           )
    )
  ),
  
  # Advanced Filtering Section (Black Background)
  fluidRow(
    column(8, offset=2, 
           div(
             style = "background-color: #333; padding: 10px; border-radius: 5px; margin-top: -10px;",
             actionButton("add_filter", "Advanced filter options", 
                          class = "btn btn-secondary", 
                          style = "color: black; background-color:rgba(160, 177, 203, 0.9);margin-left: 5px "),
             fluidRow(
               uiOutput("filter_rows")
             ),
              )
    )
  ),
  
  #downlod and visualize
  fluidRow(style="margin-top:10px",
           column(3, offset=3,
                  downloadButton("download_filtered_data", "DOWNLOAD", 
                                 class = "btn btn-primary", 
                                 style = "font-size: 22px; padding: 15px 30px; width: 100%;")
           ),
           column(3,
                  actionButton("go_visualize", label = HTML("<i class='fas fa-eye'></i> Visualize"), 
                               class = "btn btn-primary",
                               style = "font-size: 22px; padding: 15px 30px; width: 100%;")
           )
  ),

  # Data table
  fluidRow(
    DTOutput("data_table")
  )
),


    # tabPanel: Visualization ---------------------------------------------------------------------
tabPanel("Visualization",
         div(
           style = "position: relative; width: 100%; height: 100vh;",
           
           # Legend fixed on the top-left
           tags$iframe(
             src = "legend.html",
             style = "position: absolute; top: 10px; left: 20px; width: 250px; height: 220px; z-index: 1000; border: 5px solid rgba(160, 177, 203,1); background: white;"
           ),
           
           # Reset Button at the top-right
           actionButton(
             "reset_visualize", "Reset to full network",
             class = "btn btn-secondary",
             style = "position: absolute; top: 10px; right: 20px; font-size:24px;z-index: 1000; background-color:rgba(160, 177, 203,1)"
           ),
           
           # Visualization UI placeholder
           uiOutput("visualization_ui")
         )
),



    # tabPanel: Credits -------------------------------------------------------
    tabPanel("Credits",
             
             # Team Section
             fluidRow(
               column(12, h2("Team", align = "center")),
               
               # Centered text with proper alignment
               fluidRow(
                 column(12, align = "center",
                        column(6, offset = 3, 
                               h3("The Global Oasis Knowledge Hub is a non-profit initiative by the Senckenberg research institute in Frankfurt, with contributions from a global network of experts.", align = "center")
                        )
                 )
               ),
               
               fluidRow( style=" margin-top:50px",
               column(2,offset=2, align = "center",
                      img(src = "Jessica_Hetzer.jpeg", class = "team-img"),
                      p("Jessica Hetzer", style="font-weight: bold"),
                      p("Lead Developer & Concept Creator")
               ),
               
               column(2, align = "center",
                      img(src = "Aidin_Niamir.jpeg", class = "team-img"),
                      p("Aidin Niamir", style="font-weight: bold"),
                      p("Coordinator & Concept Creator")
               ),
               
               column(2, align = "center",
                      img(src = "Rainer_Krug.jpeg", class = "team-img"),
                      p("Rainer M. Krug", style="font-weight: bold"),
                      p("Software Developer")
               ),
               
               column(2, align = "center",
                      img(src = "Empty_profile.png", class = "team-img"),
                      p("Mechthilde Falkenhahn", style="font-weight: bold"),
                      p("Literature Review")
               )
               )
             ),

             
             # Advisory Board Section
             fluidRow( style="background-color: rgba(160, 177, 203, 0.3); margin-top:100px",
               column(12, h2("Advisory Members", align = "center")),
               
               column(12, style=" margin-top:50px",align = "center",
                      div(style="display: flex; justify-content: center;",
                          column(2, align = "center",
                                 img(src = "Klement_Tockner.jpeg",  class = "advisor-img"),
                                 p("Klement Tockner", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center",
                                 img(src = "Jonathan_Jeschke.jpeg", class = "advisor-img"),
                                 p("Jonathan Jeschke", style="font-weight: bold")
                          )
                      )
               )
             ),
             
             fluidRow( style="background-color: rgba(160, 177, 203,0.3);",
              
               
               column(12, align = "center",
                      div(style="display: flex; justify-content: center; gap: 20px;",
                          column(2, align = "center", style="margin-top:20px",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 1", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center", style="margin-top:20px",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 2", style="font-weight: bold")
                          ), 
                          column(2, align = "center", style="margin-top:20px",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 2", style="font-weight: bold")
                          )
                      )
               )
             ),

             
             # Partners Section
             fluidRow( 
               column(12, style="margin-top:50px", h2("Partners", align = "center")),
               
               fluidRow( style="margin-top:100px",
               column(3, align = "center",
                      img(src = "UNCCD.jpeg", class = "partner-logo-img"),
                      br(),
                      a("Visit Partner", href = "https://partner1.com", target="_blank")
               ),
               
               column(3, align = "center",
                      img(src = "Ramsar.jpeg", class = "partner-logo-img"),
                      br(),
                      a("Visit Partner", href = "https://partner2.com", target="_blank")
               ),
               
               column(3, align = "center",
                      img(src = "unesco_cairo.png", class = "partner-logo-img"),
                      br(),
                      a("Visit Partner", href = "https://partner3.com", target="_blank")
               ),
               
               column(3, align = "center",
                      img(src = "Empty_partner.png",  class = "partner-logo-img"),
                      br(),
                      a("Visit Partner", href = "https://partner4.com", target="_blank")
               ))
             )
    )
    # End of UI -------------------------------------------------------
  )
)



server <- function(input, output, session) {
  

# Statistics for ABOUT ------------------------------------------------------

  output$openAccessPie <- renderPlotly({
    table_open_access<-table(expanded_works$nodes$is_oa)

  pie_data <- data.frame(
    category = c("Limited Access", "Open Access"),
    value = as.numeric(table_open_access)
  )

    total_count <- sum(pie_data$value)  # Total number of papers

    plot_ly(pie_data, labels = ~category, values = ~value, type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'value',
            textposition = 'outside',     
            hole=0.4,
            textfont=list(size=18),
            marker = list(
              colors = col_diverging_scale(4)[c(2,3)],
              line = list(color = "black", width = 1.5) )
    ) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        annotations = list(
          text = paste(total_count, "References", sep="\n"),
          x = 0.5,
          y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      )
  })
  
  output$dataTypePie <- renderPlotly({
    table_type <- as.data.frame(table(expanded_works$nodes$type))
    colors <- col_diverging_scale(18)
    table_type$colors <- colors[1:nrow(table_type)]
    
    plot_ly(
      table_type,
      labels = ~paste0(Var1, " (", round(Freq / sum(Freq) * 100, 1), "%)"),  # Adding percentages dynamically
      values = ~Freq,
      type = 'pie',
      textinfo = 'none',  # Hide text inside the pie
      hoverinfo = 'label+percent+value',  # Show label, percentage & count on hover
      marker = list(
        colors = table_type$colors,
        line = list(color = "black", width = 1.5)
      ),
      hole = 0.4, 
      height = 400
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        legend = list(
          x = -0.3,
          y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 14)
        ),
        annotations = list(
          text = paste( nrow(table_type), "Data Types", sep = "\n"),
          x = 0.5,
          y = 0.5,
          font = list(size = 22),
          showarrow = FALSE
        )
      )
  })
  
  output$dataYearBar <- renderPlotly({
    table_year <- as.data.frame(table(expanded_works$nodes$publication_year))  # Count occurrences by year
    table_year <- table_year[order(table_year$Var1), ]  # Sort by year
    plot_ly(
      data = table_year,
      x = ~Var1,
      y = ~Freq,
      type = 'bar',
      textfont=list(size=20),
      marker = list(
        color = col_oasis[6],  # Assign colors from your color scale
        line = list(color = "black", width = 1.5)
      )
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Frequency"),
        font=list(size=14),
        margin = list(t = 40, b = 50, l = 50, r = 20),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        showlegend = FALSE,
        annotations = list(
            x = 0.1,  # Position at the top right
            y = 0.5,  # Slightly above the plot area
            text = paste("Covering ",  268, " years", sep = ""),
            showarrow = FALSE,
            font = list(size = 20, color = "#333"),
            bgcolor = "white",
            bordercolor = "#333",
            borderwidth = 1,
            borderpad = 4,
            xanchor = "left",
            yanchor = "top",
            xref = "paper",
            yref = "paper"
      ))
  })
  
  output$barplot_journals <- renderPlotly({
    table_journals <- as.data.frame(table(expanded_works$nodes$so))
    sorted_journals <- table_journals[order(table_journals$Freq, decreasing = TRUE), ]
    top20_journals <- sorted_journals[1:40, ]
    colnames(top20_journals) <- c("PublishedIn", "count")
    
    top20_journals$PublishedIn <- factor(top20_journals$PublishedIn, levels = top20_journals$PublishedIn)
    
    unique_journals_count <- nrow(table_journals)  # Count unique journals
    
    plot_ly(
      data = top20_journals,
      x = ~PublishedIn,
      y = ~count,
      type = 'bar',
      marker = list(color = col_diverging_scale(40), line = list(color = 'black', width = 1.5))
    ) %>%
      layout(
        font=list(size=14),
        xaxis = list(title = "", categoryorder = "array", categoryarray = top20_journals$PublishedIn),
        yaxis = list(title = "Frequency"),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        bargap = 0.2,
        annotations = list(
          list(
            x = 1.0,  # Position at the top right
            y = 1.1,  # Slightly above the plot area
            text = paste0( "<span style='font-size:24px;'>PUBLISHER</span><br>",
                          "\nIn total, the Global Oasis Knowlegde Hub provides references from ",unique_journals_count, " unique publisher.", "\nBelow you can find the 40 most frequent ones."),
            showarrow = FALSE,
            font = list(size = 16, color = "#333"),
            bgcolor = NA,
            bordercolor = NA,
            panel.grid = NA,  # Remove grid lines (optional)
            borderwidth = 1,
            borderpad = 4,
            xanchor = "right",
            yanchor = "top",
            xref = "paper",
            yref = "paper"
          )
        )
      )
  })
  
  output$global_map_institute <- renderPlotly({
  all_country_codes <- unlist(
    sapply(expanded_works$nodes$author, function(authors) {
      if (is.data.frame(authors) && "institution_country_code" %in% colnames(authors)) {
        authors$institution_country_code  # Return the column as a vector
      } else if (is.character(authors)) {
        authors  # Return the character vector directly
      } else {
        NA_character_  # Return NA for missing or unsupported values
      }
    })
  )

  # Remove NA values
  all_country_codes <- all_country_codes[!is.na(all_country_codes)]
  country_data<-as.data.frame(table(all_country_codes))
  colnames(country_data) <- c("country_code", "count")


  # Convert ISO2 to ISO3 for mapping
  country_data$iso_a3 <- countrycode(country_data$country_code, "iso2c", "iso3c")

  # Load world map with ISO3 country codes
  world_map <- ne_countries(scale = "medium", returnclass = "sf")  # Use spatial format (sf)

  # Merge the country data with the world map
  merged_data <- world_map %>%
    left_join(country_data, by = c("iso_a3"))

  # Compute centroids for labels
  centroids <- st_centroid(world_map) %>%
    st_coordinates() %>%
    as.data.frame()

  # Add country names and counts to centroids
  centroids$iso_a3 <- world_map$iso_a3
  centroids <- left_join(centroids, country_data, by = "iso_a3")

  # Rename columns for clarity
  colnames(centroids) <- c("longitude", "latitude", "iso_a3", "country_code","count")


    # Create the ggplot
    country_plot <- ggplot() +
      geom_sf(data = merged_data, aes(fill = count,
                                      text = paste(iso_a3, ":", count)),
              color = "#333", size = 0.1, na.rm = TRUE) +
      scale_fill_gradientn(
        colors = col_oasis[2:1],
        na.value = "gray",
        name = "Count"
      ) +
      coord_sf(crs = "+proj=eqearth") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Transparent plot background
        panel.grid.minor = element_line(color = "#333", size = 0.3)  # Remove grid lines (optional)
      )
    

    # Convert to plotly for interactivity
    ggplotly(country_plot, tooltip = c("text"))
  })
  

# Knowledge Hub Table search ----------------------------------------------

  key_data_df <- reactive({
    data.frame(
      oa_ID = expanded_works$nodes$id,  
      Authors = expanded_works$nodes$authors_short,
      Year = expanded_works$nodes$publication_year,
      Title = expanded_works$nodes$title,
      Source = paste0('<a href="', expanded_works$nodes$url, '" target="_blank">', expanded_works$nodes$url, '</a>'),
      stringsAsFactors = FALSE
    )
  })
  
  filter_list <- reactiveVal(list())
  
  observeEvent(input$add_filter, {
    new_filter <- list(
      id = paste0("filter_", length(filter_list()) + 1),
      operator = "AND",
      field = "Search in all fields",
      term = ""
    )
    filter_list(append(filter_list(), list(new_filter)))
  })
  
  output$filter_rows <- renderUI({
    tagList(
      lapply(seq_along(filter_list()), function(i) {
        fluidRow(
          column(2, offset=1, selectInput(paste0("operator_", i), "", choices = c("AND", "OR", "NOT"), selected = filter_list()[[i]]$operator)),
          column(3, selectInput(paste0("field_", i), "", 
                                choices = c("Search in all fields", "Title", "Authors", "Year", "Source"), 
                                selected = filter_list()[[i]]$field)),
          column(3, textInput(paste0("term_", i), "", placeholder = "Enter search term"))
        )
      })
    )
  })
  
  filtered_data <- reactive({
    df <- key_data_df()
    
    # Remove `oa_ID` from filtering/searching by working on a copy without `oa_ID`
    df_searchable <- df[, !colnames(df) %in% "oa_ID"]
    
    # Global search
    if (!is.null(input$global_search) && input$global_search != "") {
      if (input$global_field == "Search in all fields") {
        df_searchable <- df_searchable[apply(df_searchable, 1, function(row) any(grepl(input$global_search, row, ignore.case = TRUE))), ]
      } else {
        df_searchable <- df_searchable[grep(input$global_search, df_searchable[[input$global_field]], ignore.case = TRUE), ]
      }
    }
    
    # Process filter rows
    for (i in seq_along(filter_list())) {
      operator <- input[[paste0("operator_", i)]]
      field <- input[[paste0("field_", i)]]
      term <- input[[paste0("term_", i)]]
      
      if (!is.null(term) && term != "") {
        if (field == "Search in all fields") {
          match_rows <- apply(df_searchable, 1, function(row) any(grepl(term, row, ignore.case = TRUE)))
        } else {
          match_rows <- grepl(term, df_searchable[[field]], ignore.case = TRUE)
        }
        
        if (operator == "AND") {
          df_searchable <- df_searchable[match_rows, ]
        } else if (operator == "OR") {
          df_searchable <- rbind(df_searchable, df_searchable[match_rows, ])
        } else if (operator == "NOT") {
          df_searchable <- df_searchable[!match_rows, ]
        }
      }
    }
    
    df[df$Title %in% df_searchable$Title, ]
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_data()[, !colnames(filtered_data()) %in% "oa_ID"],  
      escape = FALSE,
      options = list(
        pageLength = 20,
        searchHighlight = TRUE,
        dom = 'tip'  
      )
    )
  })
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)  
    }
  )
  
  


# Visualization ----------------------------------------------------
  
  # Function to generate filtered visualization
  plot_snowball_interactive <- function(expanded_works, key_works, filtered_ids, 
                                        file_graph = "www/filtered_snowball.html", 
                                        file_legend = "www/legend.html") {
    
    ## Simple forceNetwork
    networkData <- data.frame(
      src = expanded_works$edges$from,
      target = expanded_works$edges$to,
      stringsAsFactors = FALSE
    )
    
    nodes <- data.frame(
      name = expanded_works$nodes$id,
      author = IPBES.R::abbreviate_authors(expanded_works$nodes),
      doi = expanded_works$nodes$doi,
      nodesize = expanded_works$nodes$cited_by_count / (2024 - expanded_works$nodes$publication_year) * 0.5,
      group = sapply(expanded_works$nodes$topics, function(x) {
        if ("display_name" %in% colnames(x)) {
          return(x$display_name[4])
        } else {
          return(NA)
        }
      }),
      stringsAsFactors = FALSE
    )
    
    # Ensure group column is valid
    nodes$group[is.na(nodes$group) | nodes$group == ""] <- "Unknown"
    
    nodes <- nodes[nodes$name %in% filtered_ids, ]
    
    # If filtered result is empty, generate an **empty** HTML and stop processing
    if (nrow(nodes) == 0) {
      writeLines("<html><body></body></html>", file_graph)  # Create an empty page
      return(list(graph = file_graph, legend = file_legend))
    }
    
    nodes$id <- seq_len(nrow(nodes)) - 1  
    
    edges <- networkData %>%
      filter(src %in% nodes$name & target %in% nodes$name) %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src, -author) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target, -author) %>%
      rename(target = id) %>%
      mutate(width = 1)
    
    if (nrow(edges) == 0) {
      # Create a dummy row with self-loop (won't be displayed)
      edges <- data.frame(source = 0, target = 0, width = 0)
    }
    
    
    # if (nrow(edges) == 0) {
    #   writeLines("<html><body></body></html>", file_graph)
    #   return(list(graph = file_graph, legend = file_legend))
    # }
    
    nodes$oa_id <- nodes$name
    nodes$name <- nodes$author
    
    # Unique groups for coloring
    unique_groups <- unique(nodes$group)
    
    color_palette <- c("#377eb8","#50521A","#794839", "#6a3d9a", "#e31a1c" ,"#333")[1:length(unique_groups)] 
    
    # color_palette <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
    #                    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
    #                    "#ccebc5", "#ffed6f")[1:length(unique_groups)]
    # 
    ColourScale <- sprintf(
      'd3.scaleOrdinal().domain(["%s"]).range(["%s"]);',
      paste(unique_groups, collapse = '", "'),
      paste(color_palette, collapse = '", "')
    )
    
    openDOI <- "window.open(d.doi)"
    
    
    nwg <- forceNetwork(
      Links = edges,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      NodeID = "name",
      Group = "group",
      Value = "width",
      opacity = 1,
      zoom = TRUE,
      colourScale = JS(ColourScale),
      fontSize = 20,
      legend = FALSE,
      clickAction = openDOI
    )
    
    
    nwg$x$nodes$doi <- nodes$doi
    
    # Save the graph as an HTML file
    networkD3::saveNetwork(nwg, file = file_graph, selfcontained = TRUE)
    
    # Generate legend HTML
    legend_html <- "<html><body><h3>Scientific Field</h3><ul style='list-style: none;'>"
    
    for (i in seq_along(unique_groups)) {
      legend_html <- paste0(
        legend_html,
        "<li style='margin:5px;'><span style='display:inline-block;width:15px;height:15px;background-color:",
        color_palette[i], ";border: 1px solid black;'></span> ",
        unique_groups[i], "</li>"
      )
    }
    
    legend_html <- paste0(legend_html, "</ul></body></html>")
    
    writeLines(legend_html, file_legend)
    
    # Return file paths
    list(graph = file_graph, legend = file_legend)
  } 
  
  
  # Reactive flag to track whether filtered visualization should be displayed
  show_filtered <- reactiveVal(FALSE)
  
  # Default: Show the full snowball visualization
  output$visualization_ui <- renderUI({
    if (show_filtered()) {
      tags$iframe(
        src = "filtered_snowball.html",
        style = "border: 5px solid #333; width: 100%; height: 100vh;"
      )
    } else {
      tags$iframe(
        src = "snowball_full.html",
        style = "border: 5px solid rgba(160, 177, 203,1); width: 100%; height: 100vh;"
      )
    }
  })
  
  # When "Visualize" is clicked, generate filtered visualization and update flag
  observeEvent(input$go_visualize, {
    updateTabsetPanel(session, "main_tabs", selected = "Visualization")
    
    # Extract filtered node IDs safely 🔧
    filtered_ids <- if (!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      filtered_data()$id
    } else {
      NULL
    }
    
    if (!is.null(filtered_ids)) {
      plot_snowball_interactive(expanded_works, key_works, filtered_ids)
      
      # Update reactive flag to show the filtered visualization
      show_filtered(TRUE)
      
      # *Explicitly trigger UI re-render**
      output$visualization_ui <- renderUI({
        tags$iframe(
          src = "filtered_snowball.html",
          style = "border: 5px solid #333; width: 100%; height: 100vh;"
        )
      })
    } 
  })
  
  # When "Reset Visualization" is clicked, switch back to the full snowball visualization
  observeEvent(input$reset_visualize, {
    show_filtered(FALSE)  # Reset to show full visualization
    
    # ✅ **Explicitly trigger UI re-render to show full visualization**
    output$visualization_ui <- renderUI({
      tags$iframe(
        src = "snowball_full.html",
        style = "border: 5px solid rgba(160, 177, 203,1); width: 100%; height: 100vh;"
      )
    })
  })
  
  }

shinyApp(ui = ui, server = server)
