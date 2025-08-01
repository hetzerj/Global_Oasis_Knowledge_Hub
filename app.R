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
library(wordcloud2)
library(leaflet)
library(visNetwork)
library(scales)


# Start of User Interface ------------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),  # Enable shinyjs
  # Link to css file
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # Definition of page with navigation bar on the top with the name "Global Oasis Knowledge Hub"
  navbarPage("Global Oasis Knowledge Hub", id = "main_tabs",

    # Start tabPanel: Home ----------------------------------------------------------------------------------------------------------------
    tabPanel("Home",  #main landing page

            #create a container for a picture in the background and text in the front (see css for definition of picture)
            div(class = "container",
                div(class = "text-box",
                    h1("Global Oasis Knowledge Hub"),
                    h3("Senckenberg's initiative for leveraging knowledge - supporting science and policy.")))
            ),
    # End tabPanel: Home --------------------------------------------------------------------------------------------------------------------

    # Start tabPanel: Overview --------------------------------------------------------------------------------------------------------------
    tabPanel("Overview",  #page with summary statistics and brief information text

            #create a page with rows, each row shows a diffrent statitic. on 
            #on the left side a brief informatve text is shown, on the right side statistic is displayed in a interactive figure, 
            div(class = "content",
                #Title and subtitle 
                h1("The Global Oasis Knowledge Hub in numbers", style="margin-left:150px"),
                h3("Version 1.0, last update 27.04.2025", style=" margin-left:150px; color: rgba(160, 177, 203)"),
                
                #first row, displaying the number of references and the percentage of open access
                fluidRow(style="background-color: rgba(160, 177, 203,0.3);margin-top: 100px;",
                        column(4, offset=1, 
                        #explanation text for overview and open access
                        div(style="text-align: justify; margin-top: 100px;",
                            h3("OPEN DATA", style="text-align: center"),
                            p("The Global OASIS Knowledge Hub serves as a trusted platform for facilitating the efficient discovery of knowledge about oases.  
                              It provides a carefully curated selection of references.  
                              With a significant portion offered as open access, valuable research findings are only a click away."))), 
                        #figure for overview and open access 
                        column(5, offset=1, plotlyOutput("open_access_pie"))  # Number of references with percentage open access
                        ),
                
                # second row displaying the time frame and how many references per year
                fluidRow(column(4,offset=1, style = "text-align: justify; margin-top:100px;" ,
                        #explanation text for time frame of data 
                        div(h3("TEMPORAL COVERAGE", style = "text-align: center;"), 
                            p("All available data is incorporated, irrespective of its publication date, whether from a century ago or the past month. 
                              This extensive temporal scope ensures the representation of both historical and contemporary research, 
                              providing a comprehensive and balanced overview."))),
                        #figure for time frame of data 
                        column(5, offset=1, plotlyOutput("data_year_barplot"))
                        ),
                
                # third row displaying the data types 
                fluidRow(style="background-color: rgba(160, 177, 203,0.3);",
                        column(4, offset=1, 
                        # explanation on data types        
                        div(style = "text-align: justify; margin-top:110px;", 
                        h3("DATA TYPES", style="text-align: center"),
                        p("Data originates from a diverse range of sources including articles, reports, book chapters, and datasets with a DOI. 
                          While dataset references are provided, the datasets themselves are not hosted on this platform.")
                          )
                        ),   
                        # figure on data types
                        column(5, offset=1, plotlyOutput("data_type_pie"))
                ), 
        
              # # fourth row for showing in which country the authors institutes are located
              #  div(style = "display: flex; flex-direction: column; align-items: center; height: 900px;",
              #       h3("GLOBAL DISTRIBUTION OF AUTHORS", style = " margin-top: 50px;"),
              #      plotlyOutput("global_map_institute", width = "1500px", height = "700px")
              #    ),
              # 
              #  
              
              # fourth row for showing in which country the authors institutes are located
               div(style = "display: flex; flex-direction: column; align-items: center; height: 2000px;",
                    h3("TOP SOURCES", style = " margin-top: 50px;"),
                   plotlyOutput("source_tree", width = "1500px", height = "2000px")
                 ),

               #  #fifth  and sixth row showing the most common institutes on the top and the most common sources on the bottom
               # fluidRow(
               #   column(5,offset=1, div(h3("TOP SOURCES", style="text-align:center"))),
               #   column(10,offset=1, plotlyOutput("source_tree")),
               #    # seventh row showing which topics are included   
               #    # column(5,offset=1, div(h3("TOP TOPICS", style="text-align:center"))),
               #    # column(10,offset=1, div(wordcloud2Output("wordcloud_topics")))
               #   ),
               # 
           
                #end of page referencing to "LEARN MORE" leading to the 'about' page  
                fluidRow(
                   style="background-color: rgba(160, 177, 203,0.3); margin-top: 50px;",
                   column(12, style = "margin-top: 50px; text-align: center; margin-bottom: 50px;",
                      div(
                        h3("LEARN MORE"), 
                        p("You want to learn more about us and our partners? Visit 'About'." 
                          ))))
        )),
    # End tabPanel: Overview ----------------------------------------------------------------------------------------------------------------
    
    # tabPanel: Knowledge Hub -------------------------------------------------------    
    tabPanel("Knowledge Hub", # page that provides easy access to the data including advances filtering and download options
        
        # Simple search tool in which a drop-down provides an selection of fields to choose and a text-input on the left in which the search term can be written in
        fluidRow(
          column(8, offset=1,
           div(style = "background-color: grey; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
             h3(HTML("<i class='fas fa-search'></i> SEARCH"), style="color: white; margin-bottom: 15px;"),
             fluidRow(column(3, selectInput("global_field", "", 
                                    choices = c("Search in all fields", "Title", "Authors", "Year", "Source","Type", "Open_access"),
                                    selected = "Search in all fields")),
                      column(5, textInput("global_search", "", placeholder = "Enter search term")))))
                  ),
  
        # Advanced Filtering Section 
        # Possibility to create additional search lines with drop-down selection of fields as well as text-input, additionally selection can be logically connected (AND/OR/NOT), for clearing of the whole line trash-icon removes the line
         fluidRow(
          column(8, offset=1, 
           div(style = "background-color: grey; padding: 10px; border-radius: 5px; margin-top: -10px;",
             actionButton("add_filter", "Advanced filter options", 
                          class = "btn btn-secondary", 
                          style = "color: #333; background-color:white;margin-left: 5px "),
             fluidRow(uiOutput("filter_rows"))
                    )
                  )
                ),
  
         # Download button for filtered table of data entries (csv-file)
          fluidRow(style="margin-top:10px; item-align:center",
           column(2, offset=1,
            downloadButton("download_filtered_data", "DOWNLOAD", 
                          class = "btn btn-primary", 
                          style = "font-size: 22px; padding: 15px 30px; width: 100%;")
                  )
                ),

        # output of filtered data table (displays all entries by default)
          fluidRow(
            DTOutput("data_table")
                  )
      
        ),
    # End tabPanel: Knowledge Hub -----------------------------------------------------------------------------------------------------------
    
    tabPanel("Visualization",
             div(style = "position: relative; width: 100%; height: auto; padding-bottom: 50px;",
                 fluidRow(
                   column(id = "map_col", width = 8,
                          h2("Select an oasis", style = "text-align:center"),
                          leafletOutput("map", height = "600px"),
                          tags$div(style = "margin-top: 10px; margin-left: 10px;",
                                   HTML('<p style="font-size: 16px;">
              <strong>Is there an important reference missing?</strong><br>
              We regularly update our database and appreciate your suggestions.
              Please feel free to reach out via our 
              <a href="https://example.com/contact" target="_blank">contact form</a>.  
            </p>')
                          )
                   ),
                   column(id = "net_col", width = 4,
                          h2("Discover the knowledge network", style = "text-align:center"),
                          conditionalPanel("output.subnetShown",
                                           tags$div(style = "margin-top: 30px;",
                                                    actionButton("back", "Back to global selection"),
                                                    visNetworkOutput("subnetwork", height = "600px")
                                           )
                          )
                   )
                 )
             )
    ),
    
    
    
    #     tabPanel("Visualization",
    #          div(style = "position: relative; width: 100%; height: auto; padding-bottom: 50px;",
    #              fluidRow(column(width = 8,  
    #                  h2("Select an oasis", style = "text-align:center"),
    #                  leafletOutput("map", height = "600px"),
    #                  tags$div(style = "margin-top: 10px; margin-left: 10px;",
    #                           HTML('<p style="font-size: 16px;">
    #                               <strong>Is there an important reference missing?</strong><br>
    #                                    We regularly update our database and appreciate your suggestions.
    #                          Please feel free to reach out via our <a href="https://example.com/contact" target="_blank">contact form</a>.  </p>'))),
    #                column(width = 4,
    #                  h2("Discover the knowledge network", style = "text-align:center"),
    #                  conditionalPanel("output.subnetShown",
    #                                 
    #                                   tags$div(style = "margin-top: 30px;",
    #                                            actionButton("back", "Back to global selection"),
    #                                            visNetworkOutput("subnetwork", height = "600px")
    #                                   )
    #                  )
    #                )
    #              )
    #          )
    # ),
    # End of Visualization ------------------------------------------------------------------------------------------------------------------
    
    # tabPanel: About -------------------------------------------------------
    tabPanel("About", #Page provides information about the general structure and involved coordinators
              
            value = "about",
            tags$img(src = "Backround_pictures_and_logos/SNG_Logo_transperant.png", class = "logo",  style="z-index: 1000; margin-top: 100px;margin-right: 50px;display: flex; "),
            div(style = "max-width: 1100px; margin: auto; padding: 20px;",
               
              #First row: General information about the Global Oasis Knowledge Hub  
              h2("About the Global Oasis Knowledge Hub", style = "text-align:center"),
               h3("General Structure"),
               p("The Global OASIS Knowledge Hub is an initiative by Senckenberg, 
                dedicated to fostering a deeper understanding of oasis ecosystems worldwide. 
                As an interdisciplinary platform, it connects science and policy to study, protect, 
                and sustainably manage these vital ecosystems."),
               p("The hub is structured as a dynamic and evolving interconnected literature network. 
                At its core is a collection of trusted, peer-reviewed literature, carefully reviewed 
                by oasis experts. This knowledge base is continuously expanded and refined to reflect 
                the latest research and developments in oasis ecosystems."),
               h3("Regular Updates and Literature Review"),
               p("To ensure it stays up to date, the hub integrates new scientific findings on a monthly basis 
                via the Global Reference Databank OpenAlex. In addition, an expert panel reviews and incorporates new literature suggestions 
                every six months to enhance the knowledge base."),
               p("To maintain transparency, the full history of updates is archived on ", 
                a(href = "https://zenodo.org/records/1420334", "Zenodo,"), 
                "allowing users to track the evolution of the knowledge hub. The underlying code used 
                for developing and maintaining this knowledge network is openly available on ", 
                a(href = "https://github.com/hetzerj/Global_Oasis_Knowledge_Hub", "GitHub,"), 
                "ensuring collaboration and reproducibility in research."),
               h3("Contact and Feedback"),
               div(style = "display: flex; gap: 20px; align-items: center;",
                 div(style = "display: flex; align-items: center; gap: 5px;",
                   tags$a(
                     href = "mailto:oasis@senckenberg.de", 
                     target = "_blank",
                     tags$i(class = "fa fa-envelope", style = "font-size: 24px; color: #007BFF; margin-right: 5px;")),
                   p("General Inquiries", style = "margin: 0;")),
                  div(style = "display: flex; align-items: center; gap: 5px;",
                   tags$a(
                     href = "mailto:jessica.hetzer@senckenberg.de", 
                     target = "_blank",
                     tags$i(class = "fa fa-envelope", style = "font-size: 24px; color: #007BFF; margin-right: 5px;")),
                   p("Technical Support", style = "margin: 0;"))
                )
              ),

             #Second row: Advisory Board Overview
             fluidRow(style="background-color: rgba(160, 177, 203, 0.3); margin-top:100px; margin-bottom:100px; align:center",
               column(12, h2("Advisory Board", align = "center")),
               #roles and pictures
               column(12, style=" margin-top:50px",align = "center",
                      div(style="justify-content: center;",
                          column(2, offset=2, align = "center",
                                 img(src = "About_profiles/Klement_Tockner.jpeg",  class = "advisor-img")),
                          column(2, align="left", 
                                 p(" Prof. Dr. Klement Tockner", style="font-weight: bold"), 
                                 p("Director General of the Senckenberg Society"), 
                                 p("Expert on dynamics, biodiversity and sustainable management of water bodies")),
                          column(2,align = "center",
                                 img(src = "About_profiles/Jonathan_Jeschke.jpeg", class = "advisor-img")),
                          column(2, align="left", 
                                 p("Prof. Dr. Jonathan Jeschke", style="font-weight: bold"), 
                                 p("Department Head of Evolutionary and Integrative Ecology, IGB Berlin"), 
                                 p("Developer of Knowledge Maps")))),
               column(12, style=" margin-top:50px; margin-bottom:50px;",align = "center",
                      div(style="justify-content: center;",
                          column(2, offset=2,align = "center",
                                 img(src = "About_profiles/Mechthilde_Falkenhahn.jpeg",  class = "advisor-img")),
                          column(2, align="left", 
                                 p("Mechthilde Falkenhahn", style="font-weight: bold"), 
                                 p("Oasis Expert"), 
                                 p("Initial literature review")),
                          column(2, align = "center",
                                 img(src = "About_profiles/Juan_A_H_Aguero.jpeg", class = "advisor-img")),
                          column(2, align="left", 
                                 p("Dr. Juan Antonio Hernández Agüero", style="font-weight: bold"), 
                                 p("Oasis Expert"))))
              ),
            
            #Third row: Oasis Knowledge Hub Team 
            fluidRow(align = "center",
                # Information text
                column(12, h2("Global Oasis Knowledge Hub Team", align = "center")),
                fluidRow(column(12, align = "center",
                        column(6, offset = 3, 
                               h3("The Global Oasis Knowledge Hub is a non-profit initiative by the Senckenberg research institute in Frankfurt, 
                                  with contributions from a global network of experts.", align = "center")))),
                # Team roles and pictures
                fluidRow( style=" margin-top:50px",
                         column(2,offset=3, align = "center",
                                img(src = "About_profiles/Jessica_Hetzer.jpeg", class = "team-img"),
                                p("Dr. Jessica Hetzer", style="font-weight: bold"),
                                p("Technical ans Scientific Coordinator")),
                         column(2, align = "center",
                                img(src = "About_profiles/Aidin_Niamir.jpeg", class = "team-img"),
                                p("Dr. Aidin Niamir", style="font-weight: bold"),
                                p("Concept Coordinator")),
                         column(2, align = "center",
                                img(src = "About_profiles/Rainer_Krug.jpeg", class = "team-img"),
                                p("Dr. Rainer M. Krug", style="font-weight: bold"),
                                p("Technical Advisor"))
                        )
                    )
    )
    # End of Visualization ------------------------------------------------------------------------------------------------------------------
   
  )
) 
# End of User Interface --------------------------------------------------------------------



# Start of Server functionalities ----------------------------------------------
server <- function(input, output, session) {
  
  # Start Statistics for Overview  ----------------------------------------------------------------------------------------------------------
  
  source("R/generate_overview_plots.R")
  
  # --- OPEN ACCESS PIEPLOT -------------------------------------------------------
  data_open_access_path <- file.path("www/Overview_figures",
                                      paste0("open_access_pie_v", GOKH_version, ".rds"))
  generate_open_access_pie(expanded_works, data_open_access_path, output)

  # --- DATA YEAR BARPLOT ---------------------------------------------------------
  data_year_barplot_path <- file.path("www/Overview_figures",
                                      paste0("data_year_barplot_v", GOKH_version, ".rds"))
  generate_data_year_barplot(expanded_works, data_year_barplot_path, output)
  
  
  # --- DATA TYPE PIE -------------------------------------------------------------
  data_type_pie_path <- file.path("www/Overview_figures",
                                  paste0("data_type_pie_v", GOKH_version, ".rds"))
  generate_data_type_pie(expanded_works, data_type_pie_path, output)
  
  
  # --- SOURCE TREEMAP -----------------------------------------------------------
  treemap_sources_path <- file.path("www/Overview_figures",
                                     paste0("treemap_sources_path_v", GOKH_version, ".rds"))
  
  generate_source_tree(expanded_works, treemap_sources_path , output)
 
  # End Statistics for Overview  -----------------------------------------------------------------------------------------------------------
  
  # Knowledge Hub Table search ----------------------------------------------
  
  #1. Prepare full data table 
  key_data_df <- reactive({
    data.frame(
      oa_ID = expanded_works$nodes$id,  
      #Authors = expanded_works$nodes$authors_short,
      Year = expanded_works$nodes$publication_year,
      Title = expanded_works$nodes$title,
      Type = expanded_works$nodes$type,
      Open_access = expanded_works$nodes$is_oa,
      Source = paste0('<a href="', expanded_works$nodes$landing_page_url, '" target="_blank">', expanded_works$nodes$source_display_name, '</a>'),
      stringsAsFactors = FALSE
    )
  })
  
  
  #2. Dynamic Filters: Create, Add, Remove
  # Store list of user-added filters (each filter has an ID, field, term, operator)
  filter_list <- reactiveVal(list())

  # Rendering of dynamic filtering rows below 
  output$filter_rows <- renderUI({
    tagList(
      lapply(seq_along(filter_list()), function(i) {
        fluidRow(
          column(1,offset = 1,style="margin-top:20px",  actionButton(paste0("remove_", i), "", icon = icon("trash"), 
                                                                     class = "filter-remove-btn")), # Small delete button
          column(2, 
                 selectInput(paste0("operator_", i), "", choices = c("AND", "OR", "NOT"), 
                             selected = filter_list()[[i]]$operator)),
          column(3, selectInput(paste0("field_", i), "", 
                                choices = c("Search in all fields", "Title", "Authors", "Year", "Source"), 
                                selected = filter_list()[[i]]$field)),
          column(3, textInput(paste0("term_", i), "", placeholder = "Enter search term"))
          
        )
      })
    )
  })
  
  # When "Add Filter" button is clicked, add a new filter row and append the filterlist by this filter
  observeEvent(input$add_filter, {
    new_filter <- list(
      id = paste0("filter_", length(filter_list()) + 1),
      operator = "AND",
      field = "Search in all fields",
      term = ""
    )
    filter_list(append(filter_list(), list(new_filter)))
  })
  
  # When "Remove Filter" button is clicked, then remove the row and the filter from the filterlist 
  observe({
    lapply(seq_along(filter_list()), function(i) {
      observeEvent(input[[paste0("remove_", i)]], {
        updated_filters <- filter_list()
        updated_filters <- updated_filters[-i]
        filter_list(updated_filters)
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  
  
  #2.1 additional filters from figures 
  
  
  # observeEvent(event_data("plotly_click", source = "open_access_pie"), {
  #   click_data <- event_data("plotly_click", source = "open_access_pie")
  #   print(click_data)  # Check what's coming through
  # }) 
  observeEvent(event_data("plotly_click", source = "open_access_pie"), {
    click_data <- event_data("plotly_click", source = "open_access_pie")
    
    if (!is.null(click_data$pointNumber)) {
      # Convert pointNumber to logical string
      label_clicked <- if (click_data$pointNumber == 1) "TRUE" else "FALSE"
      
      updateTabsetPanel(session = session, inputId = "main_tabs", selected = "Knowledge Hub")
      updateSelectInput(session = session, inputId = "global_field", selected = "Open_access")
      updateTextInput(session = session, inputId = "global_search", value = label_clicked)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "data_year_barplot"), {
    click_data <- event_data("plotly_click", source = "data_year_barplot")
    
    if (!is.null(click_data)) {
      updateTabsetPanel(session = session, inputId = "main_tabs", selected = "Knowledge Hub")
      updateSelectInput(session = session, inputId = "global_field", selected = "Year")
      updateTextInput(session = session, inputId = "global_search", value = click_data$x)
    }

  })
  
  observeEvent(event_data("plotly_click", source = "data_type_pie"), {
    click_data <- event_data("plotly_click", source = "data_type_pie")
    if (!is.null(click_data)) {
      # You can now use `clicked_type` to update search or UI
      updateTabsetPanel(session = session, inputId = "main_tabs", selected = "Knowledge Hub")
      updateSelectInput(session = session, inputId = "global_field", selected = "Type")
      updateTextInput(session = session, inputId = "global_search", value = click_data$customdata)
    }
  })
  
  
  observeEvent(event_data("plotly_click", source = "source_tree"), {
    click_data <- event_data("plotly_click", source = "source_tree")
    if (!is.null(click_data$customdata)) {
      clicked_type <- click_data$customdata
      print(paste("Treemap clicked on type:", clicked_type))
      
      # Update input fields or filter
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Knowledge Hub")
      updateSelectInput(session, inputId = "global_field", selected = "Source")
      updateTextInput(session, inputId = "global_search", value = clicked_type)
    }
  })
  
  # 
  #3. After every action react with showing only those entries that fit into the filters
  filtered_data <- reactive({
    #get the full data table 
    df <- key_data_df()
    
    # remove the oa_ID column
    df_searchable <- df
    # 
    
    #Standard filtering by first filtering row
    if (!is.null(input$global_search) && input$global_search != "") {
      if (input$global_field == "Search in all fields") {
        # if search in all fields 
        df_searchable <- df_searchable[
          apply(df_searchable, 1, function(row) any(grepl(input$global_search, row, ignore.case = TRUE))),
        ]
      } else {
        # if search in one specific field
        df_searchable <- df_searchable[
          grep(input$global_search, df_searchable[[input$global_field]], ignore.case = TRUE),
        ]
      }
    }
    
    # Process dynamic filter rows
    for (i in seq_along(filter_list())) {
      operator <- input[[paste0("operator_", i)]]
      field <- input[[paste0("field_", i)]]
      term <- input[[paste0("term_", i)]]
      
      if (!is.null(term) && term != "") {
        if (field == "Search in all fields") {
          # if search in all fields 
          match_rows <- apply(df_searchable, 1, function(row) any(grepl(term, row, ignore.case = TRUE)))
        } else {
          # if search in one specific field
          match_rows <- grepl(term, df_searchable[[field]], ignore.case = TRUE)
        }
        
        # apply logic that was selected
        if (operator == "AND") {
          df_searchable <- df_searchable[match_rows, ]
        } else if (operator == "OR") {
          df_searchable <- rbind(df_searchable, df_searchable[match_rows, ])
        } else if (operator == "NOT") {
          df_searchable <- df_searchable[!match_rows, ]
        }
      }
    }
    
    # Return rows from full df that match filtered search
    df[df$oa_ID %in% df_searchable$oa_ID, ]
  })  
 
  #4. Render final table to a DTable (easy functionalities like sorting included) 
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
  
  #5. Download the (filtered) References as a csv file 
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)  
    }
  )
  


  
# Visualization ----------------------------------------------------
  
  # # Function to generate filtered visualization
  # plot_snowball_interactive <- function(expanded_works, key_works, filtered_ids, 
  #                                       file_graph = "www/filtered_snowball.html", 
  #                                       file_legend = "www/legend.html") {
  #   
  #   ## Simple forceNetwork
  #   networkData <- data.frame(
  #     src = expanded_works$edges$from,
  #     target = expanded_works$edges$to,
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   nodes <- data.frame(
  #     name = expanded_works$nodes$id,
  #     author = IPBES.R::abbreviate_authors(expanded_works$nodes),
  #     doi = expanded_works$nodes$doi,
  #     nodesize = expanded_works$nodes$cited_by_count / (2024 - expanded_works$nodes$publication_year) * 0.5,
  #     group = sapply(expanded_works$nodes$topics, function(x) {
  #       if ("display_name" %in% colnames(x)) {
  #         return(x$display_name[4])
  #       } else {
  #         return(NA)
  #       }
  #     }),
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   # Ensure group column is valid
  #   nodes$group[is.na(nodes$group) | nodes$group == ""] <- "Unknown"
  #   
  #   nodes <- nodes[nodes$name %in% filtered_ids, ]
  #   
  #   # If filtered result is empty, generate an **empty** HTML and stop processing
  #   if (nrow(nodes) == 0) {
  #     writeLines("<html><body></body></html>", file_graph)  # Create an empty page
  #     return(list(graph = file_graph, legend = file_legend))
  #   }
  #   
  #   nodes$id <- seq_len(nrow(nodes)) - 1  
  #   
  #   edges <- networkData %>%
  #     filter(src %in% nodes$name & target %in% nodes$name) %>%
  #     left_join(nodes, by = c("src" = "name")) %>%
  #     select(-src, -author) %>%
  #     rename(source = id) %>%
  #     left_join(nodes, by = c("target" = "name")) %>%
  #     select(-target, -author) %>%
  #     rename(target = id) %>%
  #     mutate(width = 1)
  #   
  #   if (nrow(edges) == 0) {
  #     # Create a dummy row with self-loop (won't be displayed)
  #     edges <- data.frame(source = 0, target = 0, width = 0)
  #   }
  #   
  #   
  #   # if (nrow(edges) == 0) {
  #   #   writeLines("<html><body></body></html>", file_graph)
  #   #   return(list(graph = file_graph, legend = file_legend))
  #   # }
  #   
  #   nodes$oa_id <- nodes$name
  #   nodes$name <- nodes$author
  #   
  #   # Unique groups for coloring
  #   unique_groups <- unique(nodes$group)
  #   
  #   color_palette <- c("#377eb8","#50521A","#794839", "#6a3d9a", "#e31a1c" ,"#333")[1:length(unique_groups)] 
  #   
  #   # color_palette <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
  #   #                    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
  #   #                    "#ccebc5", "#ffed6f")[1:length(unique_groups)]
  #   # 
  #   ColourScale <- sprintf(
  #     'd3.scaleOrdinal().domain(["%s"]).range(["%s"]);',
  #     paste(unique_groups, collapse = '", "'),
  #     paste(color_palette, collapse = '", "')
  #   )
  #   
  #   openDOI <- "window.open(d.doi)"
  #   
  #   
  #   nwg <- forceNetwork(
  #     Links = edges,
  #     Nodes = nodes,
  #     Source = "source",
  #     Target = "target",
  #     NodeID = "name",
  #     Group = "group",
  #     Value = "width",
  #     opacity = 1,
  #     zoom = TRUE,
  #     colourScale = JS(ColourScale),
  #     fontSize = 20,
  #     legend = FALSE,
  #     clickAction = openDOI
  #   )
  #   
  #   
  #   nwg$x$nodes$doi <- nodes$doi
  #   
  #   # Save the graph as an HTML file
  #   networkD3::saveNetwork(nwg, file = file_graph, selfcontained = TRUE)
  #   
  #   # Generate legend HTML
  #   legend_html <- "<html><body><h3>Scientific Field</h3><ul style='list-style: none;'>"
  #   
  #   for (i in seq_along(unique_groups)) {
  #     legend_html <- paste0(
  #       legend_html,
  #       "<li style='margin:5px;'><span style='display:inline-block;width:15px;height:15px;background-color:",
  #       color_palette[i], ";border: 1px solid black;'></span> ",
  #       unique_groups[i], "</li>"
  #     )
  #   }
  #   
  #   legend_html <- paste0(legend_html, "</ul></body></html>")
  #   
  #   writeLines(legend_html, file_legend)
  #   
  #   # Return file paths
  #   list(graph = file_graph, legend = file_legend)
  # } 
  # 
  # 
  # # Reactive flag to track whether filtered visualization should be displayed
  # show_filtered <- reactiveVal(FALSE)
  # 
  # # Default: Show the full snowball visualization
  # output$visualization_ui <- renderUI({
  #   if (show_filtered()) {
  #     tags$iframe(
  #       src = "filtered_snowball.html",
  #       style = "border: 5px solid #333; width: 100%; height: 100vh;"
  #     )
  #   } else {
  #     tags$iframe(
  #       src = "snowball_full.html",
  #       style = "border: 5px solid rgba(160, 177, 203,1); width: 100%; height: 100vh;"
  #     )
  #   }
  # })
  # 
  # # When "Visualize" is clicked, generate filtered visualization and update flag
  # observeEvent(input$go_visualize, {
  #   updateTabsetPanel(session, "main_tabs", selected = "Visualization")
  #   
  #   # Extract filtered node IDs safely 🔧
  #   filtered_ids <- if (!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
  #     filtered_data()$id
  #   } else {
  #     NULL
  #   }
  #   
  #   if (!is.null(filtered_ids)) {
  #     plot_snowball_interactive(expanded_works, key_works, filtered_ids)
  #     
  #     # Update reactive flag to show the filtered visualization
  #     show_filtered(TRUE)
  #     
  #     # *Explicitly trigger UI re-render**
  #     output$visualization_ui <- renderUI({
  #       tags$iframe(
  #         src = "filtered_snowball.html",
  #         style = "border: 5px solid #333; width: 100%; height: 100vh;"
  #       )
  #     })
  #   } 
  # })
  # 
  # # When "Reset Visualization" is clicked, switch back to the full snowball visualization
  # observeEvent(input$reset_visualize, {
  #   show_filtered(FALSE)  # Reset to show full visualization
  #   
  #   # ✅ **Explicitly trigger UI re-render to show full visualization**
  #   output$visualization_ui <- renderUI({
  #     tags$iframe(
  #       src = "snowball_full.html",
  #       style = "border: 5px solid rgba(160, 177, 203,1); width: 100%; height: 100vh;"
  #     )
  #   })
  # })
  
  
  subnet_trigger <- reactiveVal(NULL)
  subnet_displayed <- reactiveVal(FALSE)
  
  observe({
    if (subnet_displayed()) {
      shinyjs::removeClass("map_col", "col-md-8")
      shinyjs::addClass("map_col", "col-md-5")
      
      shinyjs::removeClass("net_col", "col-md-4")
      shinyjs::addClass("net_col", "col-md-7")
    } else {
      shinyjs::removeClass("map_col", "col-md-5")
      shinyjs::addClass("map_col", "col-md-8")
      
      shinyjs::removeClass("net_col", "col-md-7")
      shinyjs::addClass("net_col", "col-md-4")
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(global_nodes) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        label = ~title,
        layerId = ~id,
        clusterOptions = markerClusterOptions()
      )
  })
  
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$id)
    node_id <- as.character(input$map_marker_click$id)
    
    # Always reset before setting, to ensure a change is detected
    subnet_trigger(NULL)
    subnet_trigger(node_id)
    subnet_displayed(TRUE)
    
    leafletProxy("map") %>%
      setView(lng = input$map_marker_click$lng, lat = input$map_marker_click$lat, zoom = 6)
  })
  
  observeEvent(input$back, {
    subnet_displayed(FALSE)
    subnet_trigger(NULL)
    leafletProxy("map") %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  output$subnetShown <- reactive({
    subnet_displayed()
  })
  outputOptions(output, "subnetShown", suspendWhenHidden = FALSE)
  
  
   output$subnetwork <- renderVisNetwork({
    req(subnet_trigger())
    
    center_id <- subnet_trigger()
    nodes_df <- expanded_works$nodes
    edges_df <- expanded_works$edges
    
    sub_edges <- edges_df %>% filter(from == center_id | to == center_id)
    if (nrow(sub_edges) == 0) return(NULL)
    
    outgoing_ids <- sub_edges$to[sub_edges$from == center_id]
    incoming_ids <- sub_edges$from[sub_edges$to == center_id]
    connected_ids <- unique(c(sub_edges$from, sub_edges$to))
    sub_nodes <- nodes_df %>% filter(id %in% connected_ids)
    
    sub_nodes$group <- ifelse(
      sub_nodes$id == center_id, "Reviewed",
      ifelse(sub_nodes$id %in% outgoing_ids, "Cited", "New citing ")
    )
    
    sub_nodes$internal_id <- 1:nrow(sub_nodes)
    id_map <- setNames(sub_nodes$internal_id, sub_nodes$id)
    
    # Fix X and Y positions
    
    sub_nodes$x <- rescale(sub_nodes$publication_year, to = c(-400, 400))
    
    sub_nodes$y <- ifelse(
      sub_nodes$id %in% outgoing_ids,
      seq(-200, 200, length.out = length(outgoing_ids)),
      ifelse(
        sub_nodes$id %in% incoming_ids,
        seq(-200, 200, length.out = length(incoming_ids)),
        0
      )
    )
    
    sub_nodes$group <- ifelse(
      sub_nodes$id == center_id, "Reviewed",
      ifelse(sub_nodes$id %in% outgoing_ids, "Cited", "New citing ")
    )
    # sub_nodes$node_color <- ifelse(
    #   sub_nodes$id == center_id, "#0072B2",  # center = dark blue
    #   ifelse(sub_nodes$id %in% incoming_ids, "#56B4E9", "#E69F00")  # citing = blue, cited = orange
    # )
    
    vis_nodes <- data.frame(
      id = sub_nodes$internal_id,
      group = sub_nodes$group,
      title = paste(sub_nodes$title, "(", sub_nodes$publication_year, ")", sep=""),
      url = sub_nodes$landing_page_url,
      label = "",
      x = sub_nodes$x,
      y = sub_nodes$y,
      fixed = TRUE
    )
    
    
    sub_edges$direction <- ifelse(sub_edges$from == center_id, "outgoing", "incoming")
    vis_edges <- data.frame(
      from = id_map[sub_edges$from],
      to = id_map[sub_edges$to],
      color = ifelse(sub_edges$direction == "outgoing", "#E69F00", "#56B4E9"),
      arrows = "from"
    )
    
    visNetwork(vis_nodes, vis_edges, height = "600px", width = "100%") %>%
      visGroups(groupname = "Reviewed", color = "#0072B2", shape = "dot") %>%
      visGroups(groupname = "Cited", color = "#E69F00", shape = "dot") %>%
      visGroups(groupname = "New citing ", color = "#56B4E9", shape = "dot") %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(useGroups = TRUE, position = "left", main = "Legend") %>%
      visEvents(
        selectNode = JS(
          "function(nodes) {
         if (nodes.nodes.length > 0) {
           var node = this.body.data.nodes.get(nodes.nodes[0]);
           if (node.title) {
             window.open(node.url, '_blank');
           }
         }
       }"
        )
      )
    
    
    
  })
  

  }

shinyApp(ui = ui, server = server)
