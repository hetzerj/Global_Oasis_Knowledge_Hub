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
library(future)
library(promises)
library(htmltools)

# definde multisession for processing big data in background job while website is already runnning 
plan(multisession)

source("R/load_data_from_zenodo.R")
source("R/generate_overview_plots.R")


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
           # tags$img(src = "Backround_pictures_and_logos/SNG_Logo_transperant.png", class = "logo",  style="z-index: 1000; margin-top: 100px;margin-right: 50px;display: flex; "),
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
  
  # Load Data in the background  ----------------------------------------------------------------------------------------------------------

  
  vals <- reactiveValues(
    nodes_df = NULL,
    edges_df = NULL,
    seedworks = NULL,
    global_nodes = NULL,
    status = "not_started"
  )
  
  session$onFlushed(function() {
    vals$status <- "loading"
    
    future({
      load_zenodo_data(CACHE_DIR)   # must return list(nodes_df=..., edges_df=..., seedworks=...)
    }) %...>% (function(res) {
      vals$nodes_df  <- res$nodes_df
      vals$edges_df  <- res$edges_df
      vals$seedworks <- res$seedworks
      vals$global_nodes <- res$global_nodes
      vals$status <- "ready"
    }) %...!% (function(e) {
      vals$status <- paste("error:", e$message)
    })
    
  }, once = TRUE)

  
  output$home_status <- renderUI({
    if (vals$status == "loading") div("Loading data in background...")
    else if (vals$status == "ready") div("Ready.")
    else if (startsWith(vals$status, "error:")) div(style = "color:red;", vals$status)
    else NULL
  })
  
  #reactive aliases
  get_ready <- function(x) { req(vals$status == "ready"); x }

nodes_df_r     <- reactive(get_ready(vals$nodes_df))
edges_df_r     <- reactive(get_ready(vals$edges_df))
seedworks_r    <- reactive(get_ready(vals$seedworks))
global_nodes_r <- reactive(get_ready(vals$global_nodes))
  
  
  # Start Statistics for Overview  ----------------------------------------------------------------------------------------------------------
  
  overview_started <- reactiveVal(FALSE)
  
  observe({
    req(vals$status == "ready")
    req(!overview_started())
    
  
  # --- OPEN ACCESS PIEPLOT -------------------------------------------------------
  data_open_access_path <- file.path("www/Overview_figures",
                                      paste0("open_access_pie_v", GOKH_version, ".rds"))
  generate_open_access_pie(nodes_df_r(), data_open_access_path, output)

  # --- DATA YEAR BARPLOT ---------------------------------------------------------
  data_year_barplot_path <- file.path("www/Overview_figures",
                                      paste0("data_year_barplot_v", GOKH_version, ".rds"))
  generate_data_year_barplot(nodes_df_r(), data_year_barplot_path, output)
  
  
  # --- DATA TYPE PIE -------------------------------------------------------------
  data_type_pie_path <- file.path("www/Overview_figures",
                                  paste0("data_type_pie_v", GOKH_version, ".rds"))
  generate_data_type_pie(nodes_df_r(), data_type_pie_path, output)
  
  
  # --- SOURCE TREEMAP -----------------------------------------------------------
  treemap_sources_path <- file.path("www/Overview_figures",
                                     paste0("treemap_sources_path_v", GOKH_version, ".rds"))
  
  generate_source_tree(nodes_df_r(), treemap_sources_path , output)
  
  
  overview_started(TRUE)
  })
 
  # End Statistics for Overview  -----------------------------------------------------------------------------------------------------------
  
  # Knowledge Hub Table search ----------------------------------------------
  
  #1. Prepare full data table 
  key_data_df <- reactive({
    
    nd <- nodes_df_r()
    
    data.frame(
      oa_ID = nd$oa_ID,  
      Authors = nd$Authors,
      Year = nd$Year,
      Title = nd$Title,
      Type = nd$Type,
      Open_access = nd$Open_access,
      Source = paste0('<a href="', nd$URL, '" target="_blank">', nd$Source, '</a>'),
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
    leaflet(global_nodes_r()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(
        lng = ~long,
        lat = ~lat,
        label = ~Title,
        layerId = ~oa_ID,
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
    
     ed <- edges_df_r()
     nd <- nodes_df_r()
     
    center_id <- subnet_trigger()

    
    sub_edges <- ed %>% filter(from == center_id | to == center_id)
    if (nrow(sub_edges) == 0) return(NULL)
    
    outgoing_ids <- sub_edges$to[sub_edges$from == center_id]
    incoming_ids <- sub_edges$from[sub_edges$to == center_id]
    connected_ids <- unique(c(sub_edges$from, sub_edges$to))
    sub_nodes <- nd %>% filter(oa_ID %in% connected_ids)
    
    sub_nodes$group <- ifelse(
      sub_nodes$oa_ID == center_id, "Reviewed",
      ifelse(sub_nodes$oa_ID %in% outgoing_ids, "Cited", "New citing ")
    )
    
    sub_nodes$internal_id <- 1:nrow(sub_nodes)
    id_map <- setNames(sub_nodes$internal_id, sub_nodes$oa_ID)
    
    # Fix X and Y positions
    
    sub_nodes$x <- rescale(sub_nodes$Year, to = c(-400, 400))
    
    sub_nodes$y <- ifelse(
      sub_nodes$oa_ID %in% outgoing_ids,
      seq(-200, 200, length.out = length(outgoing_ids)),
      ifelse(
        sub_nodes$oa_ID %in% incoming_ids,
        seq(-200, 200, length.out = length(incoming_ids)),
        0
      )
    )
    

    
    wrap_title <- function(txt, width = 60) {
      # inserts <br> every ~width characters at spaces
      paste(strwrap(txt, width = width), collapse = "<br>")
    }
    
    vis_nodes <- data.frame(
      id    = sub_nodes$internal_id,
      group = sub_nodes$group,
      title = sprintf(
        "<div style='max-width:320px; white-space:normal; overflow-wrap:anywhere; line-height:1.2;'>%s</div>",
        vapply(paste0(sub_nodes$Title, " (", sub_nodes$Year, ")"), wrap_title, character(1))
      ),
      url   = sub_nodes$URL,
      label = "",
      x     = sub_nodes$x,
      y     = sub_nodes$y,
      fixed = TRUE
    )
    

    sub_edges$direction <- ifelse(sub_edges$from == center_id, "outgoing", "incoming")
    vis_edges <- data.frame(
      from = id_map[sub_edges$from],
      to = id_map[sub_edges$to],
      color = ifelse(sub_edges$direction == "outgoing", "#56B4E9", "#E69F00"),
      arrows = "from"
    )
    
    minYear <- min(sub_nodes$Year, na.rm = TRUE)
    maxYear <- max(sub_nodes$Year, na.rm = TRUE)
    
    visNetwork(vis_nodes, vis_edges, height = "600px", width = "100%") %>%
      visNodes(size = 7, scaling = list(min = 0, max = 7)) %>%
      visGroups(groupname = "Reviewed", color = "#0072B2", shape = "dot") %>%
      visGroups(groupname = "Cited", color = "#56B4E9", shape = "dot") %>%
      visGroups(groupname = "New citing ", color = "#E69F00", shape = "dot") %>%
      visOptions(highlightNearest = FALSE) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(useGroups = TRUE, position = "right", main = "") %>%
      visEvents(
        selectNode = JS("
      function(nodes) {
        if (nodes.nodes.length > 0) {
          var node = this.body.data.nodes.get(nodes.nodes[0]);
          if (node && node.url) window.open(node.url, '_blank');
        }
      }
    "),
        afterDrawing = JS(sprintf("
  function(ctx) {
    var ns = this.body.data.nodes.get();
    if (!ns || ns.length === 0) return;

    // use visible node cloud in pixel space for left/right
    var dom = [];
    for (var i = 0; i < ns.length; i++) {
      var n = ns[i];
      if (n.x == null || n.y == null) continue;
      if (isNaN(n.x) || isNaN(n.y)) continue;
      dom.push(this.canvasToDOM({x: n.x, y: n.y}));
    }
    if (dom.length === 0) return;

    var left = dom[0].x, right = dom[0].x;
    for (var j = 1; j < dom.length; j++) {
      if (dom[j].x < left) left = dom[j].x;
      if (dom[j].x > right) right = dom[j].x;
    }

    // top axis position
    var y = -250;        // move down/up if needed
    left -= 450;
    right -= 400;
    if (right <= left) return;

    var minYear = %d;
    var maxYear = %d;

    var ticks = 8;

    ctx.save();
    ctx.font = '12px sans-serif';
    ctx.textAlign = 'center';
    ctx.strokeStyle = '#9E9E9E'; // axis line + ticks
    ctx.fillStyle   = '#9E9E9E'; // labels

    // axis line
    ctx.beginPath();
    ctx.moveTo(left, y);
    ctx.lineTo(right, y);
    ctx.stroke();

    // ticks + labels
    for (var k = 0; k < ticks; k++) {
      var t = (ticks === 1) ? 0 : k / (ticks - 1);
      var x = left + (right - left) * t;

      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.lineTo(x, y + 6);
      ctx.stroke();

      var yr = Math.round(minYear + (maxYear - minYear) * t);
      ctx.fillText(String(yr), x, y - 10);
    }

    ctx.restore();
  }
", minYear, maxYear))
      )
    
    
    
  })
  

  }

shinyApp(ui = ui, server = server)
