library(shiny)
library(openalexR)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage(
    "Global Oasis Knowledge Hub",
    tabPanel("Home", div(
      class = "container",
      
      tags$img(src = "SNG_Logo_transperant.png", class = "logo"),
      
      div(
        class = "text-box",
        h1("Global Oasis Knowledge Hub"),
        h3(
          "A Senckenberg initiative for leveraging knowledge supporting science and policy."
        )
      )
    )),
    tabPanel("About", div(
      class = "content",
      h2("About"),
      p("This page will contain information about the initiative.")
    )),
    tabPanel("Knowledge Hub", div(
      class = "content",
      h2("Knowledge Hub"),
      p("A collection of valuable resources and insights.")
    )), 
    tabPanel("Credits",
             
             # Team Section
             fluidRow(
               column(12, h2("Team", align = "center")),
               
               # Centered text with proper alignment
               fluidRow(
                 column(12, align = "center",
                        column(6, offset = 3, 
                               h3("The Global Oasis Knowledge Hub operates as a non-profit initiative by the Senckenberg research institute in Frankfurt with contributions from a global network of oasis experts.", align = "center")
                        )
                 )
               ),
               
               column(3, align = "center",
                      img(src = "Jessica_Hetzer.jpeg", class = "team-img"),
                      p("Jessica Hetzer", style="font-weight: bold"),
                      p("Lead Developer & Concept Creator")
               ),
               
               column(3, align = "center",
                      img(src = "Aidin_Niamir.jpeg", class = "team-img"),
                      p("Aidin Niamir", style="font-weight: bold"),
                      p("Coordinator & Concept Creator")
               ),
               
               column(3, align = "center",
                      img(src = "Rainer_Krug.jpeg", class = "team-img"),
                      p("Rainer M. Krug", style="font-weight: bold"),
                      p("Software Developer")
               ),
               
               column(3, align = "center",
                      img(src = "Empty_profile.png", class = "team-img"),
                      p("Mechthilde Falkenhahn", style="font-weight: bold"),
                      p("Literature Review")
               )
             ),
             
             hr(),
             
             # Advisory Board Section
             # Advisory Board Section
             fluidRow( style="background-color: rgb(160, 177, 203);",
               column(12, h3("Advisory Members", align = "center")),
               
               column(12, align = "center",
                      div(style="display: flex; justify-content: center; gap: 30px;",
                          column(3, align = "center",
                                 img(src = "Klement_Tockner.jpeg",  class = "advisor-img"),
                                 p("Klement Tockner", style="font-weight: bold")
                          ),
                          
                          column(3, align = "center",
                                 img(src = "Jonathan_Jeschke.jpeg", class = "advisor-img"),
                                 p("Jonathan Jeschke", style="font-weight: bold")
                          )
                      )
               )
             ),
             
             fluidRow( style="background-color: rgb(160, 177, 203);gap:10px",
               column(12, h3("Advisory Board", align = "center")),
               
               column(12, align = "center",
                      div(style="display: flex; justify-content: center; gap: 20px;",
                          column(2, align = "center",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 1", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 2", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 3", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 4", style="font-weight: bold")
                          ),
                          
                          column(2, align = "center",
                                 img(src = "Empty_profile.png", class = "advisor-img"),
                                 p("Advisor 5", style="font-weight: bold")
                          )
                      )
               )
             ),
             hr(),
             
             # Partners Section
             fluidRow(
               column(12, h2("Partners", align = "center")),
               
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
               )
             )
    )
    
  )
)


server <- function(input, output) {
  # 1. Define seed references and save metadata --------------------------------
  # In this step, a predefined set of seed references is used as the starting point.
  # The metadata of these references is retrieved from the open-source OpenAlex database.
  
  # Set update flags to FALSE to prevent unnecessary reloading of data
  seed_knowledge_update <- FALSE
  expanded_knowledge_update <- FALSE
  
  # Define the file path where the metadata of seed references should be stored
  fn <- file.path("data", "seed_knowledge_works.rds")
  
  # Check if the metadata file already exists and no updates are required
  # If the file exists and updates are not needed, load the existing file instead of fetching new data
  if (file.exists(fn) & (!seed_knowledge_update)) {
    seed_works <- readRDS(fn)
  }
  else {
    # Read predefined literature IDs from a CSV file
    seed_df <- read.csv(
      file.path(".", "input", "oasis_seed_references.csv"),
      header = TRUE,
      sep = "\t"
    )
    
    # Retrieve metadata for the seed references from OpenAlex
    seed_works <- oa_fetch(entity = "works",
                           id = seed_df$oa_id,
                           verbose = FALSE)
    
    # Save the retrieved metadata for future use
    saveRDS(seed_works, fn)
  }
  
  # 2. Expand references by exploring citations from and to seed references -----------------
  # This step performs a snowball search by collecting all citations both from and to the seed references.
  
  # Define the file path where the expanded reference metadata should be stored
  fn <- file.path("data", "expanded_knowledge_works.rds")
  
  # If the expanded reference file exists, load the saved data; otherwise, perform the expansion search
  if (file.exists(fn) &
      (!seed_knowledge_update) & (!expanded_knowledge_update)) {
    expanded_works <- readRDS(fn)
  }
  else {
    # Perform a snowball search to retrieve citations related to the seed references
    expanded_works <- oa_snowball(identifier = seed_works$id, verbose = FALSE)
    # Add a new field `is_seed` indicating whether a reference was one of the original seed references
    expanded_works$nodes$is_seed <- expanded_works$nodes$id %in% openalexR:::shorten_oaid(seed_works$id)
    
    # Add short names for authors to enhance readability
    expanded_works$nodes$authors_short <- sapply(expanded_works$nodes$author, function(authors) {
      if (is.data.frame(authors)) {
        # If there are multiple authors, use the first author's name followed by 'et al.'
        paste(authors$au_display_name[1], "et al.")
      } else if (is.character(authors)) {
        # If there is only one author, use their name
        authors
      } else {
        # If author information is missing, return NA
        NA_character_
      }
    })
    # Save the expanded metadata for future reference
    saveRDS(expanded_works, fn)
  }
}

shinyApp(ui = ui, server = server)
