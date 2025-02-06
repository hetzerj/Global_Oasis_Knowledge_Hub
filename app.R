library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage("Global Oasis Knowledge Hub",
             tabPanel("Home",
                      div(class = "container",
                          div(class = "text-box",
                              h1("Global Oasis Knowledge Hub"),
                              h3("A Senckenberg initiative for leveraging knowledge supporting science and policy.")
                          )
                      )
             ),
             tabPanel("About",
                      div(class = "content", h2("About"), p("This page will contain information about the initiative."))
             ),
             tabPanel("Knowledge Hub",
                      div(class = "content", h2("Knowledge Hub"), p("A collection of valuable resources and insights."))
             ),
             tabPanel("Credits",
                      div(class = "content", h2("Credits"), p("Acknowledgments and contributors to this initiative."))
             )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
