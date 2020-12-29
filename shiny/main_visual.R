library(shiny)
ui <- navbarPage("LOL Analysis", fluid = TRUE, 
      tabPanel("Matches", headerPanel("Match analysis: predict the winning team")), 
      tabPanel("Teams", headerPanel("Team analysis: find the best team")))

server <- function(input, output){
  
}

shinyApp(ui=ui, server=server)