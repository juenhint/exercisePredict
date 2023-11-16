library(shiny)
library(tools, readxl)
source("predict script.R")

ui <- fluidPage(
  
  # exercisePredict ----
  titlePanel("Calculate submax test parameters"),
  fluidPage(
    fluidRow(
      column(width=4,fileInput("filinp", h3("File input (txt, csv, xls or xlsx"))),
      #column(width=2,actionButton(inputId = "loadexample",label = "Example data",)),
      column(width=3,radioButtons("sep", "Separator (only adjust if there's a problem with file read)",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t")),
      column(width=3,radioButtons("quote", "Quote (only adjust if there's a problem with file read)",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = "")),
    ),
    fluidRow(
      h4("Base data:"),
      tableOutput(outputId = "metadata"),
    ),
    fluidRow(
      h4("Calculated values:"),
      column(width=4,numericInput(inputId = "manualhr",
                   label="Manual override maxHR",
                   value="",
      )),
      column(width=4,numericInput(inputId = "manualweight",
                          label="Manual override weight",
                          value="",
      )),
      tableOutput(outputId = "fitness_pm"),
    ),
    fluidRow(
      h4("Regression plot:"),
      plotOutput(outputId = "lm_plot")
    ),
    fluidRow(
      #textOutput(outputId = "annotation")
      p(strong("maxHR:"), "Age-dependent maximum HR (220-age) (min^-1).\n"), 
      p(strong("Powermax:"), "Extrapolated maximum power output (W).\n"),
      p(strong("VO2max:"), "Estimated maximum oxygen uptake (ml min^-1 kg^-1).\n"),
      p(strong("Powersubmax0:"),"Approximate 60 % of maximum load (W).\n"),
      p(strong("HRsubmax:"),"Heart rate at 60 % of max load (min^-1).\n"),
      p(strong("VO2submax:"),"Oxygen uptake at 60 % of max load (L min^-1).\n"),
      p(strong("RERsubmax:"),"Respiratory exhange ratio at 60 % of max load.\n"),
      p(strong("GEsubmax:"),"Gross efficiency at 60 % of max load (%).\n"),
      div("Copyright Jukka Hintikka, University of Jyväskylä. MIT License. Contact juenhint@jyu.fi", style="color: grey; font-size: 10pt"),
      div("v1.2", style="color: grey; font-size: 10pt")
    )
  )
)

server <- function(input, output) {
  r <- reactive({
    runScript(input)
  })
  
  #output$annotation <- renderText({
  #  req(input$filinp)
  #  submax = 60
  #  HTML(
  #  paste0("<strong>maxHR:</strong> Age-dependent maximum HR (220-age) (min^-1).<br> 
  #  <strong>Powermax:</strong> Extrapolated maximum power output (W).<br>
  #  <strong>VO2max:</strong> Estimated maximum oxygen uptake (ml min^-1 kg^-1).<br>",
  #  "<strong>Powersubmax0:</strong> Approximate ",submax," % of maximum load (W).<br> 
  #  <strong>HRsubmax:</strong> Heart rate at ",submax," % of max load (min^-1).<br>
  #  <strong>VO2submax:</strong> Oxygen uptake at ",submax," % of max load (L min^-1).<br>
  #  <strong>RERsubmax:</strong> Respiratory exhange ratio at ",submax," % of max load.<br>
  #  <strong>GEsubmax:</strong> Gross efficiency at ",submax," % of max load (%)."))
  #})
  
  output$metadata <- renderTable({
    req(input$filinp)
    objs <- r()
    metadata <- objs$metadata
    as.data.frame(metadata)
  })
  
  output$lm_plot <- renderPlot({
    req(input$filinp)
    objs <- r()
    objs$plot
  })
  
  output$fitness_pm <- renderTable({
    req(input$filinp)
    objs <- r()
    objs$results
  })

}

shinyApp(ui = ui, server = server)

