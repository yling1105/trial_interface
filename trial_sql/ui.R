#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# header ------------------------------------------------------------------

header <- dashboardHeader(
  title = HTML("Trial Emulation"),
  dropdownMenu(
    type = 'message',
    messageItem(
      from = "Yaobin.ling@uth.tmc.edu",
      message =  "",
      icon = icon("envelope"),
      href = "mailto:Yaobin.ling@uth.tmc.edu"
    ),
    icon = icon('envelope')
  )
)


# sidebar ----------------------------------------------------------------

sidebar <- dashboardSidebar(
  width=300,
  sidebarMenu(
    id='sidebar',
    style = "position: relative; overflow: visible",
    
    fileInput(
      inputId = "getData",
      label="upload",
      accept = ".json",
      buttonLabel = "Browse"
    ),
    
    actionBttn(inputId = "generate", label = "Generate report",
               style = "material-flat", color='primary'),
    
    menuItem("Templates", tabName='temp_page', icon=icon("bars")),
    
    menuItem("Main Dashboard", tabName = 'dashboard', icon = icon("dashboard"),
             menuSubItem("Inclusion criteria", tabName = 'inclu_page', icon = icon("check")),
             menuSubItem("Exclusion criteria", tabName = "exclu_page", icon = icon("close")),
             menuSubItem("Added criteria", tabName = "added_page", icon=icon("plus"))
    ),
    
    tags$hr(),
    
    textInput(inputId = 'addCriNum', label = 'Number of criteria to add:'),
    actionBttn(inputId = "addCri", label = "Add criteria", style = "material-flat", color='primary'),
    
    tags$hr(),
    
    fluidRow(
      column(width=3, actionButton("save", "Save" , icon("save"),
                                   style = "color: #fff; background-color: #1E90FF; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 10px 18px; ")),
      column(width=3, actionButton('updateResults', 'Update', icon("refresh"),
                                   style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 5px 5px; ")),
      column(width=3, downloadButton('download_js', 'Download', class='dl')),
      tags$head(tags$style(".dl{background-color:coral;} .dl{color: black;} .dl{margin: 5px 5px 5px 5px;} .dl{padding: 5px 5px 5px 5px;}"))
    ),
    
    tags$hr(),
    
    menuItem("Processing Workflow", tabName = 'flow', icon=icon("rocket")),
    
    menuItem("Cohort Summary", tabName = 'summary', icon=icon("bar-chart"))
    
  )
)

# body --------------------------------------------------------------------


body <- dashboardBody(
  tags$head(
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    tags$style( HTML(" hr {border-top: 1px solid #000000;}") ),
    tags$style( HTML(" mark {background-color: tomato}"))
  ),
  
  tabItems(
    tabItem(
      tabName = 'Templates',
      conditionalPanel("output.getData == True && input.generate%2 == 1", uioutput("template_page")),      
    ),
    
    tabItem(
      tabName="inclu_page",
      conditionalPanel("output.getData == True && input.generate%2 == 1",uiOutput("inclu_ui")),
    ),
    
    tabItem(
      tabName = "exclu_page",
      conditionalPanel("output.getData == True && input.generate%2 == 1",uiOutput("exclu_ui"))
    ),
    
    tabItem(tabName = "added_page",
            conditionalPanel("input.addCri%2 == 1", uiOutput("add_template_select")),
            conditionalPanel("input.Add%2 == 1", uiOutput("add_cri"))
    ),
    
    tabItem(tabName = "flow",
            conditionalPanel("input$updateResults%2==1", DiagrammeROutput("cri_flow"))),
    
    tabItem(tabName = "summary")
    
  
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")