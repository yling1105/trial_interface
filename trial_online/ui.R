
# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = HTML("Patient eligibility form"),
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

# Sidebar -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  width=300,
  sidebarMenu(
    id='sidebar',
    style = "position: relative; overflow: visible",
    
    dateInput('visit_date', label='Visit Date:'),
    #timeInput('visit_time', "Record Time:", seconds = FALSE),
    
    awesomeCheckboxGroup(
      inputId = "selectedTrials",
      label = "Trials", 
      choices = namelst,
      selected = NULL
    ),
    
    actionBttn(inputId = "genForm", label = "Intialize Criteria",
               style = "material-flat", color='primary'),
    
    menuItem('Eligibility form',tabName = 'form')
  )
)

# body --------------------------------------------------------------------


body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem('form', uiOutput("qe_pat"))
  )
)


# Output ------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "black")