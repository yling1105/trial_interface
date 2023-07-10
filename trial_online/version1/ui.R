
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
    
    textInput('pat_id', label='Patient ID:'),
    
    dateInput('visit_date', label='Visit Date:'),
    #timeInput('visit_time', "Record Time:", seconds = FALSE),
    
    awesomeRadio(
      inputId = "selectedTrials",
      label = "Trials", 
      choices = namelst,
      selected = NULL
    ),
    
    actionButton(inputId = "genForm", label = "Intialize Criteria",
                 style = "color: #fff; background-color: #1E90FF; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 10px 18px; "),
    
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