
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
    timeInput('visit_time', "Record Time:", seconds = FALSE),
    
    selectizeInput()
  )
)

# body --------------------------------------------------------------------


body <- dashboardBody(
  useShinyjs()
)


# Output ------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body, skin = "black")