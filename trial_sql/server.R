#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

server <- function(input, output, session) {
  
  output$getData <- reactive({
    return(!is.null(input$getData))
  })
  outputOptions(output, "getData", suspendWhenHidden = FALSE)
  
  sample <- reactive({
    file1 <- input$getData
    if (is.null(file1)) { 
      return() 
    } 
    
    #dataset <- load_json_py(file1$datapath)
    #dataset <- map_clinical_trial_dump(dataset)
    dataset <- fromJSON(file=file1$datapath)
    sample <- list()
    sample[['inclusion']] <- dataset$inclusion
    sample[['exclusion']] <- dataset$exclusion
    sample
  })
  
  # Template intro popup ----------------------------------------------------
  
  onclick('demo_intro', showModal(modalDialog(
    title = "Demographic template",
    tags$div(
      tags$ul(
        tags$li("Age"),
        tags$li("Gender"),
        tags$li("Race"),
        tags$li("Ethnic Group")
      )
    )  
  )))
  
  onclick('diag_intro', showModal(modalDialog(
    title = "Diagnosis template",
    tags$div(
      tags$ul(
        tags$li("Diagnosis Code is"),
        tags$li("Diagnosis Code starts with"),
        tags$li("Diagnosis Description contains"),
        tags$li("Time Period within"),
        tags$li("Whether search by diagnosis group"),
        tags$li("Encounter based")
      )
    )  
  )))
  
  onclick('drug_intro', showModal(modalDialog(
    title = "Prescription template",
    tags$div(
      tags$ul(
        tags$li("Drug description contains"),
        tags$li("Time Period within"),
        tags$li("Encounter based")
      )
    )  
  )))
  
  onclick('event_intro', showModal(modalDialog(
    title = "Event template",
    tags$div(
      tags$ul(
        tags$li("Event name contains"),
        tags$li("Value range: (>, <, >=, <=)"),
        tags$li("Time Period within"),
        tags$li("Encounter based")
      )
    )  
  )))
  
  onclick('lab_intro', showModal(modalDialog(
    title = "Lab template",
    tags$div(
      tags$ul(
        tags$li("Lab name contains"),
        tags$li("LOINC code is:"),
        tags$li("Value range: (>, <, >=, <=)"),
        tags$li("Time Period within"),
        tags$li("Encounter based")
      )
    )  
  )))
  
  onclick('order_intro', showModal(modalDialog(
    title = "Order template",
    tags$div(
      tags$ul(
        tags$li("Procedure name contains"),
        tags$li("Time Period within"),
        tags$li("Encounter based")
      )
    )  
  )))
  
  
  # template generate page --------------------------------------------------
  
  template_page <- eventReactive(input$temp_generate, {
    inclu_lst <- sample()[['inclusion']]
    exclu_lst <- sample()[['exclusion']]
    ni <- length(inclu_lst)
    ne <- length(exclu_lst)
    n <- ni + ne
    
    template_page <- tagList()
    
    for (i in c(1 : ni)){
      idx_eve <- 1
      idx_lab <- 1
      cri <- inclu_lst[[i]]
      temptemp <-  cri[['mapped_templates']]
      critext <- cri[['display_text']]
      temp_lst <- c()
      if (length(temptemp) > 0){
        for (j in c(1:length(temptemp))){
          tempj <- temptemp[[j]][['template']]
          if (tempj == 'Condition by Diagnosis Code'){
            tempj <- 'Diagnosis'
          } else if (tempj == 'Event') {
            tempj <- paste('Event', idx_eve)
            idx_eve <- idx_eve + 1
          } else if (tempj == 'Lab') {
            tempj <- paste('Lab', idx_lab)
            idx_lab <- idx_lab + 1
          }
          temp_lst <- append(temp_lst, tempj)
        }
      }
      
      #print(temp_lst)
      selection <- multiInput(inputId = paste0('temp_page_',i), label='Pick templates for the criteria:',
                              choices = c('Demographic', 'Diagnosis', 'Prescription', 'Event 1', 'Event 2', 'Event 3', 'Lab 1', 'Lab 2', 'Lab 3', 'Order', 'Encounter'),
                              selected = temp_lst
      )
      template_page <- tagAppendChild(template_page, h1(paste('Inclusion Criteria', i)))
      template_page <- tagAppendChild(template_page, fluidRow(
        column(width = 6, selection),
        column(width = 6, HTML(critext))
      ))
      template_page <- tagAppendChild(template_page, tags$hr())
    }
    #print("Exclusion")
    for (i in c(1 : ne)){
      idx_eve <- 1
      idx_lab <- 1
      idx <-  ni + i
      cri <- exclu_lst[[i]]
      temptemp <-  cri[['mapped_templates']]
      critext <- cri[['display_text']]
      #print(critext)
      temp_lst <- c()
      if (length(temptemp) > 0){
        for (j in c(1:length(temptemp))){
          tempj <- temptemp[[j]][['template']]
          if (tempj == 'Condition by Diagnosis Code'){
            tempj <- 'Diagnosis'
          } else if (tempj == 'Event') {
            tempj <- paste('Event',idx_eve)
            idx_eve <- idx_eve + 1
          } else if (tempj == 'Lab') {
            tempj <- paste('Lab', idx_lab)
            idx_lab <- idx_lab + 1
          }
          
          temp_lst <- append(temp_lst, tempj)
        }
      }
      selection <- multiInput(inputId = paste0('temp_page_',idx), label='Pick templates for the criteria:',
                              choices = c('Demographic', 'Diagnosis', 'Prescription', 'Event 1', 'Event 2', 'Event 3', 'Lab 1', 'Lab 2', 'Lab 3', 'Order', 'Encounter'),
                              selected = temp_lst
      )
      template_page <- tagAppendChild(template_page, h1(paste('Exclusion Criteria', i)))
      template_page <- tagAppendChild(template_page, fluidRow(
        column(width = 6, selection),
        column(width = 6, HTML(critext))
      ))
      template_page <- tagAppendChild(template_page, tags$hr())
      
    }
    
    template_page <- tagAppendChild(template_page, actionButton(inputId = "generate",label = "Generate criteria",
                                                                style = "color: #FFF8DC; background-color: #2F4F4F; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "))
    
    template_page
  })
  
  output$template_page <- renderUI(template_page())
  
  # Inclusion page ----------------------------------------------------------
  inclu_sample <- eventReactive(input$generate, {
    inclu_cri <- list()
    inclu_lst <- sample()[['inclusion']]
    inclu_sample <- tagList()
    
    for (i in c(1:length(inclu_lst))){
      idx_temp0 <- paste("inclu", i)
      inclu_temp <- list()
      temp <- inclu_lst[[i]]$mapped_templates
      temp_dict <- initial(temp)
      inclu_text <- inclu_lst[[i]][['display_text']]
      logic_temp <- inclu_lst[[i]][['internal_logic']]
      print('inclu')
      print(i)
      print(logic_temp)
      interval <- inclu_lst[[i]][['interval']]
      cri_id <- paste0('temp_page_',i)
      temp_lst <- input[[cri_id]]
      if (length(temp_lst) > 0){
        for (j in c(1:length(temp_lst))){
          kk <- temp_lst[[j]]
          idx_temp1 <- paste(idx_temp0, j)
          inclu_temp[[j]] <- template_map(kk, temp_dict, idx_temp1)
        }
      }
      
      logic_bttn <- pickerInput(
        inputId = paste('inclu_logic', idx_temp0),
        label = "Internal logic", 
        choices = c('AND', 'OR', 'Template 2 WITHIN Template 1', 'Template 1 WITHIN Template 2'),
        selected = logic_temp
      )
      
      time_interval <- textInput(inputId = paste('inclu_interval', idx_temp0), label="Time interval before or after:", value = interval)
      
      inclu_sample <- tagAppendChild(inclu_sample,h1(paste('Inclusion Criteria ', i)))
      inclu_sample <- tagAppendChild(inclu_sample, logic_bttn)
      inclu_sample <- tagAppendChild(inclu_sample, time_interval)
      inclu_sample <- tagAppendChild(inclu_sample, fluidRow(
        column(width = 6, inclu_temp),
        column(width = 6, HTML(inclu_text))
      ))
      inclu_sample <- tagAppendChild(inclu_sample, tags$hr())
    }
    inclu_sample
  })
  output$inclu_ui <- renderUI(inclu_sample())
  
  # Exclusion page -----------------------------------------------------
  exclu_sample <- eventReactive(input$generate, {
    exclu_cri <- list()
    exclu_lst <- sample()[['exclusion']]
    inclu_lst <- sample()[['inclusion']]
    exclu_sample <- tagList()
    ni <- length(inclu_lst)
    
    for (i in c(1:length(exclu_lst))){
      idx_temp0 <- paste('exclu', i)
      exclu_temp <- list()
      temp <- exclu_lst[[i]]$mapped_templates
      temp_dict <- initial(temp)
      exclu_text <- exclu_lst[[i]][['display_text']]
      logic_temp <- exclu_lst[[i]][['internal_logic']]
      interval <- exclu_lst[[i]][['interval']]
      e_idx <- i + ni
      cri_id <- paste0('temp_page_',e_idx)
      temp_lst <- input[[cri_id]]
      print('exclu')
      print(i)
      print(logic_temp)
      
      if (length(temp_lst) != 0){
        for (j in c(1:length(temp_lst))){
          kk <- temp_lst[[j]]
          idx_temp1 <- paste(idx_temp0, j)
          exclu_temp[[j]] <- template_map(kk, temp_dict, idx_temp1)
        }
      }
      
      logic_bttn <- pickerInput(
        inputId = paste('exclu_logic', idx_temp0),
        label = "Internal logic", 
        choices = c('AND', 'OR', 'Template 2 WITHIN Template 1', 'Template 1 WITHIN Template 2'),
        selected = logic_temp
      )
      
      interval_text <- textInput(inputId = paste('exclu_interval', idx_temp0), label = 'Time interval before or after:', value = interval)
      
      exclu_sample <- tagAppendChild(exclu_sample,h1(paste('Exclusion Criteria ', i)))
      exclu_sample <- tagAppendChild(exclu_sample, logic_bttn)
      exclu_sample <- tagAppendChild(exclu_sample, interval_text)
      exclu_sample <- tagAppendChild(exclu_sample, fluidRow(
        column(width = 6, exclu_temp),
        column(width = 6, HTML(exclu_text))
      ))
      exclu_sample <- tagAppendChild(exclu_sample, tags$hr())
    }
    exclu_sample
  })
  output$exclu_ui <- renderUI(exclu_sample())
  
  
  # Save to a new file ------------------------------------------------------
  
  new_data <- eventReactive(input$save, {
    new_data <- sample()
    
    inclu <- new_data[['inclusion']]
    exclu <- new_data[['exclusion']]
    
    for (i in c(1:length(inclu))){
      # save inclusion part -----------------------------------------------------
      idx_temp0 <- paste('inclu', i)
      logic_temp <- input[[paste('inclu_logic', idx_temp0)]]
      new_data[['inclusion']][[i]][['internal_logic']] <- logic_temp
      new_data[['inclusion']][[i]][['interval']] <- input[[paste('inclu_interval', idx_temp0)]]
      temp <- inclu[[i]][['mapped_templates']]
      
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          template_id <- temp[[j]][['template']]
          idx_temp <- paste(idx_temp0, j)
          
          ## Condition --------
          if (template_id == 'Condition by Diagnosis Code'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code starts with']] <- input[[paste("diag_like_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Type']] <- input[[paste("diag_type_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
            
            if (length(input[[paste("diag_grp_", idx_temp)]]) ==0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
            } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
            } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
            } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
            }
            
            if (length(input[[paste("diag_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("diag_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("diag_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("diag_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Demographics --------  
          } else if (template_id=='Demographic'){
            gdr <- input[[paste("gender_is_", idx_temp)]]
            race <- input[[paste("race_is_", idx_temp)]]
            eth <- input[[paste("ethic_grp_", idx_temp)]]
            age_from <- input[[paste("age_from_", idx_temp)]]
            age_to <- input[[paste("age_to_", idx_temp)]]
            if (length(gdr) == 0){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- ''
            } else if (gdr == 'None'){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- ''
            } else {
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- gdr
            }
            
            if (length(race) == 0){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- ''
            } else if (gdr == 'None'){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- ''
            } else {
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- race
            }
            
            if (length(eth) == 0){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
            } else if (eth == 'None'){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
            } else {
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- eth
            }
            
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Age from ( include )']] <- age_from
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Age to ( include )']] <- age_to
            
            ## Prescription -------- 
          } else if (template_id == 'Prescription'){
            drug_desc <- input[[paste('drug_desc_', idx_temp)]]
            drug_time <- input[[paste('drug_time_', idx_temp)]]
            encounter <- input[[paste('drug_enc_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- drug_time
            if (length(input[[paste("drug_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("drug_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("drug_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("drug_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Event --------  
          } else if (template_id == 'Event'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('event_val_fromi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('event_val_fromn_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('event_val_toi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('event_val_ton_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('event_time_', idx_temp)]]
            if (length(input[[paste("event_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("event_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("event_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("event_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Lab -------- 
          } else if (template_id == 'Lab'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('lab_val_fromi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('lab_val_fromn_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('lab_val_toi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('lab_val_ton_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
            
            if (length(input[[paste("lab_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("lab_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("lab_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("lab_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Order -------- 
          } else if (template_id == 'Order'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_time_', idx_temp)]]
            if (length(input[[paste("order_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("order_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
          } else if (template_id == 'Encounter'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Admit Type']] <- input[[paste('admit_type_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Encounter Type']] <- input[[paste('encounter_type_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Discharge Disposition']] <- input[[paste('disch_', idx_temp)]]
          }
        }
        
        
        
      }
    }
    
    ### save exclusion part -----------------------------------------------------
    for (i in c(1:length(exclu))){
      idx_temp0 <- paste("exclu", i)
      logic_temp <- input[[paste('exclu_logic', idx_temp0)]]
      new_data[['exclusion']][[i]][['internal_logic']] <- logic_temp
      new_data[['exclusion']][[i]][['interval']] <- input[[paste('exclu_interval', idx_temp0)]]
      temp <- exclu[[i]][['mapped_templates']]
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          template_id <- temp[[j]][['template']]
          idx_temp <- paste(idx_temp0, j)
          
          ### Condition --------
          if (template_id == 'Condition by Diagnosis Code'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code starts with']] <- input[[paste("diag_like_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Type']] <- input[[paste("diag_type_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
            
            if (length(input[[paste("diag_grp_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
            } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis"]] <- ''
            } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
            } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
            }
            
            if (length(input[[paste("diag_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("diag_base_", idx_temp)]] == 'NULL'){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("diag_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("diag_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Demographics --------  
          } else if (template_id=='Demographic'){
            gdr <- input[[paste("gender_is_", idx_temp)]]
            race <- input[[paste("race_is_", idx_temp)]]
            eth <- input[[paste("ethic_grp_", idx_temp)]]
            age_from <- input[[paste("age_from_", idx_temp)]]
            age_to <- input[[paste("age_to_", idx_temp)]]
            if (length(gdr) == 0){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- ''
            } else if (gdr == 'None'){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- ''
            } else {
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Gender is']] <- gdr
            }
            
            if (length(race) == 0){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- ''
            } else if (gdr == 'None'){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- ''
            } else {
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Race is']] <- race
            }
            
            if (length(eth) == 0){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
            } else if (eth == 'None'){
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
            } else {
              new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- eth
            }
            
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Age from ( include )']] <- age_from
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Age to ( include )']] <- age_to
            
            ### Prescription -------- 
          } else if (template_id == 'Prescription'){
            drug_desc <- input[[paste('drug_desc_', idx_temp)]]
            drug_time <- input[[paste('drug_time_', idx_temp)]]
            encounter <- input[[paste('drug_enc_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- drug_time
            
            if (length(input[[paste("drug_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("drug_base_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("drug_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("drug_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ### Event --------  
          } else if (template_id == 'Event'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('event_val_fromi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('event_val_fromn_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('event_val_toi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('event_val_ton_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('event_time_', idx_temp)]]
            
            if (length(input[[paste("event_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("event_base_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("event_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("event_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            
          } else if (template_id == 'Lab'){
            ### Lab -------- 
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('lab_val_fromi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('lab_val_fromn_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('lab_val_toi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('lab_val_ton_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
            if (length(input[[paste("lab_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("lab_base_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("lab_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("lab_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ### Order -------- 
          } else if (template_id == 'Order'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_time_', idx_temp)]]
            if (length(input[[paste("order_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("order_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
          } else if (template_id == 'Encounter'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Admit Type']] <- input[[paste('admit_type_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Encounter Type']] <- input[[paste('encounter_type_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Discharge Disposition']] <- input[[paste('disch_', idx_temp)]]
          }
        }
      }
    }
    
    # Save added part -----------------------------------------------------
    
    if (input$addCriNum != '' & input$addCriNum != '0'){
      n_added <- strtoi(input$addCriNum)
      #print(n_added)
      n_inclu <- length(sample()[['inclusion']])
      n_exclu <- length(sample()[['exclusion']])
      
      
      for (i in c(1:n_added)){
        temp_lst <- input[[paste("criTemp", i)]]
        clu <- input[[paste("clu", i)]]
        print(clu)
        
        # Added inclusion ---------------
        if (clu == 'Inclusion'){
          n_inclu <- n_inclu + 1
          new_data[['inclusion']][[n_inclu]] <- list()
          new_data[['inclusion']][[n_inclu]][["mapped_templates"]] <- list()
          new_data[['inclusion']][[n_inclu]][['internal_logic']] <- input[[paste('add_logic', i)]]
          new_data[['inclusion']][[n_inclu]][['interval']] <- input[[paste('add_interval', i)]]
          new_data[['inclusion']][[n_inclu]][['text']] <- input[[paste('add_text',i)]]
          for (j in c(1:length(temp_lst))){
            temp <- temp_lst[[j]]
            new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]] <- list()
            idx_temp <- paste("add_", clu, i, j)
            if (temp == 'Diagnosis'){
              
              new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- 'Condition by Diagnosis Code'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Code starts with']] <- input[[paste("diag_like_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Type']] <- input[[paste("diag_type_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
              
              if (length(input[[paste("diag_grp_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
              } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
              }
              
              if (length(input[[paste("drug_base_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("drug_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Demographic'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- 'Demographic'
              gdr <- input[[paste("gender_is_", idx_temp)]]
              race <- input[[paste("race_is_", idx_temp)]]
              eth <- input[[paste("ethic_grp_", idx_temp)]]
              age_from <- input[[paste("age_from_", idx_temp)]]
              age_to <- input[[paste("age_to_", idx_temp)]]
              if (length(gdr) == 0){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Gender is']] <- ''
              } else if (gdr == 'None'){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Gender is']] <- ''
              } else {
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Gender is']] <- gdr
              }
              
              if (length(race) == 0){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Race is']] <- ''
              } else if (gdr == 'None'){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Race is']] <- ''
              } else {
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Race is']] <- race
              }
              
              if (length(eth) == 0){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][[' is']] <- ''
              } else if (eth == 'None'){
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
              } else {
                new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- eth
              }
              
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Age from ( include )']] <- age_from
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Age to ( include )']] <- age_to
            } else if (temp == 'Prescription') {
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- 'Prescription'
              drug_desc <- input[[paste('drug_desc_', idx_temp)]]
              drug_time <- input[[paste('drug_time_', idx_temp)]]
              encounter <- input[[paste('drug_enc_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
              
              if (length(input[[paste("drug_base_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("drug_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp =='Event 1'|temp == 'Event 2'|temp == 'Event 3'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Event'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('event_val_fromi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('event_val_fromn_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('event_val_toi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('event_val_ton_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('event_time_', idx_temp)]]
              
              if (length(input[[paste("event_base_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("event_base_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("event_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("event_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Lab 1' | temp == 'Lab 2' | temp == 'Lab 3'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Lab'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('lab_val_fromi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('lab_val_fromn_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('lab_val_toi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('lab_val_ton_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
              if (length(input[[paste("lab_base_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("lab_base_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("lab_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("lab_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Order'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- "Order"
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_name_', idx_temp)]]
              if (length(input[[paste("order_base_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("order_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Encounter'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Admit Type']] <- input[[paste('admit_type_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Encounter Type']] <- input[[paste('encounter_type_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Discharge Disposition']] <- input[[paste('disch_', idx_temp)]]
            }
          }
          
          
          
        } 
        ### Added exclusion ----------
        else {
          n_exclu <- n_exclu + 1
          new_data[['exclusion']][[n_exclu]] <- list()
          new_data[['exclusion']][[n_exclu]][["mapped_templates"]] <- list()
          new_data[['exclusion']][[n_exclu]][['internal_logic']] <- input[[paste('add_logic', i)]]
          new_data[['exclusion']][[n_exclu]][['interval']] <- input[[paste('add_interval', i)]]
          new_data[['exclusion']][[n_exclu]][['text']] <- input[[paste('add_text',i)]]
          
          for (j in c(1:length(temp_lst))){
            temp <- temp_lst[[j]]
            new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]] <- list()
            new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']]
            idx_temp <- paste("add_", clu, i, j)
            print(idx_temp)
            if (temp == 'Diagnosis'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Condition by Diagnosis Code'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Code starts with']] <- input[[paste("diag_like_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Type']] <- input[[paste("diag_type_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
              
              
              if (length(input[[paste("diag_grp_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
              } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
              }
              
              print(length(input[[paste("diag_base_", idx_temp)]]) == 0)
              if (length(input[[paste("diag_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("diag_base_", idx_temp)]] == 'NULL'){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("diag_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("diag_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Demographic'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Demographic'
              gdr <- input[[paste("gender_is_", idx_temp)]]
              race <- input[[paste("race_is_", idx_temp)]]
              eth <- input[[paste("ethic_grp_", idx_temp)]]
              age_from <- input[[paste("age_from_", idx_temp)]]
              age_to <- input[[paste("age_to_", idx_temp)]]
              if (length(gdr) == 0){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Gender is']] <- ''
              } else if (gdr == 'None'){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Gender is']] <- ''
              } else {
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Gender is']] <- gdr
              }
              
              if (length(race) == 0){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Race is']] <- ''
              } else if (gdr == 'None'){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Race is']] <- ''
              } else {
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Race is']] <- race
              }
              
              if (length(eth) == 0){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
              } else if (eth == 'None'){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
              } else {
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- eth
              }
              
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Age from ( include )']] <- age_from
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Age to ( include )']] <- age_to
            } else if (temp == 'Prescription') {
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Prescription'
              drug_desc <- input[[paste('drug_desc_', idx_temp)]]
              drug_time <- input[[paste('drug_time_', idx_temp)]]
              encounter <- input[[paste('drug_enc_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
              
              if (length(input[[paste("drug_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("drug_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("drug_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp =='Event 1'|temp == 'Event 2' | temp == 'Event 3'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Event'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('event_val_fromi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('event_val_fromn_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('event_val_toi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('event_val_ton_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('event_time_', idx_temp)]]
              
              if (length(input[[paste("event_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("event_base_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("event_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("event_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Lab 1' | temp == 'Lab 2' | temp == 'Lab 3'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Lab'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from ( include )']] <- input[[paste('lab_val_fromi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from ( not include )']] <- input[[paste('lab_val_fromn_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to ( include )']] <- input[[paste('lab_val_toi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to ( not include )']] <- input[[paste('lab_val_ton_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
              if (length(input[[paste("lab_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("lab_base_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("lab_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("lab_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Order'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['template']] <- 'Order'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_name_', idx_temp)]]
              if (length(input[[paste("order_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("order_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Encounter'){
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Admit Type']] <- input[[paste('admit_type_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Encounter Type']] <- input[[paste('encounter_type_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Discharge Disposition']] <- input[[paste('disch_', idx_temp)]]
            }
          }
          
        }
      }
    }
    #print(new_data)
    print(str(reactiveValuesToList(input)) )
    new_data
  })
  
  # Add criteria ------------------------------------------------------------
  ## Template selection 
  
  add_template_select <- eventReactive(input$addCri, {
    addnumber <- strtoi(input$addCriNum)
    add_template_select <- tagList()
    
    for (i in c(1:addnumber)){
      add_template_select <- tagAppendChild(add_template_select, h3(paste("Criteria ", i)))
      add_template_select <- tagAppendChild(add_template_select, pickerInput(inputId = paste("clu", i), choices = c("Inclusion", "Exclusion")))
      add_template_select <- tagAppendChild(add_template_select,       
                                            multiInput(inputId = paste('criTemp', i), 
                                                       label = "Pick the templates",
                                                       choices = c('Demographic', 'Diagnosis', 'Prescription', 'Event 1', 'Event 2', 'Event 3', 'Lab 1', 'Lab 2', 'Lab 3', 'Order', 'Encounter')))
    }
    
    add_template_select <- tagAppendChild(add_template_select, actionButton(inputId = "Add",label = "Generate criteria",
                                                                            style = "color: #FFF8DC; background-color: #2F4F4F; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "))
    add_template_select
  })
  
  output$add_template_select <- renderUI(add_template_select())
  
  # Update idx -------
  add_cri <- eventReactive(input$Add,{
    add_cri <- tagList()
    add_cri <- tagAppendChild(add_cri, hr())
    add_cri_temp <- list()
    addnumber <- strtoi(input$addCriNum)
    
    for (i in c(1 : addnumber)){
      clu <- input[[paste("clu", i)]]
      if (clu == 'Inclusion'){
        add_cri <- tagAppendChild(add_cri, h3("Inclusion","Criteria", i))
      } else {
        add_cri <- tagAppendChild(add_cri, h3("Exclusion", "Criteria", i))
      }
      add_cri <- tagAppendChild(add_cri, textInput(inputId = paste("add_text", i),label = "Criteria description", value = ''))
      temp_lst <- input[[paste("criTemp", i)]]
      add_cri <- tagAppendChild(add_cri, pickerInput(inputId = paste("add_logic", i), choices = c("AND", "OR", "Template 2 WITHIN Template 1", "Template 1 WITHIN Template 2")))
      add_cri <- tagAppendChild(add_cri, textInput(inputId = paste('add_interval', i), label='Time interval before or after:'))
      for (j in c(1:length(temp_lst))){
        idx_temp <- paste("add_", clu, i, j)
        print(idx_temp)
        temptemp <- add_template(temp_lst[j], idx_temp)
        add_cri_temp[[j]] <- temptemp
      }
      add_cri <- tagAppendChild(add_cri, add_cri_temp)
    }
    add_cri
  })
  
  output$add_cri <- renderUI(add_cri())
  
  # Download json file ------------------------------------------------------
  
  output$ddjson <- downloadHandler(
    filename <- function() {
      paste("data_", Sys.Date(), ".json")
    },
    content = function(file) {
      write(toJSON(new_data()), file)
    },
    
    contentType =  "application/json"
  )
  
  
  # Interactive with database -----------------------------------------------
  
  js_res <- eventReactive(input$updateResults,{
    updates_js <- toJSON(new_data())
    print(updates_js)
    jsout_update_num <- refresh_patient_numbers(updates_js)
    js_res <- fromJSON(jsout_update_num)
    js_res
  })
  
  output$cri_flow <- renderDiagrammeR({
    res <- js_res()
    inclu <- res[['inclusion']]
    exclu <- res[['exclusion']]
    n_inclu <- length(inclu)
    n_exclu <- length(exclu)
    n <- n_inclu + n_exclu
    print(n)
    print(n_exclu)
    accumulate <- list()
    current <- list()
    diagram <- "graph TB\n subgraph Inclusion criteria\n"
    if (n_exclu > 1){
      for (i in c(1 : (n-1))){
        if (i <= n_inclu){
          cri_text <- inclu[[i]][['text']]
          cri_text <- gsub("\\(|\\)", "", cri_text)
          if (i == n_inclu){
            diagram <- paste0(diagram, 'end\n')
            diagram <- paste0(diagram, 'subgraph Exclusion criteria\n')
          }
          diagram <- paste0(diagram, "id",i,"(Inclusion Criteria ", i, ": ", br(), cri_text, br(), "Patients meet this criteria:", inclu[[i]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", inclu[[i]]$accumulated_patients,")-->",
                            "id", i+1, "\n")
        } else if (i != n-1) {
          j <- i - n_inclu
          cri_text <- exclu[[j]][['text']]
          cri_text <- gsub("\\(|\\)", "", cri_text)
          diagram <- paste0(diagram, "id", i,"(Exclusion Criteria ", j, ": ", br(), cri_text, br(), "Patients meet this criteria:", exclu[[j]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", exclu[[j]]$accumulated_patients,")-->",
                            "id", i+1, "\n")
        } else {
          j <- n - n_inclu - 1
          cri_text <- exclu[[j]][['text']]
          cri_text <- gsub("\\(|\\)", "", cri_text)
          cri_text1 <- exclu[[j+1]][['text']]
          cri_text1 <- gsub("\\(|\\)", "", cri_text1)
          diagram <- paste0(diagram, "id", i,"(Exclusion Criteria ", j, ": ",  br(), cri_text, br(),"Patients meet this criteria:", exclu[[j]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", exclu[[j]]$accumulated_patients,")-->",
                            "id", i+1, "(Exclusion Criteria ", j+1, ": ", br(), cri_text1, br(), "Patients meet this criteria:", exclu[[j+1]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", exclu[[j+1]]$accumulated_patients, ")","\n",'end')
        }
      }
    } else if (n_exclu == 1){
      for (i in c(1 : (n-1))){
        cri_text <- inclu[[i]][['text']]
        cri_text <- gsub("\\(|\\)", "", cri_text)
        if (i == n-1){
          diagram <- paste0(diagram, 'end\n')
        }
        diagram <- paste0(diagram, "id",i,"(Inclusion Criteria ", i, ": ", br(), cri_text, br(), "Patients meet this criteria:", inclu[[i]]$criteria_patients, ", ", 
                          "Patients meet all previous criteria:", inclu[[i]]$accumulated_patients,")-->",
                          "id", i+1, "\n")
      } 
      
      cri_text <- exclu[[1]][['text']]
      cri_text <- gsub("\\(|\\)", "", cri_text)
      print(n)
      diagram <- paste0(diagram, "id", n,"(Exclusion Criteria ", 1, ": ",  br(), cri_text, br(),"Patients meet this criteria:",exclu[[1]]$criteria_patients, ", ", 
                        "Patients meet all previous criteria:", exclu[[1]]$accumulated_patients,")\n")
      
      
    } else {
      for (i in c(1 : n)){
        if (i < n){
          cri_text <- inclu[[i]][['text']]
          cri_text <- gsub("\\(|\\)", "", cri_text)
          diagram <- paste0(diagram, "id",i,"(Inclusion Criteria ", i, ": ", br(), cri_text, br(), "Patients meet this criteria:", inclu[[i]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", inclu[[i]]$accumulated_patients,")-->",
                            "id", i+1, "\n")
        } else {
          cri_text <- inclu[[n]][['text']]
          cri_text <- gsub("\\(|\\)", "", cri_text)
          diagram <- paste0(diagram, "id", n,"(Inclusion Criteria ", n, ": ",  br(), cri_text, br(),"Patients meet this criteria:", inclu[[n]]$criteria_patients, ", ", 
                            "Patients meet all previous criteria:", inclu[[n]]$accumulated_patients,")\n",'end')
        }
      }
    }
    mermaid(diagram)
  })
  
  
  master <- reactive({
    pat <- js_res()[['result_set']]
    master <- data.frame(matrix(unlist(pat), nrow=length(pat), byrow=TRUE))
    colnames(master) <- c('patid', 'age', 'gender', 'race', 'postcode', 'Ethnic Group')
    master
  })
  
  output$masterTable <- renderDataTable({
    master()
  })
  
  output$ageDist <- renderPlot({
    ggplot(master(), aes(x=age)) + geom_histogram(stat = 'count')
  })
  
  output$demoTable <- DT::renderDataTable({
    pat <- master()
    # row 1
    n11 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Female' & pat$race == 'White/Caucasian'))
    n12 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Male' & pat$race == 'White/Caucasian'))
    n13 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'White/Caucasian'))
    n14 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'White/Caucasian'))
    n15 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'White/Caucasian'))
    n16 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'White/Caucasian'))
    n17 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'White/Caucasian'))
    n18 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'White/Caucasian'))
    n19 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'White/Caucasian'))
    
    # row 2
    n21 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Female' & pat$race == 'African American'))
    n22 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Male' & pat$race == 'African American'))
    n23 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'African American'))
    n24 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'African American'))
    n25 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'African American'))
    n26 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'African American'))
    n27 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'African American'))
    n28 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'African American'))
    n29 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'African American'))
    
    # row 3
    n31 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Female' & pat$race == 'Asian'))
    n32 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Male' & pat$race == 'Asian'))
    n33 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Asian'))
    n34 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Asian'))
    n35 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Asian'))
    n36 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Asian'))
    n37 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Asian'))
    n38 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Asian'))
    n39 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Asian'))
    
    # row 4
    n41 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Female' & pat$race == 'Other'))
    n42 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Male' & pat$race == 'Other'))
    n43 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Other'))
    n44 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Other'))
    n45 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Other'))
    n46 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Other'))
    n47 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Other'))
    n48 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Other'))
    n49 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Other'))
    
    # row 5
    n51 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Female' & pat$race == 'Unknown'))
    n52 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender == 'Male' & pat$race == 'Uknown'))
    n53 <- length(which(pat$`Ethnic Group`=='Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Unknown'))
    n54 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Unknown'))
    n55 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Unknown'))
    n56 <- length(which(pat$`Ethnic Group`=='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Unknown'))
    n57 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Female' & pat$race == 'Unknown'))
    n58 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender == 'Male' & pat$race == 'Unknown'))
    n59 <- length(which(pat$`Ethnic Group`!='Hispanic' & pat$`Ethnic Group`!='Non-Hispanic' & pat$gender != 'Female' & pat$gender != 'Male' & pat$race == 'Unknown'))
    
    summaryT <- data.frame('Hispanic' = c(n11, n21, n31, n41, n51), 'Non-Hispanic' = c(n12, n22, n32, n42, n52), 'Other' = c(n13, n23, n33, n43, n53),
                           'Hispanic1' = c(n14, n24, n34, n44, n54), 'Non-Hispanic1' = c(n15, n25, n35, n45, n55), 'Other1' = c(n16, n26, n36, n46, n56),
                           'Hispanic2' = c(n17, n27, n37, n47, n57), 'Non-Hispanic2' = c(n18, n28, n38, n48, n58), 'Other2' = c(n19, n29, n39, n49, n59))
    rownames(summaryT) <- c('White/Caucasian', 'African American', 'Asian', 'Other', 'Unknown')
    colnames(summaryT) <- c('Female', 'Male', 'Unknown','Female', 'Male', 'Unknown','Female', 'Male', 'Unknown')
    
    
    summaryContainer <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(),
          th(colspan = 3, 'Hispanic', class = "dt-center"),
          th(colspan = 3, 'Non-Hispanic', class = "dt-center"),
          th(colspan = 3, 'Other', class = "dt-center")
        ),
        tr(
          th(),
          lapply(names(summaryT), th)
        )
      )
    ))
    
    DT::datatable(summaryT, container = summaryContainer,extensions = "Buttons", options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel')
    ))
    
  })
  
  output$reminder <- renderText({
    res <- js_res()
    inclu <- res[['inclusion']]
    exclu <- res[['exclusion']]
    n_inclu <- length(inclu)
    n_exclu <- length(exclu)
    n <- n_inclu + n_exclu
    
    cri_lst <- c()
    for (i in c(1:n_inclu)){
      if (inclu[[i]] == 'NA'){
        cri_lst <- append()
      }
    }
  })
  
  
}

