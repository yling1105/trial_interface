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
    
    dataset <- load_json_py(file1$datapath)
    #dataset <- map_clinical_trial_dump(dataset)
    dataset <- fromJSON(dataset)
    sample <- list()
    sample[['inclusion']] <- dataset$inclusion
    sample[['exclusion']] <- dataset$exclusion
    sample
  })

  # template generate page --------------------------------------------------

  template_page <- eventReactive(input$temp_generate, {
    inclu_lst <- sample()[['inclusion']]
    exclu_lst <- sample()[['exclusion']]
    ni <- length(inclu_lst)
    ne <- length(exclu_lst)
    n <- ni + ne
    
    template_page <- tagList()
    
    for (i in c(1 : ni)){
      cri <- inclu_lst[[i]]
      temptemp <-  cri[['mapped_templates']]
      critext <- temptemp[['display_text']]
      temp_lst <- list()
      if (length(temptemp) > 1){
        for (j in c(1:length(temptemp) - 1)){
          tempj <- temptemp[[j]][['template']]
          temp_lst <- append(temp_lst, tempj)
        }
      }
      selection <- multiInput(inputId = paste0('temp_page_',i), label='Pick templates for the criteria:',
                              choices = c('Demographic', 'Condition by Diagnosis Code', 'Prescription', 'Event 1', 'Event 2', 'Event 3', 'Lab 1', 'Lab 2', 'Lab 3', 'Order'),
                              selected = temp_lst
                              )
      template_page <- tagAppendChild(template_page, h1(paste('Inclusion Criteria', i)))
      template_page <- tagAppendChild(template_page, fluidRow(
        column(width = 6, selection),
        column(width = 6, critext)
      ))
      template_page <- tagAppendChild(template_page, tags$hr())
    }
    
    for (i in c(1 : ne)){
      idx <-  ni + i
      cri <- exclu_lst[[i]]
      temptemp <-  cri[['mapped_templates']]
      critext <- temptemp[['display_text']]
      temp_lst <- list()
      if (length(temptemp) > 1){
        for (j in c(1:length(temptemp) - 1)){
          tempj <- temptemp[[j]][['template']]
          temp_lst <- append(temp_lst, tempj)
        }
      }
      selection <- multiInput(inputId = paste0('temp_page_',idx), label='Pick templates for the criteria:',
                              choices = c('Demographic', 'Condition by Diagnosis Code', 'Prescription', 'Event 1', 'Event 2', 'Event 3', 'Lab 1', 'Lab 2', 'Lab 3', 'Order'),
                              selected = temp_lst
      )
      template_page <- tagAppendChild(template_page, h1(paste('Exclusion Criteria', i)))
      template_page <- tagAppendChild(template_page, fluidRow(
        column(width = 6, selection),
        column(width = 6, critext)
      ))
      template_page <- tagAppendChild(template_page, tags$hr())
    }
    
    template_page
  })
  
  output$template_page <- renderUI(template_page())
  

  # Modify templates --------------------------------------------------------
  inclu_sample <- eventReactive(input$generate, {
    inclu_cri <- list()
    inclu_lst <- sample()[['inclusion']]
    
    inclu_sample <- tagList()
    
    for (i in c(1:length(inclu_lst))){
      idx_temp0 <- paste("inclu", i)
      inclu_temp <- list()
      temp <- inclu_lst[[i]]$mapped_templates
      inclu_text <- inclu_lst[[i]][['display_text']]
      logic_temp <- inclu_lst[[i]][['internal_logic']]
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          kk <- temp[[j]]
          idx_temp1 <- paste(idx_temp0, j)
          inclu_temp[[j]] <- template_map(kk, idx_temp1)
        }
      }
      
      logic_bttn <- pickerInput(
        inputId = paste('inclu_logic', idx_temp0),
        label = "Internal logic", 
        choices = c('AND', 'OR'),
        selected = logic_temp
      )
      
      inclu_sample <- tagAppendChild(inclu_sample,h1(paste('Inclusion Criteria ', i)))
      inclu_sample <- tagAppendChild(inclu_sample, logic_bttn)
      inclu_sample <- tagAppendChild(inclu_sample, fluidRow(
        column(width = 6, inclu_temp),
        column(width = 6, HTML(inclu_text))
      ))
      inclu_sample <- tagAppendChild(inclu_sample, tags$hr())
    }
    inclu_sample
  })
  
  exclu_sample <- eventReactive(input$generate, {
    exclu_cri <- list()
    exclu_lst <- sample()[['exclusion']]
    
    exclu_sample <- tagList()
    
    for (i in c(1:length(exclu_lst))){
      idx_temp0 <- paste('exclu', i)
      exclu_temp <- list()
      temp <- exclu_lst[[i]]$mapped_templates
      exclu_text <- exclu_lst[[i]][['display_text']]
      logic_temp <- exclu_lst[[i]][['internal_logic']]
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          kk <- temp[[j]]
          idx_temp1 <- paste(idx_temp0, j)
          exclu_temp[[j]] <- template_map(kk, idx_temp1)
        }
      }
      
      logic_bttn <- pickerInput(
        inputId = paste('exclu_logic', idx_temp0),
        label = "Internal logic", 
        choices = c('AND', 'OR'),
        selected = logic_temp
      )
      
      exclu_sample <- tagAppendChild(exclu_sample,h1(paste('Exclusion Criteria ', i)))
      exclu_sample <- tagAppendChild(exclu_sample, logic_bttn)
      exclu_sample <- tagAppendChild(exclu_sample, fluidRow(
        column(width = 6, exclu_temp),
        column(width = 6, HTML(exclu_text))
      ))
      exclu_sample <- tagAppendChild(exclu_sample, tags$hr())
    }
    exclu_sample
  })
  # Inclusion page ----------------------------------------------------------
  
  output$inclu_ui <- renderUI(inclu_sample())
  
  # Exclusion criteria -----------------------------------------------------
  
  output$exclu_ui <- renderUI(exclu_sample())
  
  
  # Save to a new file ------------------------------------------------------
  
  new_data <- eventReactive(input$save, {
    new_data <- sample()
    
    inclu <- new_data[['inclusion']]
    exclu <- new_data[['exclusion']]
    
    ## save inclusion part -----------------------------------------------------
    
    for (i in c(1:length(inclu))){
      idx_temp0 <- paste('inclu', i)
      logic_temp <- input[[paste('inclu_logic', idx_temp0)]]
      new_data[['inclusion']][[i]][['internal_logic']] <- logic_temp
      temp <- inclu[[i]][['mapped_templates']]
      
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          template_id <- temp[[j]][['template']]
          idx_temp <- paste(idx_temp0, j)
          
          ## Condition --------
          if (template_id == 'Condition by Diagnosis Code'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Codes starts with']] <- input[[paste("diag_like_", idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
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
            encounter <- input[[paste('drug_enc_'), idx_temp]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
            if (encounter == 'True'){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Encounter based']] <- '1'
            } else if (encounter == 'False'){
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Encounter based']] <- '0'
            } else {
              new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Encounter based']] <- ''
            }
            
            ## Event --------  
          } else if (template_id == 'Event'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('event_val_formi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('event_val_fromn_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('event_val_toi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('event_val_ton_', idx_temp)]]
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
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('lab_val_formi_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('lab_val_fromn_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('lab_val_toi', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('lab_val_ton', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
            
            if (length(input[[paste("lab_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("lab_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("event_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("event_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
            
            ## Order -------- 
          } else if (template_id == 'Order'){
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
            new_data[['inclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_name_', idx_temp)]]
            if (length(input[[paste("order_base_", idx_temp)]]) == 0){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "True"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("order_base_", idx_temp)]] == "False"){
              new_data[["inclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
          }
          
          
        }
        
        
        
      }
    }
    
    ## save exclusion part -----------------------------------------------------
    for (i in c(1:length(exclu))){
      idx_temp0 <- paste("exclu", i)
      logic_temp <- input[[paste('exclu_logic', i)]]
      new_data[['exclusion']][[i]][['internal_logic']] <- logic_temp
      temp <- exclu[[i]][['mapped_templates']]
      if (length(temp) != 0){
        for (j in c(1:length(temp))){
          template_id <- temp[[j]][['template']]
          idx_temp <- paste(idx_temp0, j)
          
          ### Condition --------
          if (template_id == 'Condition by Diagnosis Code'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Codes starts with']] <- input[[paste("diag_like_", idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
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
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
            
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
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('event_val_formi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('event_val_fromn_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('event_val_toi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('event_val_ton_', idx_temp)]]
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
            
            ### Lab -------- 
          } else if (template_id == 'Lab'){
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('lab_val_formi_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('lab_val_fromn_', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('lab_val_toi', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('lab_val_ton', idx_temp)]]
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('lab_time_', idx_temp)]]
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
            new_data[['exclusion']][[i]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_name_', idx_temp)]]
            if (length(input[[paste("order_base_", idx_temp)]]) == 0){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
            } else if (input[[paste("order_base_", idx_temp)]] == "True"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
            } else if (input[[paste("order_base_", idx_temp)]] == "False"){
              new_data[["exclusion"]][[i]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
            }
          }
        }
      }
    }
    
    ### Save added part -----------------
    
    if (input$addCriNum == '' || input$addCriNum == '0'){
      
    } else {
      
      n_added <- strtoi(input$addCriNum)
      
      n_inclu <- length(sample()[['inclusion']])
      n_exclu <- length(sample()[['exclusion']])
      for (i in c(1:n_added)){
        temp_lst <- input[[paste("criTemp", i)]]
        clu <- input[[paste("clu", i)]]
        
        #### Added inclusion ---------------
        if (clu == 'inclu'){
          n_inclu <- n_inclu + 1
          new_data[['inclusion']][[n_inclu]] <- list()
          new_data[['inclusion']][[n_inclu]][["mapped_templates"]] <- list()
          new_data[['inclusion']][[n_inclu]][['internal_logic']] <- input[[paste('add_logic', i)]]
          for (j in c(1:length(temp_lst))){
            temp <- temp_lst[[j]]
            new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]] <- list()
            idx_temp <- paste("add_", clu, i, j)
            if (temp == 'Diagnosis'){
              
              new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- 'Condition by Diagnosis Code'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Codes starts with']] <- input[[paste("diag_like_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
              
              if (length(input[[paste("diag_grp_", idx_temp)]]) == 0){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
              } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
              }
              
              if (input[[paste("diag_base_", idx_temp)]] == 'NULL'){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("diag_base_", idx_temp)]] == "True"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("diag_base_", idx_temp)]] == "False"){
                new_data[["inclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Demographics'){
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
            } else if (temp == 'Drug') {
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- 'Prescription'
              drug_desc <- input[[paste('drug_desc_', idx_temp)]]
              drug_time <- input[[paste('drug_time_', idx_temp)]]
              encounter <- input[[paste('drug_enc_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
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
            } else if (temp =='Event'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Event'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('event_val_formi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('event_val_fromn_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('event_val_toi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('event_val_ton_', idx_temp)]]
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
            } else if (temp == 'Lab'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Lab'
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('lab_val_formi_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('lab_val_fromn_', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('lab_val_toi', idx_temp)]]
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('lab_val_ton', idx_temp)]]
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
              new_data[['exclusion']][[n_inclu]][["mapped_templates"]][[j]][["template"]] <- "Order"
              new_data[['exclusion']][[n_inclu]][["mapped_templates"]][[j]][['Procedure Name contains']] <- input[[paste('order_name_', idx_temp)]]
              new_data[['exclusion']][[n_inclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste('order_name_', idx_temp)]]
              if (length(input[[paste("order_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("order_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("order_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_inclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            }
          }
          
          
          
        } 
        ### Added exclusion ----------
        else {
          n_exclu <- n_exclu + 1
          new_data[['exclusion']][[n_exclu]] <- list()
          new_data[['inclusion']][[n_inclu]][["mapped_templates"]] <- list()
          new_data[['exclusion']][[n_exclu]][['internal_logic']] <- input[[paste('add_logic', i)]]
          for (j in c(1:length(temp_lst))){
            temp <- temp_lst[[j]]
            new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]] <- list()
            new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']]
            idx_temp <- paste("add_", clu, i, j)
            if (temp == 'Diagnosis'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Condition by diagnosis codes'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Code is']] <- input[[paste("diag_is_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Codes starts with']] <- input[[paste("diag_like_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Diagnosis Description contains']] <- input[[paste("diag_desc_", idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Time Period within']] <- input[[paste("diag_date_", idx_temp)]]
              
              if (length(input[[paste("diag_grp_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "NULL"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis"]] <- ''
              } else if (input[[paste("diag_grp_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '1'
              } else if (input[[paste("diag_grp_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Search by diagnosis group"]] <- '0'
              }
              
              if (length(input[[paste("diag_base_", idx_temp)]]) == 0){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("diag_base")]] == 'NULL'){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- ''
              } else if (input[[paste("diag_base_", idx_temp)]] == "True"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '1'
              } else if (input[[paste("diag_base_", idx_temp)]] == "False"){
                new_data[["exclusion"]][[n_exclu]][["mapped_templates"]][[j]][["Encounter based"]] <- '0'
              }
            } else if (temp == 'Demographics'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Demographic'
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
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][[' is']] <- ''
              } else if (eth == 'None'){
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- ''
              } else {
                new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Ethnic_Group is']] <- eth
              }
              
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Age from ( include )']] <- age_from
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Age to ( include )']] <- age_to
            } else if (temp == 'Drug') {
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Prescription'
              drug_desc <- input[[paste('drug_desc_', idx_temp)]]
              drug_time <- input[[paste('drug_time_', idx_temp)]]
              encounter <- input[[paste('drug_enc_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Drug Description contains']] <- drug_desc
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
            } else if (temp =='Event'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Event'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Event Name contains']] <- input[[paste('event_name_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('event_val_formi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('event_val_fromn_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('event_val_toi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('event_val_ton_', idx_temp)]]
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
            } else if (temp == 'Lab'){
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Lab'
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Lab Name contains']] <- input[[paste('lab_name_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['LOINC is']] <- input[[paste('lab_code_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from (include)']] <- input[[paste('lab_val_formi_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value from (not include)']] <- input[[paste('lab_val_fromn_', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to (include)']] <- input[[paste('lab_val_toi', idx_temp)]]
              new_data[['exclusion']][[n_exclu]][["mapped_templates"]][[j]][['Value to (not include)']] <- input[[paste('lab_val_ton', idx_temp)]]
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
              new_data[['inclusion']][[n_inclu]][["mapped_templates"]][[j]][['template']] <- 'Order'
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
            }
          }
          
        }
      }
    }
    
    new_data
  })
  
  
  # Add criteria ------------------------------------------------------------
  
  ## Template selection
  
  add_template_select <- eventReactive(input$addCri, {
    addnumber <- input$addCriNum
    add_template_select <- tagList()
    for (i in c(1:addnumber)){
      add_template_select <- tagAppendChild(add_template_select, h3(paste("Criteria ", i)))
      add_template_select <- tagAppendChild(add_template_select, pickerInput(inputId = paste("clu", i), choices = c("inclu", "exclu")))
      add_template_select <- tagAppendChild(add_template_select, pickerInput(inputId = paste("add_logic", i), choices = c("AND", "OR")))
      add_template_select <- tagAppendChild(add_template_select,       
                                            multiInput(inputId = paste('criTemp', i), 
                                                       label = "Pick the templates",
                                                       choices=c("Demographics", "Diagnosis", "Lab", "Event", "Drug", "Procedure"),
                                                       choiceNames=c("Demographics", "Diagnosis", "Lab", "Event", "Drug", "Procedure")))
      
      
    }
    add_template_select <- tagAppendChild(add_template_select, actionButton(inputId = "Add",label = "Generate criteria",
                                                                            style = "color: #FFF8DC; background-color: #2F4F4F; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "))
    add_template_select
  })
  
  output$add_template_select <- renderUI(add_template_select())
  
  ## Update idx -------
  add_cri <- eventReactive(input$Add,{
    add_cri <- tagList()
    add_cri <- tagAppendChild(add_cri, hr())
    add_cri_temp <- list()
    addnumber <- strtoi(input$addCriNum)
    
    for (i in c(1 : addnumber)){
      clu <- input[[paste("clu", i)]]
      if (clu == 'inclu'){
        add_cri <- tagAppendChild(add_cri, h3("Inclusion","Criteria", i))
      } else {
        add_cri <- tagAppendChild(add_cri, h3("Exclusion", "Criteria", i))
      }
      
      temp_lst <- input[[paste("criTemp", i)]]
      
      for (j in c(1:length(temp_lst))){
        idx_temp <- paste("add_", clu, i, j)
        temptemp <- add_template(temp_lst[j], idx_temp)
        add_cri_temp[[j]] <- temptemp
      }
      add_cri <- tagAppendChild(add_cri, add_cri_temp)
    }
    add_cri
  })
  
  output$add_cri <- renderUI(add_cri())
  
  # Download json file ------------------------------------------------------
  
  # output$ddjson <- downloadHandler(
  #   filename <- function() {
  #     paste("data_", Sys.Date(), ".json")
  #   },
  #   content = function(file) {
  #     list.save(new_data(), file)
  #   },
  #   
  #   contentType =  "application/json"
  #)
  
  
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
    accumulate <- list()
    current <- list()
    diagram <- "graph TB\n"
    for (i in c(1 : (n-1))){
      if (i <= n_inclu){
        diagram <- paste0(diagram, "id",i,"(Inclusion Criteria ", i, ": ", "Criteria patients:", inclu[[i]]$criteria_patients, ", ", 
                          "Accumulate patients:", inclu[[i]]$accumulated_patients,")-->",
                          "id", i+1, "\n")
      } else if (i != n-1) {
        j <- i - n_inclu
        diagram <- paste0(diagram, "id", i,"(Exclusion Criteria ", j, ": ", "Criteria patients:", exclu[[j]]$criteria_patients, ", ", 
                          "Accumulated patients:", exclu[[j]]$accumulated_patients,")-->",
                          "id", i+1, "\n")
      } else {
        j <- n - n_inclu - 1
        diagram <- paste0(diagram, "id", i,"(Exclusion Criteria ", j, ": ", "Criteria patients:", exclu[[j]]$criteria_patients, ", ", 
                          "Accumulated patients:", exclu[[j]]$accumulated_patients,")-->",
                          "id", i+1, "(Exclusion Criteria ", j+1, ": ", "Criteria patients:", exclu[[j]]$criteria_patients, ", ", 
                          "Accumulated patients:", exclu[[j]]$accumulated_patients, ")","\n")
      }
    }
    mermaid(diagram)
  })
  
  
  output$res_db <- renderPrint({
    
    js_res()
  })
  
  
}

