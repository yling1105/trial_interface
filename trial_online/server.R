# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

server <- function(input, output, session) {

# Upload a json file  -----------------------------------------------------
  
  sample <- eventReactive(input$genForm,{
    trial_temp <- input$selectedTrials
    idx <- match(trial_temp, namelst)
    sample <- data_files[[idx]]
    sample
  })
  

# Generate a form given the input json file -------------------------------
  output_form <- reactive({
    data <- sample()
    n_i <- length(data[['inclusion']])
    n_e <- length(data[['exclusion']])
    output_form <- tagList()
    
    # Inclusion 
    
    for (i in c(1 : n_i)){
      
      idx <- paste0('inclu_', i)
      temp <- data[['inclusion']][[i]][['mapped_templates']]
      text_temp <- data[['inclusion']][[i]][['text']]
      logic_temp <- data[['inclusion']][[i]][['logic']]
      output_form <- tagAppendChild(output_form, h3(paste('Inclusion criteria', i)))
      if ((length(temp) > 0) & ((logic_temp == 'AND') | logic_temp == 'OR')){
        output_form <- tagAppendChild(output_form, h5(text_temp))
        for (j in c(1 : length(temp))){
          idx_temp <- paste0(idx, j)
          temp_dict <- temp[[j]]
          value_dict <- json.values(temp_dict)
          temp_form <- json2form(value_dict, idx_temp)
          output_form <- tagAppendChild(output_form, temp_form)
        }
      } else {
        diag <- prettySwitch(
          inputId = paste0('txt_inclu_', i),
          label = text_temp,
          status = "success",
          fill = TRUE
        )
        output_form <- tagAppendChild(output_form, diag)
      }
      
      output_form <- tagAppendChild(output_form, tags$hr())
    }
    
    for (i in c(1:n_e)){
      
      idx <- paste0('exclu_', i)
      temp <- data[['exclusion']][[i]][['mapped_templates']]
      text_temp <- data[['exclusion']][[i]][['text']]
      output_form <- tagAppendChild(output_form, h3(paste('Exclusion criteria', i)))
      if (length(temp) > 0){
        output_form <- tagAppendChild(output_form, h5(text_temp))
        for (j in c(1 : length(temp))){
          idx_temp <- paste0(idx, j)
          temp_dict <- temp[[j]]
          value_dict <- json.values(temp_dict)
          temp_form <- json2form(value_dict, idx_temp)
          output_form <- tagAppendChild(output_form, temp_form)
        }
      } else{
        diag <- prettySwitch(
          inputId = paste0('txt_exclu_', i),
          label = text_temp,
          status = "success",
          fill = TRUE
        )
        output_form <- tagAppendChild(output_form, diag)
      }
      output_form <- tagAppendChild(output_form, tags$hr())
    }
    
    confirm <- actionButton(inputId = "submitForm", label = "Submit",
                            style = "color: #fff; background-color: #1E90FF; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 10px 18px; ")
    output_form <- tagAppendChild(output_form, confirm)
    
    output_form
  })
  
  output$qe_pat <- renderUI(output_form())

# judgement event ---------------------------------------------------------
  
  observeEvent(input$submitForm, {
    standard <- sample()
    flag <-judgement_func(standard, input, output, session)
    print(flag)
    if (flag == 0){
      shinyalert('The patient is not qualified!', type = "error")
    } else if (flag == 1) {
      shinyalert('The patient is qualified for the trial!', type = "success")
    } else {
      shinyalert('')
    }
  })
  
  
}