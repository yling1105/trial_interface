# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

server <- function(input, output, session) {

# Upload a json file  -----------------------------------------------------
  output$getData <- reactive({
    return(!is.null(input$getData))
  })
  
  outputOptions(output, "getData", suspendWhenHidden = FALSE)
  
  sample <- reactive({
    file1 <- input$getData
    if (is.null(file1)) { 
      return() 
    } 
    
    dataset <- fromJSON(file=file1$datapath)
    sample <- list()
    sample[['inclusion']] <- dataset$inclusion
    sample[['exclusion']] <- dataset$exclusion
    sample
  })
  

# Generate a form given the input json file -------------------------------
  output_form <- reactive({
    n_i <- length(sample[['inclusion']])
    n_e <- length(sample[['exclusion']])
    output_form <- tagList()
    # Inclusion 
    
    for (i in c(1 : n_i)){
      idx <- paste0('inclu_', i)
      temp <- sample['inclusion'][i]['mapped_templates']
      output_form <- tagAppendChild(output_form, h1(paste('Inclusion criteria', i)))
      if (length(temp) > 0){
        for (j in c(1 : length(temp))){
          idx_temp <- paste(idx, j)
          temp_dict <- temp[[j]]
          value_dict <- json.values(temp_dict)
          temp_form <- json2form(value_dict, idx_temp)
          output_form <- tagAppendChild(output_form, temp_form)
        }
      }
      output_form <- tagAppendChild(output_form, tags$hr())
    }
    
    for (i in c(1:n_e)){
      idx <- paste0('inclu_', i)
      temp <- sample['exclusion'][i]['mapped_templates']
      output_form <- tagAppendChild(output_form, h1(paste('Exclusion criteria', i)))
      if (length(temp) > 0){
        for (j in c(1 : length(temp))){
          idx_temp <- paste(idx, j)
          temp_dict <- temp[[j]]
          value_dict <- json.values(temp_dict)
          temp_form <- json2form(value_dict, idx_temp)
          output_form <- tagAppendChild(ouput_form, temp_form)
        }
      }
      output_form <- tagAppendChild(output_form, tags$hr())
    }
  })
  
  

# Next page event ---------------------------------------------------------
  
  ntext <- eventReactive(input$goButton,{
    
    # Initialize values; refresh each session
    values <- reactiveValues()
    values$cri_index <- 1
    
    # Check if the value satisfy the inclusion or exclusion criteria
    #   If satisfy an inclusion criteria: 
    #     Check if the counter `values$count` are not equal to the length of your questions
    #       If not then increment questions by 1 and return that question
    # If satisfy an exclusion criteria:
    #   Break; Raise an notice that the patients is not qualified for this trial
    #   else: go next
    # Note that initially the button hasn't been pressed yet so the `ntext()` will not be executed
    
    flag <- check_cri()
    if (flag){
      if (values$cri_index != length(cri_lst)){
        values$cri_index <- values$cri_index + 1
        return(cri_lst()[values$cri_index])
      }
    } else {
      return(div(h1('The patients is not qualified!')))
    }
    if(values$count != length(questions)){
      values$count <- values$count + 1
      return(questions[values$count])
    }
    else{
      # otherwise just return the last questions
      return(questions[length(questions)])
    }
  })
  
  
}