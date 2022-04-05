library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyTime)
library(rjson)
library(shinyWidgets)

# Read all the json files into the memory ---------------------------------

filelst <- list.files(path = './trial_json/', pattern = '*.json')
namelst <- list()
for (i in c(1:length(filelst))){
  temp_nm <- strsplit(filelst[[i]], '[.]')
  print(temp_nm)
  namelst <- append(namelst, temp_nm[[1]][[1]])
}


read_json <- function(file_name){
  file_name <- paste0('./trial_json/', file_name)
  a <- fromJSON(file = file_name)
  return(a)
}

data_files <- lapply(filelst, function(x) read_json(x)) 

# Extract existing values from the json file ------------------------------

json.values <- function(temp_dict){
  value_dict <- list()
  for (name in names(temp_dict)){
    if (temp_dict[[name]] != '' & temp_dict[[name]] != 'NULL'){
      value_dict[[name]] <- temp_dict[[name]]
    }
  }
}

# Function used for generating forms given one criteria --------------------------------------

json2form <- function(value_dict,idx_temp){
  form <- tagList()
  temp <- value_dict[[template]]
  if (temp == 'Demographic'){
    form <- tagAppendChild(form, textInput(inputId = paste0('age_',idx_temp), label = 'Age'))
    form <- tagAppendChild(form, selectInput(inputId = paste0('gdr_',idx_temp), label = 'Gender', choices = c('Female', 'Male'), selected = NULL))
    form <- tagAppendChild(form, textInput(inputId = paste0('race_',idx_temp), label = 'Race', choices = c("Afriacn American", "Asian", "White", "Caucasian"), selected = NULL))
    form <- tagAppend(Chile(form, textInput(inputId = paste0('race_', idx_temp), label = 'Ethic group', choices = c('Hispanic', 'Non-Hispanic'), selected=NULL)))
  } else if (temp == 'Condition by Diagnosis Code') {
    if ("Diagnosis code is" %in% names(value_dict)){
      choice_lst <- append(value_dict[['Diagnosis code is']], 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_is_', idx_temp), label = "Have following diagnosis codes:", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    if ('Diagnosis Code starts with' %in% names(value_dict)){
      choice_lst <- append(value_dict[['Diagnosis Code starts with']], 'None')
      
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_like_', idx_temp), label = "Have diagnosis codes starts with:", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ("Diagnosis Description contains" %in% names(value_dict)){
      choice_lst <- append(value_dict[['Diagnosis Description contains']], 'None')
      
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_desc_', idx_temp), label = "Have following diagnosis description:", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ("Diagnosis Type" %in% names(value_dict)){
      choice_lst <- append(value_dict[['Diagnosis Type']], 'None')
      
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_desc_', idx_temp), label = "Have following diagnosis description:", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = 'diag_time_',
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter based' %in% value_dict){
      diag <- prettySwitch(
        inputId = 'diag_base_',
        label = paste('Encounter based', time),
        status = "success",
        fill = TRUE
      )
    form <- tagAppendChild(form, diag)
    }
  } else if (temp == 'Prescription'){
    if ('Drug Description contains' %in% names(value_dict)){
      drugs <- strsplit(value_dict[['Drug Description contains']], '[|]')
      drugs <- append(drugs, 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('drug_desc_', idx_temp), label = "Patient took the drug(s):", 
        choices = drugs,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = 'drug_time_',
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter based' %in% value_dict){
      diag <- prettySwitch(
        inputId = 'drug_base_',
        label = paste('Encounter based', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
  } else if (temp == 'Event'){
    if ('Event Name contains' %in% names(value_dcit)){
      events <- value_dict[['Event Name contains']]
      events <- append(events, 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('event_desc_', idx_temp), label = "Patient encounter the events:", 
        choices = events,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('event_val_fromi' %in% names(value_dict)|'event_val_fromn' %in% names(value_dict) | 'event_val_toi' %in% names(value_dict) | 'event_val_ton' %in% names(value_dict)){
      diag <- textInput(inputId = paste0('event_val_', idx_temp), label = 'Event value:')
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = 'event_time_',
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter based' %in% value_dict){
      diag <- prettySwitch(
        inputId = 'event_base_',
        label = paste('Encounter based', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    } 
  } else if (temp == 'Lab'){
    if ('Lab Name contains' %in% names(value_dcit)){
      labs <- value_dict[['Lab Name contains']]
      labs <- append(labs, 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('lab_desc_', idx_temp), label = "Patient took the lab tests:", 
        choices = labs,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Value from ( include )' %in% names(value_dict)|'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
      diag <- textInput(inputId = paste0('lab_val_', idx_temp), label = 'Lab test values:')
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = 'lab_time_',
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter based' %in% value_dict){
      diag <- prettySwitch(
        inputId = paste('base_'),
        label = paste('Encounter based', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    } 
  } else if (temp == 'Order'){
    orders <- strsplit(value_dict[['order_name']], '[|]')
    orders <- append(orders, 'None')
    diag <- awesomeCheckboxGroup(
      inputId = paste0('order_name_', idx_temp), label = "Patient took the order:", 
      choices = orders,
      selected = NULL,inline = TRUE, status = "danger"
    )
    form <- tagAppendChild(form, diag)
    
    if ('Time Period within' %in% names(val_dict)){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = paste0('order_time_', idx_temp),
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter based' %in% names(value_dict)){
      diag <- prettySwitch(
        inputId = paste('order_base_', idx_temp),
        label = paste('Encounter based', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
  } else if (temp == 'Encounter') {
    if ('Admit Type' %in% names(value_dict)){
      admits <- value_dict[['Admit Type']]
      diag <- awesomeCheckboxGroup(
        inputId = paste0('admit_type_', idx_temp), label = "Admission type:", 
        choices = admits,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Encounter Type' %in% names(value_dict)){
      enc <- value_dict[['Encounter Type']]
      diag <- awesomeCheckboxGroup(
        inputId = paste0('enc_type_', idx_temp), label = "Encounter type:",
        choices = enc,
        selected = NULL, inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Discharge Disposition' %in% names(value_dict)){
      dd <- value_dict[['Discharge Disposition']]
      diag <- awesomeCheckboxGroup(
        inputId = paste0('dich_disp_', idx_temp), label = "Discharge Disposition:",
        choices = enc,
        selected = NULL, inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
  }
  
  
}
