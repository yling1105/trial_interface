library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyTime)
library(rjson)
library(shiny)

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


# Extract existing values from the json file ------------------------------

json.values <- function(temp_dict){
  value_dict <- list()
  for (name in names(temp_dict)){
    if (temp_dict[[name]] != ''){
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
        inputId = paste0('diag_is_', idx_temp), label = "Have following diagnosis", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
    }
    diag <- textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code(Group) is:", value = temp_dict[[temp]][['Diagnosis Code is']])),
      column(width = 3, "Group codes can be found at: https://www.icd10data.com/ICD10CM/DRG")
    ),
    
    textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:", value = temp_dict[[temp]][["Diagnosis Code starts with"]]),
    textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains:", value = temp_dict[[temp]][["Diagnosis Description contains"]]),
    selectizeInput(inputId = paste("diag_type_", idx_temp), 
                   label = "Diagnosis Type:", 
                   choices = diag_type_lst,
                   options = list(create = TRUE),
                   multiple = TRUE,
                   selected = temp_dict[[temp]][["Diagnosis Type"]]),
    textInput(inputId = paste("diag_date_", idx_temp), label = "Diagnosis date within:", value = temp_dict[[temp]][['Time Period within']]),
    
    pickerInput(
      inputId = paste("diag_grp_", idx_temp),
      label = "Search by diagnosis group", 
      choices = c('True', 'False', 'NULL'),
      selected = gr
    ),
    
    pickerInput(
      inputId = paste("diag_base_", idx_temp),
      label = "Encounter based", 
      choices = c('True', 'False', 'NULL'),
      selected = eb
    )
  }
}