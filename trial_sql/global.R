library(shiny)
library(shinydashboard)
library(rjson)
library(shinyWidgets)
library(htmltools)
library(rlist)
library(reticulate)
library(DiagrammeR)

add_inclu_idx <- 1
add_exclu_idx <- 1

#åsource_python('clinical_trial_py.py')

# Initialize the output dataframe -----------------------------------------

# sample <- fromJSON(file = 'Parsed_NCT03805308_Dummy.json')
# inclu_lst <- sample$inclusion
# exclu_lst <- sample$exclusion

# map templates to div output ---------------------------------------------

template_map <- function(initial, idx_temp){
  temp <- initial$template
  idx_temp <- as.character(idx_temp)
  if (temp == 'Condition by Diagnosis Code'){
    return(
      div(
        h3("------- Diagnosis template -------"),
        textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:", value = initial$`Diagnosis Code is`),
        textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code start with:", value = initial[["Diagnosis Code starts with"]]),
        textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains", value = initial[["Diagnosis Description contains"]]),
        textInput(inputId = paste("diag_date_", idx_temp), label = "Diagnosis date within:", value = initial$`Time Period within`),
        pickerInput(
          inputId = paste("diag_grp_", idx_temp),
          label = "Search by diagnosis group", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        ),
        
        pickerInput(
          inputId = paste("diag_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
        
      ))
  } else if(temp == 'Demographic'){
    return(
      div(
        h3("------- Demographics template -------"),
        # sliderInput(inputId = paste('age_within_', idx_temp), label = 'Age within', 0, 100, value = initial$`Age from ( include )`),
        fluidRow(
          column(width = 3, textInput(inputId = paste('age_from_', idx_temp),label = 'Age from',value = initial$`Age from ( include )`)),
          column(width = 3, textInput(inputId = paste('age_to_', idx_temp),label = 'Age to',value = initial$`Age to ( include )`)),
        ),
        awesomeCheckboxGroup(
          inputId = paste('race_is_', idx_temp),
          label = "Race", 
          choices = c("Afriacn American", "Asian", "White", "Caucasian", "None"),
          selected = "None",
          inline = TRUE, 
          status = "danger"
        ),
        pickerInput(inputId = paste('gender_is_', idx_temp), label = 'Gender', choices=c('Male', 'Female', 'None'), selected="None"),
        pickerInput(inputId = paste('ethic_grp_', idx_temp), label = 'Ethic', choices=c('Hispanic', 'Non-Hispanic', 'None'), selected="None")
      )
    )
  } else if (temp == 'Prescription'){
    return(
      div(
        h3("------- Drug template -------"),
        textInput(inputId = paste('drug_desc_', idx_temp), label='Drug description contains:',  value = initial[['Drug description contains']]),
        textInput(inputId = paste('drug_time_', idx_temp), label='Time Period within:', value = initial[['Time Period within']]),
        
        pickerInput(
          inputId = paste("drug_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Event'){
    return(
      div(
        h3("------- Event template -------"),
        textInput(inputId = paste('event_name_', idx_temp), label='Event name contains', value = initial[['Event Name contains']]),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_value_fromi_', idx_temp), label='Value from (include: >=)', value = initial$`Value from ( include )`)),
          column(width = 3, textInput(inputId = paste('event_value_fromn_', idx_temp), label='Value from (Not include: >)', value = initial$`Value from ( not include )`)),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_value_toi_', idx_temp), label='Value to (include: <=)', value = initial$`Value to ( include )`)),
          column(width = 3, textInput(inputId = paste('event_value_ton_', idx_temp), label='Value to (include: <)', value = initial$`Value to ( not include )`)),
        ),
        textInput(inputId = paste('event_time_', idx_temp), label='Time period within', value=initial[['Time Period within']]),
        pickerInput(
          inputId = paste("event_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Lab'){
    return(
      div(
        h3("------- Lab template -------"),
        textInput(inputId = paste('lab_name_', idx_temp), label='Lab name contains', value = initial$`Lab Name contains`),
        textInput(inputId = paste('lab_code_', idx_temp), label='LOINC', value = initial$`LOINC is`),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_value_fromi_', idx_temp), label='Value from (include: >=)', value = initial$`Value from ( include )`)),
          column(width = 3, textInput(inputId = paste('lab_value_fromn_', idx_temp), label='Value from (Not include: >)', value = initial$`Value from ( not include )`)),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_value_toi_', idx_temp), label='Value to (include: <=)', value = initial$`Value to ( include )`)),
          column(width = 3, textInput(inputId = paste('lab_value_ton_', idx_temp), label='Value to (include: <)', value = initial$`Value to ( not include )`)),
        ),
        textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within', value = initial$`Time Period within`),
        pickerInput(
          inputId = paste("lab_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Order'){
    return(
      div(
        h3("------- Order template -------"),
        textInput(inputId = paste('order_name_', idx_temp), label='Procedure Name contains', value=initial$`Procedure Name contains`),
        textInput(inputId = paste('order_time_', idx_temp), label='Time Period within', value=initial$`Time Period within`),
        pickerInput(
          inputId = paste("order_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )           
    )
  }
}


# add_template function ---------------------------------------------------


add_template <- function(temp, idx_temp){
  if (temp == 'Diagnosis'){
    return(
      div(
        h3("------- Diagnosis template -------"),
        textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:", value = ''),
        textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code start with:", value = ''),
        textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains", value = ''),
        textInput(inputId = paste("diag_date_", idx_temp), label = "Diagnosis date within:", value = ''),
        pickerInput(
          inputId = paste("diag_grp_", idx_temp),
          label = "Search by diagnosis group", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        ),
        
        pickerInput(
          inputId = paste("diag_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if(temp == 'Demographics'){
    return(
      div(
        h3("------- Demographics template -------"),
        fluidRow(
          column(width = 3, textInput(inputId = paste('age_from_', idx_temp),label = 'Age from',value = '')),
          column(width = 3, textInput(inputId = paste('age_to_', idx_temp),label = 'Age to',value = '')),
        ),
        awesomeCheckboxGroup(
          inputId = paste('race_is_', idx_temp),
          label = "Race", 
          choices = c("Africann American", "Asian", "White", "Caucasian", "None"),
          selected = "None",
          inline = TRUE, 
          status = "danger"
        ),
        pickerInput(inputId = paste('gender_is_', idx_temp), label = 'Gender', choices=c('Male', 'Female', 'None'), selected="None"),
        pickerInput(inputId = paste('ethic_grp_', idx_temp), label = 'Ethic', choices=c('Hispanic', 'Non-Hispanic', 'None'), selected="None")
      )
    )
  } else if (temp == 'Drug'){
    return(
      div(
        h3("------- Drug template -------"),
        textInput(inputId = paste('drug_desc_', idx_temp), label='Drug description contains:',  value = ''),
        textInput(inputId = paste('drug_time_', idx_temp), label='Time Period within:', value = ''),
        
        pickerInput(
          inputId = paste("drug_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Event'){
    return(
      div(
        h3("------- Event template -------"),
        textInput(inputId = paste('event_name_', idx_temp), label='Event name contains', value = ''),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_value_fromi_', idx_temp), label='Value from (include: >=)', value = '')),
          column(width = 3, textInput(inputId = paste('event_value_fromn_', idx_temp), label='Value from (Not include: >)', value = '')),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_value_toi_', idx_temp), label='Value to (include: <=)', value = '')),
          column(width = 3, textInput(inputId = paste('event_value_ton_', idx_temp), label='Value to (include: <)', value ='')),
        ),
        textInput(inputId = paste('event_time_', idx_temp), label='Time period within', value=''),
        pickerInput(
          inputId = paste("event_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Lab'){
    return(
      div(
        h3("------- Lab template -------"),
        textInput(inputId = paste('lab_name_', idx_temp), label='Lab name contains', value = ''),
        textInput(inputId = paste('lab_code_', idx_temp), label='LOINC', value = ''),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_value_fromi_', idx_temp), label='Value from (include: >=)', value = '')),
          column(width = 3, textInput(inputId = paste('lab_value_fromn_', idx_temp), label='Value from (Not include: >)', value = '')),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_value_toi_', idx_temp), label='Value to (include: <=)', value = '')),
          column(width = 3, textInput(inputId = paste('lab_value_ton_', idx_temp), label='Value to (include: <)', value = '')),
        ),
        textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within', value = ''),
        pickerInput(
          inputId = paste("lab_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )
    )
  } else if (temp == 'Procedure'){
    return(
      div(
        h3("------- Order template -------"),
        textInput(inputId = paste('order_name_', idx_temp), label='Procedure Name contains', value=''),
        textInput(inputId = paste('order_time_', idx_temp), label='Time Period within', value=''),
        pickerInput(
          inputId = paste("order_base_", idx_temp),
          label = "Encounter based", 
          choices = c('True', 'False', 'NULL'),
          selected = 'NULL'
        )
      )           
    )
  }
  
  
  
  
  
  
}
