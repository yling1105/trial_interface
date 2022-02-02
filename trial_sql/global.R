library(shiny)
library(shinydashboard)
library(rjson)
library(shinyWidgets)
library(htmltools)
library(rlist)
library(reticulate)
library(DiagrammeR)
library(shinyjs)
library(shinyBS)

add_inclu_idx <- 1
add_exclu_idx <- 1

source_python('clinical_trial_py.py')


# Get initial values from original file -----------------------------------

initial <- function(lst){
  initial_val <- list()
  if (length(lst) > 0){
    idx_eve <- 1
    idx_lab <- 1
    for (i in c(1:length(lst))){
      
      if ("template" %in% names(lst[[i]])){
        temp <- lst[[i]][['template']]
        #print(temp)
        initial_val[[temp]] <- list()
        if (temp == 'Demographic'){
          initial_val[[temp]][['Age from ( include )']] <- lst[[i]][['Age from ( include )']]
          initial_val[[temp]][['Age to ( include )']] <- lst[[i]][['Age to ( include )']]
          initial_val[[temp]][['Race is']] <- lst[[i]][['Race is']]
          initial_val[[temp]][['Gender is']] <- lst[[i]][['Gender is']]
          initial_val[[temp]][['Ethnic_Group is']] <- lst[[i]][['Ethnic_Group is']]
        } else if (temp == 'Condition by Diagnosis Code'){
          temp <- 'Diagnosis'
          initial_val[[temp]][['Diagnosis Code is']] <- lst[[i]][['Diagnosis Code is']]
          initial_val[[temp]][['Diagnosis Code starts with']] <- lst[[i]][['Diagnosis Code starts with']]
          initial_val[[temp]][['Diagnosis Description contains']] <- lst[[i]][['Diagnosis Description contains']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Search by diagnosis group']] <- lst[[i]][['Search by diagnosis group']]
          initial_val[[temp]][['Encounters']] <- lst[[i]][['Encounters']]
        } else if (temp == 'Prescription'){
          initial_val[[temp]][['Drug description contains']] <- lst[[i]][['Drug description contains']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Encounter based']] <- lst[[i]][['Encounter based']]
        } else if (temp == "Event"){
          temp <- paste('Event', idx_eve)
          idx_eve <- idx_eve + 1
          initial_val[[temp]][['Event Name contains']] <- lst[[i]][['Event Name contains']]
          initial_val[[temp]][['Value from ( include )']] <- lst[[i]][['Value from ( include )']]
          initial_val[[temp]][['Value from ( not include )']] <- lst[[i]][['Value from ( not include )']]
          initial_val[[temp]][['Value to ( include )']] <- lst[[i]][['Value to ( include )']]
          initial_val[[temp]][['Value to ( not include )']] <- lst[[i]][['Value to ( not include )']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Encounter based']] <- lst[[i]][['Encounter based']]
        } else if (temp == "Lab"){
          temp <- paste("Lab", idx_lab)
          idx_lab <- idx_lab + 1
          initial_val[[temp]][['Lab Name contains']] <- lst[[i]][['Lab Name contains']]
          initial_val[[temp]][['Loinc Code']] <- lst[[i]][['LOINC is']]
          initial_val[[temp]][['Value from ( include )']] <- lst[[i]][['Value from ( include )']]
          initial_val[[temp]][['Value from ( not include )']] <- lst[[i]][['Value from ( not include )']]
          initial_val[[temp]][['Value to ( include )']] <- lst[[i]][['Value to ( include )']]
          initial_val[[temp]][['Value to ( not include )']] <- lst[[i]][['Value to ( not include )']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Encounter based']] <- lst[[i]][['Encounter based']]
          #print(initial_val[[temp]])
        } else if (temp == 'Order'){
          initial_val[[temp]][['Procedure Name contains']] <- lst[[i]][['Procedure Name contains']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Encounter based']] <- lst[[i]][['Encounter based']]
          
        }
      }
    }
    #print(initial_val)
    return(initial_val)
  } else {
    return(initial_val)
  }
  
}

# map templates to div output ---------------------------------------------

template_map <- function(temp, temp_dict, idx_temp){
  print(temp_dict)
  idx_temp <- as.character(idx_temp)
  
  if (temp == 'Diagnosis'){
    if (temp %in% names(temp_dict)){
      if ('Search by diagnosis group' %in% names(temp_dict[[temp]])){
        gr <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        gr <- 'NULL'
      }
      
      if ('Encounter based' %in% names(temp_dict)){
        eb <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        eb <- 'NULL'
      }
      return(
        div(
          h3("------- Diagnosis template -------"),
          textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:", value = temp_dict[[temp]][['Diagnosis Code is']]),
          textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:", value = temp_dict[[temp]][["Diagnosis Code starts with"]]),
          textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains", value = temp_dict[[temp]][["Diagnosis Description contains"]]),
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
        ))
    } else {
      return(
        div(
          h3("------- Diagnosis template -------"),
          textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:"),
          textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:"),
          textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains"),
          textInput(inputId = paste("diag_date_", idx_temp), label = "Diagnosis date within:"),
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
    }
    
  } else if(temp == 'Demographic'){
    if (temp %in% names(temp_dict)){
      if ('Gender is' %in% names(temp_dict[[temp]])){
        gr <- temp_dict[[temp]][['Gender is']]
      } else {
        gr <- 'None'
      }
      
      if ('Ethnic_Group is' %in% temp_dict[[temp]]){
        eg <- temp_dict[[temp]][['Ethnic_Group is']]
      } else {
        eg <- 'None'
      }
      return(
        div(
          h3("------- Demographics template -------"),
          # sliderInput(inputId = paste('age_within_', idx_temp), label = 'Age within', 0, 100, value = temp_dict[[temp]]$`Age from ( include )`),
          fluidRow(
            column(width = 3, textInput(inputId = paste('age_from_', idx_temp),label = 'Age from',value = temp_dict[[temp]][['Age from ( include )']])),
            column(width = 3, textInput(inputId = paste('age_to_', idx_temp),label = 'Age to',value = temp_dict[[temp]][['Age to ( include )']])),
          ),
          awesomeCheckboxGroup(
            inputId = paste('race_is_', idx_temp),
            label = "Race", 
            choices = c("Afriacn American", "Asian", "White", "Caucasian"),
            selected = temp_dict[[temp]][['Race is']],
            inline = TRUE, 
            status = "danger"
          ),
          pickerInput(inputId = paste('gender_is_', idx_temp), label = 'Gender', choices=c('Male', 'Female', 'None'), selected=gr),
          pickerInput(inputId = paste('ethic_grp_', idx_temp), label = 'Ethic', choices=c('Hispanic', 'Non-Hispanic', 'None'), selected=eg)
        )
      )
    } else {
      return(
        div(
          h3("------- Demographics template -------"),
          # sliderInput(inputId = paste('age_within_', idx_temp), label = 'Age within', 0, 100, value = temp_dict[[temp]]$`Age from ( include )`),
          fluidRow(
            column(width = 3, textInput(inputId = paste('age_from_', idx_temp),label = 'Age from',value = temp_dict[[temp]]$`Age from ( include )`)),
            column(width = 3, textInput(inputId = paste('age_to_', idx_temp),label = 'Age to',value = temp_dict[[temp]]$`Age to ( include )`)),
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
    }
    
  } else if (temp == 'Prescription'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        eb <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        eb <- 'NULL'
      }
      return(
        div(
          h3("------- Drug template -------"),
          textInput(inputId = paste('drug_desc_', idx_temp), label='Drug description contains:',  value = temp_dict[[temp]][['Drug description contains']]),
          textInput(inputId = paste('drug_time_', idx_temp), label='Time Period within:', value = temp_dict[[temp]][['Time Period within']]),
          
          pickerInput(
            inputId = paste("drug_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          )
        )
      )
    } else {
      return(
        div(
          h3("------- Drug template -------"),
          textInput(inputId = paste('drug_desc_', idx_temp), label='Drug description contains:'),
          textInput(inputId = paste('drug_time_', idx_temp), label='Time Period within:'),
          
          pickerInput(
            inputId = paste("drug_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          )
        )
      )
    }
    
  } else if (temp == 'Event 1'|temp == 'Event 2'|temp == 'Event 3'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        eb <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        eb <- 'NULL'
      }
      return(
        div(
          h3("------- Event template -------"),
          textInput(inputId = paste('event_name_', idx_temp), label='Event name contains', value = temp_dict[[temp]][['Event Name contains']]),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_value_fromi_', idx_temp), label='Value from (include: >=)', value = temp_dict[[temp]]$`Value from ( include )`)),
            column(width = 3, textInput(inputId = paste('event_value_fromn_', idx_temp), label='Value from (Not include: >)', value = temp_dict[[temp]]$`Value from ( not include )`)),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_value_toi_', idx_temp), label='Value to (include: <=)', value = temp_dict[[temp]]$`Value to ( include )`)),
            column(width = 3, textInput(inputId = paste('event_value_ton_', idx_temp), label='Value to (include: <)', value = temp_dict[[temp]]$`Value to ( not include )`)),
          ),
          textInput(inputId = paste('event_time_', idx_temp), label='Time period within', value=temp_dict[[temp]][['Time Period within']]),
          pickerInput(
            inputId = paste("event_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          )
        )
      )
    } else {
      return(
        div(
          h3("------- Event template -------"),
          textInput(inputId = paste('event_name_', idx_temp), label='Event name contains'),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_value_fromi_', idx_temp), label='Value from (include: >=)')),
            column(width = 3, textInput(inputId = paste('event_value_fromn_', idx_temp), label='Value from (Not include: >)')),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_value_toi_', idx_temp), label='Value to (include: <=)')),
            column(width = 3, textInput(inputId = paste('event_value_ton_', idx_temp), label='Value to (include: <)')),
          ),
          textInput(inputId = paste('event_time_', idx_temp), label='Time period within'),
          pickerInput(
            inputId = paste("event_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          )
        )
      )
    }
    
  } else if (temp == 'Lab 1'|temp == 'Lab 2'|temp == 'Lab 3'){
    print(temp)
    if (temp %in% names(temp_dict)){
      print(temp_dict[[temp]])
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        eb <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        eb <- 'NULL'
      }  
      return(
        div(
          h3("------- Lab template -------"),
          textInput(inputId = paste('lab_name_', idx_temp), label='Lab name contains', value = temp_dict[[temp]]$`Lab Name contains`),
          textInput(inputId = paste('lab_code_', idx_temp), label='LOINC', value = temp_dict[[temp]]$`LOINC is`),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_value_fromi_', idx_temp), label='Value from (include: >=)', value = temp_dict[[temp]]$`Value from ( include )`)),
            column(width = 3, textInput(inputId = paste('lab_value_fromn_', idx_temp), label='Value from (Not include: >)', value = temp_dict[[temp]]$`Value from ( not include )`)),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_value_toi_', idx_temp), label='Value to (include: <=)', value = temp_dict[[temp]]$`Value to ( include )`)),
            column(width = 3, textInput(inputId = paste('lab_value_ton_', idx_temp), label='Value to (include: <)', value = temp_dict[[temp]]$`Value to ( not include )`)),
          ),
          textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within', value = temp_dict[[temp]]$`Time Period within`),
          pickerInput(
            inputId = paste("lab_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          )
        )
      )
    } else {
      return(
        div(
          h3("------- Lab template -------"),
          textInput(inputId = paste('lab_name_', idx_temp), label='Lab name contains'),
          textInput(inputId = paste('lab_code_', idx_temp), label='LOINC'),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_value_fromi_', idx_temp), label='Value from (include: >=)')),
            column(width = 3, textInput(inputId = paste('lab_value_fromn_', idx_temp), label='Value from (Not include: >)')),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_value_toi_', idx_temp), label='Value to (include: <=)')),
            column(width = 3, textInput(inputId = paste('lab_value_ton_', idx_temp), label='Value to (include: <)')),
          ),
          textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within', value = temp_dict[[temp]]$`Time Period within`),
          pickerInput(
            inputId = paste("lab_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          )
        )
      )
    }
    
  } else if (temp == 'Order')
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict)){
        eb <- temp_dict[[temp]][['Search by diagnosis group']]
      } else {
        eb <- 'NULL'
      }
      return(
        div(
          h3("------- Order template -------"),
          textInput(inputId = paste('order_name_', idx_temp), label='Procedure Name contains', value=temp_dict[[temp]]$`Procedure Name contains`),
          textInput(inputId = paste('order_time_', idx_temp), label='Time Period within', value=temp_dict[[temp]]$`Time Period within`),
          pickerInput(
            inputId = paste("order_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          )
        )           
      )
    } else {
      return(
        div(
          h3("------- Order template -------"),
          textInput(inputId = paste('order_name_', idx_temp), label='Procedure Name contains', value=temp_dict[[]]),
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
        textInput(inputId = paste('order_name_', idx_temp), label='Procedure Name contains', value=temp_dict[[]]),
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
