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
library(ggplot2)
library(DT)

add_inclu_idx <- 1
add_exclu_idx <- 1

source_python('clinical_trial_py.py')
lab_name <- read.csv('lab_name.csv')
event_name <- read.csv('event_name.csv')

lab_name_lst <- as.list(lab_name)
event_name_lst <- as.list(event_name)


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
          initial_val[[temp]][['Diagnosis Type']] <- lst[[i]][['Diagnosis Type']]
          initial_val[[temp]][['Time Period within']] <- lst[[i]][['Time Period within']]
          initial_val[[temp]][['Search by diagnosis group']] <- lst[[i]][['Search by diagnosis group']]
          initial_val[[temp]][['Encounter based']] <- lst[[i]][['Encounter based']]
        } else if (temp == 'Prescription'){
          #print(lst[[i]])
          initial_val[[temp]][['Drug Description contains']] <- lst[[i]][['Drug Description contains']]
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
          initial_val[[temp]][['LOINC is']] <- lst[[i]][['LOINC is']]
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
          
        } else if (temp == 'Encounter'){
          initial_val[[temp]][['Admit Type']] <- lst[[i]][['Admit Type']]
          initial_val[[temp]][['Encounter Type']] <- lst[[i]][['Encounter Type']]
          initial_val[[temp]][['Discharge Disposition']] <- lst[[i]][['Discharge Disposition']]
        }
      }
    }
    return(initial_val)
  } else {
    return(initial_val)
  }
  
}

# map templates to div output ---------------------------------------------

template_map <- function(temp, temp_dict, idx_temp){
  #print(temp_dict)
  idx_temp <- as.character(idx_temp)
  
  if (temp == 'Diagnosis'){
    if (temp %in% names(temp_dict)){
      if ('Search by diagnosis group' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Search by diagnosis group']] == '1'){
          gr <- 'True'
        } else if (temp_dict[[temp]][['Search by diagnosis group']] == '0'){
          gr <- 'False'
        } else {
          gr <- 'NULL'
        }
      } else {
        gr <- 'NULL'
      }
      
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Encounter based']] == '1'){
          eb <- 'True'
        } else if (temp_dict[[temp]][['Encounter based']] == '0'){
          eb <- 'False'
        } else {
          eb <- 'NULL'
        }
      } else {
        eb <- 'NULL'
      }
      
      return(
        div(
          h3("------- Diagnosis template -------"),
          fluidRow(
            column(width = 3, textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code(Group) is:", value = temp_dict[[temp]][['Diagnosis Code is']])),
            column(width = 3, "Group codes can be found at: https://www.icd10data.com/ICD10CM/DRG")
          ),
          
          textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:", value = temp_dict[[temp]][["Diagnosis Code starts with"]]),
          textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains:", value = temp_dict[[temp]][["Diagnosis Description contains"]]),
          textInput(inputId = paste("diag_type_", idx_temp), label = "Diagnosis Type:", value = temp_dict[[temp]][["Diagnosis Type"]]),
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
          ),
          
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        ))
    } else {
      return(
        div(
          h3("------- Diagnosis template -------"),
          textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:"),
          textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:"),
          textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains"),
          textInput(inputId = paste("diag_type_", idx_temp), label = "Diagnosis Type"),
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
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        ))
    }
    
  } else if(temp == 'Demographic'){
    if (temp %in% names(temp_dict)){
      
      #print(temp_dict[[temp]])
      if (!(is.null(temp_dict[[temp]][['Gender is']]))){
        if (temp_dict[[temp]][['Gender is']] != ""){
          gr <- temp_dict[[temp]][['Gender is']]
        }
        else{
          gr <- 'None'
        }
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
            choices = c("African American", "Asian", "White", "Caucasian"),
            selected = temp_dict[[temp]][['Race is']],
            inline = TRUE, 
            status = "danger"
          ),
          pickerInput(inputId = paste('gender_is_', idx_temp), label = 'Gender', choices=c('Male', 'Female', 'None'), selected=gr),
          pickerInput(inputId = paste('ethic_grp_', idx_temp), label = 'Ethic', choices=c('Hispanic', 'Non-Hispanic', 'None'), selected=eg),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
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
          pickerInput(inputId = paste('ethic_grp_', idx_temp), label = 'Ethic', choices=c('Hispanic', 'Non-Hispanic', 'None'), selected="None"),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    }
    
  } else if (temp == 'Prescription'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Encounter based']] == '1'){
          eb <- 'True'
        } else if (temp_dict[[temp]][['Encounter based']] == '0'){
          eb <- 'False'
        } else {
          eb <- 'NULL'
        }
      } else {
        eb <- 'NULL'
      }
      
      return(
        div(
          h3("------- Drug template -------"),
          textInput(inputId = paste('drug_desc_', idx_temp), label='Drug description contains:',  value = temp_dict[[temp]][['Drug Description contains']]),
          textInput(inputId = paste('drug_time_', idx_temp), label='Time Period within:', value = temp_dict[[temp]][['Time Period within']]),
          
          pickerInput(
            inputId = paste("drug_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          ),
          
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
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
          ),
          
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    }
    
  } else if (temp == 'Event 1'|temp == 'Event 2'|temp == 'Event 3'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Encounter based']] == '1'){
          eb <- 'True'
        } else if (temp_dict[[temp]][['Encounter based']] == '0'){
          print('pass 0')
          eb <- 'False'
        } else {
          print('pass 1')
          eb <- 'NULL'
        }
      } else {
        print('pass 2')
        eb <- 'NULL'
      }
      print(temp_dict[[temp]]$`Event Name contains`)
      return(
        div(
          h3("------- Event template -------"),
          selectizeInput(inputId = paste('event_name_', idx_temp), 
                         label = 'Event name contains',  
                         choices = event_name_lst, options = list(create = TRUE),
                         multiple = TRUE,
                         selected = temp_dict[[temp]]$`Event Name contains`),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_val_fromi_', idx_temp), label='Value from (include: >=)', value = temp_dict[[temp]]$`Value from ( include )`)),
            column(width = 3, textInput(inputId = paste('event_val_fromn_', idx_temp), label='Value from (Not include: >)', value = temp_dict[[temp]]$`Value from ( not include )`)),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_val_toi_', idx_temp), label='Value to (include: <=)', value = temp_dict[[temp]]$`Value to ( include )`)),
            column(width = 3, textInput(inputId = paste('event_val_ton_', idx_temp), label='Value to (include: <)', value = temp_dict[[temp]]$`Value to ( not include )`)),
          ),
          textInput(inputId = paste('event_time_', idx_temp), label='Time period within', value=temp_dict[[temp]][['Time Period within']]),
          pickerInput(
            inputId = paste("event_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    } else {
      return(
        div(
          h3("------- Event template -------"),
          selectizeInput(inputId = paste('event_name_', idx_temp), 
                         label = 'Event name contains',  
                         choices = event_name_lst, options = list(create = TRUE),
                         multiple = TRUE
          ),
          textInput(inputId = paste('event_name_', idx_temp), label='Event name contains'),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_val_fromi_', idx_temp), label='Value from (include: >=)')),
            column(width = 3, textInput(inputId = paste('event_val_fromn_', idx_temp), label='Value from (Not include: >)')),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('event_val_toi_', idx_temp), label='Value to (include: <=)')),
            column(width = 3, textInput(inputId = paste('event_val_ton_', idx_temp), label='Value to (include: <)')),
          ),
          textInput(inputId = paste('event_time_', idx_temp), label='Time period within'),
          pickerInput(
            inputId = paste("event_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    }
    
  } else if (temp == 'Lab 1'|temp == 'Lab 2'|temp == 'Lab 3'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Encounter based']] == '1'){
          eb <- 'True'
        } else if (temp_dict[[temp]][['Encounter based']] == '0'){
          eb <- 'False'
        } else {
          eb <- 'NULL'
        }
      } else {
        eb <- 'NULL'
      }
      
      return(
        div(
          h3("------- Lab template -------"),
          selectizeInput(inputId = paste('lab_name_', idx_temp), 
                         label = 'Lab name contains',  
                         choices = lab_name_lst, options = list(create = TRUE),
                         multiple = TRUE, 
                         selected = temp_dict[[temp]]$`Lab Name contains`),
          textInput(inputId = paste('lab_code_', idx_temp), label='LOINC', value = temp_dict[[temp]]$`LOINC is`),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_val_fromi_', idx_temp), label='Value from (include: >=)', value = temp_dict[[temp]]$`Value from ( include )`)),
            column(width = 3, textInput(inputId = paste('lab_val_fromn_', idx_temp), label='Value from (Not include: >)', value = temp_dict[[temp]]$`Value from ( not include )`)),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_val_toi_', idx_temp), label='Value to (include: <=)', value = temp_dict[[temp]]$`Value to ( include )`)),
            column(width = 3, textInput(inputId = paste('lab_val_ton_', idx_temp), label='Value to (include: <)', value = temp_dict[[temp]]$`Value to ( not include )`)),
          ),
          textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within', value = temp_dict[[temp]]$`Time Period within`),
          pickerInput(
            inputId = paste("lab_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = eb
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    } else {
      return(
        div(
          h3("------- Lab template -------"),
          selectizeInput(inputId = paste('lab_name_', idx_temp), 
                         label = 'Lab name contains',  
                         choices = lab_name_lst, options = list(create = TRUE),
                         multiple = TRUE,
                         selected = NULL),
          textInput(inputId = paste('lab_code_', idx_temp), label='LOINC'),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_val_fromi_', idx_temp), label='Value from (include: >=)')),
            column(width = 3, textInput(inputId = paste('lab_val_fromn_', idx_temp), label='Value from (Not include: >)')),
          ),
          fluidRow(
            column(width = 3, textInput(inputId = paste('lab_val_toi_', idx_temp), label='Value to (include: <=)')),
            column(width = 3, textInput(inputId = paste('lab_val_ton_', idx_temp), label='Value to (include: <)')),
          ),
          textInput(inputId = paste('lab_time_', idx_temp), label = 'Time Period within'),
          pickerInput(
            inputId = paste("lab_base_", idx_temp),
            label = "Encounter based", 
            choices = c('True', 'False', 'NULL'),
            selected = 'NULL'
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    }
    
  } else if (temp == 'Order'){
    if (temp %in% names(temp_dict)){
      if ('Encounter based' %in% names(temp_dict[[temp]])){
        if (temp_dict[[temp]][['Encounter based']] == '1'){
          eb <- 'True'
        } else if (temp_dict[[temp]][['Encounter based']] == '0'){
          eb <- 'False'
        } else {
          eb <- 'NULL'
        }
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
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )           
      )
    } else {
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
          ),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )           
      )
    }
  } else if (temp=='Encounter'){
    if (temp %in% names(temp_dict)){
      return(
        div(
          h3("------- Encounter template -------"),
          textInput(inputId = paste('admit_type_', idx_temp), label='Admit Type:', value=temp_dict[[temp]][['Admit Type']]),
          textInput(inputId = paste("encounter_type_", idx_temp), label = 'Encounter Type:', value=temp_dict[[temp]][['Encounter Type']]),
          textInput(inputId = paste('disch_', idx_temp), label='Discharge Disposition:', value = temp_dict[[temp]][['Discharge Disposition']]),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    } else{
      return(
        div(
          h3("------- Encounter template -------"),
          textInput(inputId = paste('admit_type_', idx_temp), label='Admit Type:', value=''),
          textInput(inputId = paste("encounter_type_", idx_temp), label = 'Encounter Type:', value=''),
          textInput(inputId = paste('disch_', idx_temp), label='Discharge Disposition:', value = ''),
          textInput(inputId = paste("diag_comment_", idx_temp), label='Comments:')
        )
      )
    }
  }
}

# add_template function ---------------------------------------------------


add_template <- function(temp, idx_temp){
  if (temp == 'Diagnosis'){
    return(
      div(
        h3("------- Diagnosis template -------"),
        textInput(inputId = paste("diag_is_", idx_temp),label = "Diagnosis Code is:", value = ''),
        textInput(inputId = paste("diag_like_", idx_temp), label = "Diagnosis Code starts with:", value = ''),
        textInput(inputId = paste("diag_desc_", idx_temp), label = "Diagnosis Description contains:", value = ''),
        textInput(inputId = paste("diag_type_", idx_temp), label = "Diagnosis Type:", value = ''),
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
  } else if(temp == 'Demographic'){
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
  } else if (temp == 'Prescription'){
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
  } else if (temp == 'Event 1' | temp == 'Event 2' | temp == 'Event 3'){
    return(
      div(
        h3("------- Event template -------"),
        selectizeInput(inputId = paste('event_name_', idx_temp), 
                       label='Event name contains', 
                       choices=unique(event_name_lst),
                       multiple = TRUE
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_val_fromi_', idx_temp), label='Value from (include: >=)', value = '')),
          column(width = 3, textInput(inputId = paste('event_val_fromn_', idx_temp), label='Value from (Not include: >)', value = '')),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('event_val_toi_', idx_temp), label='Value to (include: <=)', value = '')),
          column(width = 3, textInput(inputId = paste('event_val_ton_', idx_temp), label='Value to (include: <)', value ='')),
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
  } else if (temp == 'Lab 1' | temp == 'Lab 2' | temp == 'Lab 3'){
    return(
      div(
        h3("------- Lab template -------"),
        selectizeInput(inputId = paste('lab_name_', idx_temp), 
                       label = 'Lab name contains',  
                       choices = lab_name_lst, options = list(create = TRUE),
                       multiple = TRUE
        ),
        textInput(inputId = paste('lab_code_', idx_temp), label='LOINC', value = ''),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_val_fromi_', idx_temp), label='Value from (include: >=)', value = '')),
          column(width = 3, textInput(inputId = paste('lab_val_fromn_', idx_temp), label='Value from (Not include: >)', value = '')),
        ),
        fluidRow(
          column(width = 3, textInput(inputId = paste('lab_val_toi_', idx_temp), label='Value to (include: <=)', value = '')),
          column(width = 3, textInput(inputId = paste('lab_val_ton_', idx_temp), label='Value to (include: <)', value = '')),
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
  } else if (temp == 'Order'){
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
  } else if (temp =='Encounter'){
    return(
      div(
        h3("------- Encounter template -------"),
        textInput(inputId = paste('admit_type_', idx_temp), label = 'Type of admission:', value = ''),
        textInput(inputId = paste('encounter_type_', idx_temp), label = 'Encounter Type:', value =''),
        textInput(inputId = paste('disch_',idx_temp), label = 'Discharge Disposition:', value = '')
      )
    )
  }
}

