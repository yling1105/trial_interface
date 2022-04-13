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
    #print(name)
    if ((temp_dict[[name]] != '') & (temp_dict[[name]] != 'NULL')){
      value_dict[[name]] <- temp_dict[[name]]
    }
  }
  value_dict
}

# Function used for generating forms given one criteria --------------------------------------

json2form <- function(value_dict,idx_temp){
  form <- tagList()
  temp <- value_dict[['template']]
  
  if (temp == 'Demographic'){

    # Demographic -------------------------------------------------------------
    form <- tagAppendChild(form, textInput(inputId = paste0('age_',idx_temp), label = 'Age'))
    form <- tagAppendChild(form, pickerInput(inputId = paste0('gdr_',idx_temp), label = 'Gender', choices = c('Female', 'Male', 'Unknown'), selected = 'Unknown'))
    form <- tagAppendChild(form, pickerInput(inputId = paste0('race_',idx_temp), label = 'Race', choices = c("Afriacn American", "Asian", "White", "Caucasian", 'Unknown'), selected = 'Unknown'))
    form <- tagAppendChild(form, pickerInput(inputId = paste0('ethic_', idx_temp), label = 'Ethic group', choices = c('Hispanic', 'Non-Hispanic', 'Unknown'), selected='Unknown'))
  } else if (temp == 'Condition by Diagnosis Code') {

    # Diagnosis ---------------------------------------------------------------
    
    if ("Diagnosis Code is" %in% names(value_dict)){
      codes <- strsplit(value_dict[['Diagnosis Code is']], '[|]')
      codes <- unlist(codes)
      codes <- append(codes, 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_is_', idx_temp), label = "Have following diagnosis codes:", 
        choices = codes,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    if ('Diagnosis Code starts with' %in% names(value_dict)){
      codes <- strsplit(value_dict[['Diagnosis Code starts with']], '[|]')
      codes <- unlist(codes)
      codes <- append(codes, 'None')
      
      diag <- awesomeCheckboxGroup(
        inputId = paste0('diag_like_', idx_temp), label = "Have diagnosis codes start with:", 
        choices = codes,
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
    
  } else if (temp == 'Prescription'){

    # Durg --------------------------------------------------------------------

    
    if ('Drug Description contains' %in% names(value_dict)){
      drugs <- strsplit(value_dict[['Drug Description contains']], '[|]')
      drugs <- unlist(drugs)
      drugs <- append(drugs, 'unknown')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('drug_desc_', idx_temp), label = "Patient took the drug(s):", 
        choices = drugs,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time Period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = paste0('drug_time_', idx_temp),
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
  
  } else if (temp == 'Event'){
    ## Event -----------
    if ('Event Name contains' %in% names(value_dict)){
      events <- value_dict[['Event Name contains']]
      events <- append(events, 'None')
      diag <- awesomeCheckboxGroup(
        inputId = paste0('event_desc_', idx_temp), label = "Patient encounter the events:", 
        choices = events,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Value from ( include )' %in% names(value_dict)|'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
      diag <- textInput(inputId = paste0('event_val_', idx_temp), label = 'Event value:')
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time Period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = paste0('event_time_', idx_temp),
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    
  } else if (temp == 'Lab'){
    # Lab -----------
    if ('Lab Name contains' %in% names(value_dict)){
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
    
    if ('Time Period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = 'lab_time_',
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    # if ('Encounter based' %in% value_dict){
    #   diag <- prettySwitch(
    #     inputId = paste('base_'),
    #     label = paste('Encounter based', time),
    #     status = "success",
    #     fill = TRUE
    #   )
    #   form <- tagAppendChild(form, diag)
    # } 
  } else if (temp == 'Order'){

    # Order -------------------------------------------------------------------

    
    print(value_dict)
    orders <- strsplit(value_dict[['Procedure Name contains']], '[|]')
    orders <- unlist(orders)
    orders <- append(orders, 'None')
    diag <- awesomeCheckboxGroup(
      inputId = paste0('order_name_', idx_temp), label = "Patient took the order:", 
      choices = orders,
      selected = NULL,inline = TRUE, status = "danger"
    )
    form <- tagAppendChild(form, diag)
    
    if ('Time Period within' %in% names(value_dict)){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = paste0('order_time_', idx_temp),
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
    # if ('Encounter based' %in% names(value_dict)){
    #   diag <- prettySwitch(
    #     inputId = paste0('order_base_', idx_temp),
    #     label = 'Encounter based',
    #     status = "success",
    #     fill = TRUE
    #   )
    #   form <- tagAppendChild(form, diag)
    # }
  }
  # } else if (temp == 'Encounter') {
  #   if ('Admit Type' %in% names(value_dict)){
  #     admits <- value_dict[['Admit Type']]
  #     diag <- awesomeCheckboxGroup(
  #       inputId = paste0('admit_type_', idx_temp), label = "Admission type:", 
  #       choices = admits,
  #       selected = NULL,inline = TRUE, status = "danger"
  #     )
  #     form <- tagAppendChild(form, diag)
  #   }
  #   
  #   if ('Encounter Type' %in% names(value_dict)){
  #     enc <- value_dict[['Encounter Type']]
  #     diag <- awesomeCheckboxGroup(
  #       inputId = paste0('enc_type_', idx_temp), label = "Encounter type:",
  #       choices = enc,
  #       selected = NULL, inline = TRUE, status = "danger"
  #     )
  #     form <- tagAppendChild(form, diag)
  #   }
  #   
  #   if ('Discharge Disposition' %in% names(value_dict)){
  #     dd <- value_dict[['Discharge Disposition']]
  #     diag <- awesomeCheckboxGroup(
  #       inputId = paste0('dich_disp_', idx_temp), label = "Discharge Disposition:",
  #       choices = enc,
  #       selected = NULL, inline = TRUE, status = "danger"
  #     )
  #     form <- tagAppendChild(form, diag)
  #   }
  # }
  form
}

# judgement function ------------------------------------------------------

judgement_func <- function(standard, input, output, session){
  n_i <- length(standard[['inclusion']])
  n_e <- length(standard[['exclusion']])
  flag <- -1
  undefined <- c()
  
  for (i in c(1:n_i)){

  # Inclusion ---------------------------------------------------------------

    idx <- paste0('inclu_', i)
    temp <- standard[['inclusion']][[i]][['mapped_templates']]
    if (length(temp) > 0){
      for (j in c(1 : length(temp))){
        idx_temp <- paste0(idx, j)
        value_dict <- json.values(temp[[j]])
        if (temp[[j]]['template'] == 'Demographic'){

        # Demographic -------------------------------------------------------------
          
          age <- input[[paste0('age_', idx_temp)]]
          sex <- input[[paste0('gdr_', idx_temp)]]
          race <- input[[paste0('race_', idx_temp)]]
          ethic <- input[[paste0('ethic_', idx_temp)]]
          
          if ('Age from ( include )' %in% names(value_dict)){
            if (age < value_dict[['Age from ( include )']]){
              flag <- 0
            }
          }
          
          if ('Age to ( include )' %in% names(value_dict)){
            if (age > value_dict[['Age to ( include )']]) {
              flag <- 0
            }
          }
          
          if ('Race is' %in% names(value_dict)){
            if (!(race %in% value_dict[['Race is']])){
              flag <- 0
            }
          }
          
          if ('Gender' %in% names(value_dict)){
            if (!(sex != value_dict[['Gender']])){
              flag <- 0
            }
          }
          
          if ('Ethic_Group is' %in% names(value_dict)) {
            if (!(ethic != value_dict[['Ethic_Group is']])){
              flag <- 0
            }
          }
        } else if (temp[[j]][['template']] == 'Condition by Diagnosis Code'){

          # Diagnosis ---------------------------------------------------------------

          if ('Diagnosis Code is' %in% names(value_dict)){
            icd <- input[[paste0('diag_is_', idx_temp)]]
            if (icd == 'None'){
              flag <- 0
            } else if (icd == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis code is undefined.'))
            }
          }
          
          if ('Diagnosis Code starts with' %in% names(value_dict)){
            icd_h <- input[[paste0('diag_like_', idx_temp)]]
            if (icd_h == 'None'){
              flag <- 0
            } else if (icd_h == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis code is undefined.'))
            }
          }
          
          if ("Diagnosis Description contains" %in% names(value_dict)){
            icd_d <- input[[paste0('diag_desc_', idx_temp)]]
            if (icd_d == 'None'){
              flag <- 0
            } else if (icd_d == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis description is not provided.'))
            }
          }
          
          if ("Diagnosis Type" %in$% names(value_dict)){
            icd_t <- input[['Diagnosis type']]
            if (icd_t == 'None'){
              flag <- 0
            } else if (icd_t == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis type is not provided'))
            }
          }
          
          if ("Time Period within" %in% names(value_dict)){
            icd_t <- input[['Time Period within']]
            if (icd_t == 'None'){
              flag <- 0
            } else if (icd_t == '') {
              undefined <- append(undefined, paste('Template', i, ': Diagnosis time period is not provided.'))
            } else{
              if (icd_t > value_dict[['Time Period within']]){
                flag <- 0
              }
            }
          }
          
        } else if (temp[[j]][['Template']] == 'Prescription'){

          # Drug --------------------------------------------------------------------

          
          if ("Drug Description contains" %in% names(value_dict)){
            dd <- input[[paste('drug_desc_', idx_temp)]]
            if (dd == 'None'){
              flag <- 0
            } else if (dd == ''){
              undefined <- append(undefined, paste('Template', i, ))
            }
          } else if ("Time Period within" %in% names(value_dict)) {
            dt <- input[[paste('Time Period within')]]
            if (dt == 'None'){
              flag <- 0
            } else if (dt == ''){
              undefined <- append(undefined, paste('Template', i, ': Drug prescription time period is unclear.'))
            } else {
              if (dt > value_dict[['Time Period within']]){
                 flag <- 0
              }
            }
          }
          
        } else if (temp == 'Event'){

          # Event -------------------------------------------------------------------

          
          event <- input[[paste('event_name_', idx_temp)]]
          if (event == 'None'){
            flag <- 0
          } else if (event == ''){
            return
          }
          
          if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
            value <- input[[paste0('event_val_', idx_temp)]]
            if (value == ''){
              if ('Value from ( include )' %in% names(value_dict)){
                if (value < value_dict[['value']]){
                  return(FALSE)
                }
              } else if ('Value from ( not include )' %in% names(value_dict)){
                if (value <= value_dict['Value from ( not include )']){
                  flag <- 0
                }
              }
              
              if ('Value to ( include )' %in% names(value_dict)) {
                if (value > value_dict[['Value to ( include )']]){
                  flag <- 0
                }
              } else if ('Value to ( not include )' %in% names(value_dict)){
                if (value >= value_dict[['Value to ( not include )']]){
                  flag <- 0
                }
              }
            }
          }
          
          if ('Time Period within' %in% names(value_dict)){
            et <- input[[paste0('event_time_', idx_temp)]]
            if (et == ''){
              undefined <- append(undefined, paste('Template', i, ': Event time is not clear.'))
            } else if (et > value_dict[['Time Period within']]){
              flag <- 0
            }
          }
        } else if (temp == 'Lab'){

          # Lab ---------------------------------------------------------------------
          
          l_nm <- value_dict[['Lab Name contains']]
          if (l_nm == 'None'){
            flag <- 0
          } else if (l_nm == ''){
            
          }
          
          if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
            value <- input[[paste0('lab_val_', idx_temp)]]
            if (value == ''){
              if ('Value from ( include )' %in% names(value_dict)){
                if (value < value_dict[['value']]){
                  return(FALSE)
                }
              } else if ('Value from ( not include )' %in% names(value_dict)){
                if (value <= value_dict['Value from ( not include )']){
                  return(FALSE)
                }
              }
              
              if ('Value to ( include )' %in% names(value_dict)) {
                if (value > value_dict[['Value to ( include )']]){
                  return(FALSE)
                }
              } else if ('Value to ( not include )' %in% names(value_dict)){
                if (value >= value_dict[['Value to ( not include )']]){
                  return(FALSE)
                }
              }
            }
          }
          
          if ('Time Period within' %in% names(value_dict)){
            lt <- input[[paste0('lab_time_', idx_temp)]]
            if (lt == ''){
              undefined <- append(undefined, paste('Template', i, ': Lab test time is not clear.'))
            } else if (lt > value_dict[['Time Period within']]) {
              flag <- 0
            }
          }
          
        } else if (temp == 'Order'){

          # Order -------------------------------------------------------------------
          on <- input[[paste0('order_name', idx_temp)]]
          if (on == ''){
            undefined <- append(undefined, paste('Template', i, ': Order name is not provided.'))
          }
          
          if ('Time Period within' %in% names(value_dict)){
            ot <- input[[paste0('order_time', idx_temp)]]
            if (ot > value_dict[['Time Period within']]){
              flag <- 0
            }
          }
          
        }
      }
    }
  }
  
  for (i in c(1 : n_e)){
    
    # Exclusion ---------------------------------------------------------------
    
    flag_lst <- vector()
    idx <- paste0('exclu_', i)
    temp <- standard[['exclusion']][[i]][['mapped_templates']]
    logic <- standard[['exclusion']][[i]][['']]
    if (length(temp) > 0){
      for (j in c(1 : length(temp))){
        
        idx_temp <- paste0(idx, j)
        value_dict <- json.values(temp[[j]])
        if (temp[[j]][['template']] == 'Demographic'){
          
          # Demographic -------------------------------------------------------------
          
          age <- input[[paste0('age_', idx_temp)]]
          sex <- input[[paste0('gdr_', idx_temp)]]
          race <- input[[paste0('race_', idx_temp)]]
          ethic <- input[[paste0('ethic_', idx_temp)]]
          
          if ('Age from ( include )' %in% names(value_dict)){
            if (age < value_dict[['Age from ( include )']]){
              flag_temp <- 0
            }
          } else if ('Age to ( include )'){
            if (age >= value_dict[['Age from ( not include )']]){
              flag_temp <- 0
            }
          }
          
          if ('Age to ( include )' %in% names(value_dict)){
            if (age < value_dict[['Age to ( include )']]) {
              flag_temp <- 1 * flag_temp
            }
          } else if ('Age to ( not include )'){
            if (age <= value_dict[['Age to ( include )']]){
              flag_temp <- 1 * flag_temp
            }
          }
          
          if ('Race is' %in% names(value_dict)){
            if (!(race %in% value_dict[['Race is']])){
              flag_temp <- 0
            }
          }
          
          if ('Gender' %in% names(value_dict)){
            if (!(sex != value_dict[['Gender']])){
              flag_temp <- 0
            }
          }
          
          if ('Ethic_Group is' %in% names(value_dict)) {
            if (!(ethic != value_dict[['Ethic_Group is']])){
              flag_temp <- 0
            }
          }
          
          flag_lst[j] <- flag_temp
          
        } else if (temp[[j]][['template']] == 'Condition by Diagnosis Code'){
          
          # Diagnosis ---------------------------------------------------------------
          
          if ('Diagnosis Code is' %in% names(value_dict)){
            icd <- input[[paste0('diag_is_', idx_temp)]]
            if (icd == 'None'){
              flag_temp <- 0
            } else if (icd == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis code is undefined.'))
            }
          }
          
          if ('Diagnosis Code starts with' %in% names(value_dict)){
            icd_h <- input[[paste0('diag_like_', idx_temp)]]
            if (icd_h == 'None'){
              flag_temp <- 0
            } else if (icd_h == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis code is undefined.'))
            }
          }
          
          if ("Diagnosis Description contains" %in% names(value_dict)){
            icd_d <- input[[paste0('diag_desc_', idx_temp)]]
            if (icd_d == 'None'){
              flag_temp <- 0
            } else if (icd_d == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis description is not provided.'))
            }
          }
          
          if ("Diagnosis Type" %in$% names(value_dict)){
            icd_t <- input[['Diagnosis type']]
            if (icd_t == 'None'){
              flag_temp <- 0
            } else if (icd_t == ''){
              undefined <- append(undefined, paste('Template', i, ': Diagnosis type is not provided'))
            }
          }
          
          if ("Time Period within" %in% names(value_dict)){
            icd_t <- input[['Time Period within']]
            if (icd_t == 'None'){
              flag_temp <- 0
            } else if (icd_t == '') {
              undefined <- append(undefined, paste('Template', i, ': Diagnosis time period is not provided.'))
            } else{
              if (icd_t > value_dict[['Time Period within']]){
                flag <- 0
              }
            }
          }
          
        } else if (temp[[j]][['Template']] == 'Prescription'){
          
          # Drug --------------------------------------------------------------------
          
          
          if ("Drug Description contains" %in% names(value_dict)){
            dd <- input[[paste('drug_desc_', idx_temp)]]
            if (dd == 'None'){
              flag <- 0
            } else if (dd == ''){
              undefined <- append(undefined, paste('Template', i, ))
            }
          } else if ("Time Period within" %in% names(value_dict)) {
            dt <- input[[paste('Time Period within')]]
            if (dt == 'None'){
              flag <- 0
            } else if (dt == ''){
              undefined <- append(undefined, paste('Template', i, ': Drug prescription time period is unclear.'))
            } else {
              if (dt > value_dict[['Time Period within']]){
                flag <- 0
              }
            }
          }
          
        } else if (temp == 'Event'){
          
          # Event -------------------------------------------------------------------
          
          
          event <- input[[paste('event_name_', idx_temp)]]
          if (event == 'None'){
            flag <- 0
          } else if (event == ''){
            return
          }
          
          if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
            value <- input[[paste0('event_val_', idx_temp)]]
            if (value == ''){
              if ('Value from ( include )' %in% names(value_dict)){
                if (value < value_dict[['value']]){
                  return(FALSE)
                }
              } else if ('Value from ( not include )' %in% names(value_dict)){
                if (value <= value_dict['Value from ( not include )']){
                  return(FALSE)
                }
              }
              
              if ('Value to ( include )' %in% names(value_dict)) {
                if (value > value_dict[['Value to ( include )']]){
                  return(FALSE)
                }
              } else if ('Value to ( not include )' %in% names(value_dict)){
                if (value >= value_dict[['Value to ( not include )']]){
                  return(FALSE)
                }
              }
            }
          }
          
          if ('Time Period within' %in% names(value_dict)){
            et <- input[[paste0('event_time_', idx_temp)]]
            if (et == ''){
              undefined <- append(undefined, paste('Template', i, ': Event time is not clear.'))
            } else if (et > value_dict[['Time Period within']]){
              flag <- 0
            }
          }
        } else if (temp == 'Lab'){
          
          # Lab ---------------------------------------------------------------------
          
          l_nm <- value_dict[['Lab Name contains']]
          if (l_nm == 'None'){
            flag <- 0
          } else if (l_nm == ''){
            
          }
          
          if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
            value <- input[[paste0('lab_val_', idx_temp)]]
            if (value == ''){
              if ('Value from ( include )' %in% names(value_dict)){
                if (value < value_dict[['value']]){
                  return(FALSE)
                }
              } else if ('Value from ( not include )' %in% names(value_dict)){
                if (value <= value_dict['Value from ( not include )']){
                  return(FALSE)
                }
              }
              
              if ('Value to ( include )' %in% names(value_dict)) {
                if (value > value_dict[['Value to ( include )']]){
                  return(FALSE)
                }
              } else if ('Value to ( not include )' %in% names(value_dict)){
                if (value >= value_dict[['Value to ( not include )']]){
                  return(FALSE)
                }
              }
            }
          }
          
          if ('Time Period within' %in% names(value_dict)){
            lt <- input[[paste0('lab_time_', idx_temp)]]
            if (lt == ''){
              undefined <- append(undefined, paste('Template', i, ': Lab test time is not clear.'))
            } else if (lt > value_dict[['Time Period within']]) {
              flag <- 0
            }
          }
          
        } else if (temp == 'Order'){
          
          # Order -------------------------------------------------------------------
          on <- input[[paste0('order_name', idx_temp)]]
          if (on == ''){
            undefined <- append(undefined, paste('Template', i, ': Order name is not provided.'))
          }
          
          if ('Time Period within' %in% names(value_dict)){
            ot <- input[[paste0('order_time', idx_temp)]]
            if (ot > value_dict[['Time Period within']]){
              flag <- 0
            }
          }
          
        }
      }
    }
  }
  
  return(TRUE)
}
