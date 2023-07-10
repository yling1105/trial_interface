library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyTime)
library(rjson)
library(shinyWidgets)
library(shinyalert)

pat_lst <- list() # The original one is an empty list

# Read all the json files into the memory ---------------------------------

filelst <- list.files(path = './trial_json/', pattern = '*.json')
namelst <- list()
for (i in c(1:length(filelst))){
  temp_nm <- strsplit(filelst[[i]], '[.]')
  #print(temp_nm)
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
  #print(names(temp_dict))
  for (name in names(temp_dict)){
    if (length(temp_dict[[name]]) > 1){
      value_dict[[name]] <- temp_dict[[name]]
    } else if ((temp_dict[[name]] == '')){
      next
    } else {
      value_dict[[name]] <- temp_dict[[name]]
    }
  }
  value_dict
}

# Function used for generating forms given one criteria --------------------------------------

json2form <- function(value_dict,idx_temp){
  form <- tagList()
  temp <- value_dict[['template']]
  #print(value_dict)
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
        inputId = paste0('diag_type_', idx_temp), label = "Diagnosis type is:", 
        choices = choice_lst,
        selected = NULL,inline = TRUE, status = "danger"
      )
      form <- tagAppendChild(form, diag)
    }
    
    if ('Time period within' %in% value_dict){
      time <- value_dict[['Time period within']]
      diag <- prettySwitch(
        inputId = paste0('diag_time_', idx_temp),
        label = paste('Time period within', time),
        status = "success",
        fill = TRUE
      )
      form <- tagAppendChild(form, diag)
    }
    
  } else if (temp == 'Prescription'){

    # Drug --------------------------------------------------------------------

    
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
        inputId = paste0('lab_time', idx_temp),
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
  # } else if (temp == 'Encounter') {
  #   
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
  }
  form
}

# judgement function ------------------------------------------------------

judgement_func <- function(standard, input, output, session){
  n_i <- length(standard[['inclusion']])
  n_e <- length(standard[['exclusion']])
  undefined <- c()
  flag_lst <- c()
  
  pat_id <- input[['pat_id']]
  pat_lst[[str(pat_id)]] <- list()
  
  for (i in c(1:n_i)){

  # Inclusion ---------------------------------------------------------------
    
    print(paste("Inclu", i, '/', n_i))
    idx <- paste0('inclu_', i)
    temp <- standard[['inclusion']][[i]][['mapped_templates']]
    #print(length(temp))
    if (length(temp) > 0){
      
      flag_temp_lst <- rep(1, length(temp))
      logic <- standard[['inclusion']][[i]][['internal_logic']]
      #print(logic)
      if ((logic != 'AND') & (logic != 'OR')){
        #print(input[[paste0('txt_inclu_', idx)]])
        if (input[[paste0('txt_inclu_', idx)]]==FALSE){
          flag_lst[i] <- 0
          #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Not satisfied.'))
        } else {
          flag_lst[i] <- 1
        }
      } else {
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
              if (age == ''){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Age information is missing.'))
              }else if (age < value_dict[['Age from ( include )']]){
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Age is unqualified.'))
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Age to ( include )' %in% names(value_dict)){
              if (age == ''){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Age information is missing.'))
              } else if (age > value_dict[['Age to ( include )']]) {
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Age is unqualified.'))
              }
            }
            
            if ('Race is' %in% names(value_dict)){
              if (race == 'Unknown'){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Race information is missing.'))
              } else if (!(race %in% value_dict[['Race is']])){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Race is not satified with the trial. '))
              }
            }
            
            if ('Gender' %in% names(value_dict)){
              if (sex == 'Unknown'){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Gender information is missing.'))
              } else if (!(sex != value_dict[['Gender']])){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Gender is not satified for the trial'))
              }
            }
            
            if ('Ethic_Group is' %in% names(value_dict)) {
              if (ethic == ''){
                
                flag_temp_lst[j] <- 2
              } else if (!(ethic != value_dict[['Ethic_Group is']])){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Condition by Diagnosis Code'){
            
            # Diagnosis ---------------------------------------------------------------
            
            
            if ('Diagnosis Code is' %in% names(value_dict)){
              icd <- input[[paste0('diag_is_', idx_temp)]]
              if (is.null(icd)){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Diagnosis code is undefined.'))
                flag_temp_lst[j] <- 2
              }else if (icd == 'None'){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Diagnosis is not satified for the trial'))
              }
            }
            
            if ('Diagnosis Code starts with' %in% names(value_dict)){
              icd_h <- input[[paste0('diag_like_', idx_temp)]]
              if (is.null(icd_h)){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Diagnosis code is undefined.'))
                flag_temp_lst[j] <- 2
              }else if (icd_h == 'None'){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Diagnosis is not satified for the trial'))
              }
            }
            
            if ("Diagnosis Description contains" %in% names(value_dict)){
              icd_d <- input[[paste0('diag_desc_', idx_temp)]]
              if (is.null(icd_d)){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Diagnosis code is undefined.'))
                flag_temp_lst[j] <- 2
              }else if (icd_d == 'None'){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Diagnosis is not satified for the trial'))
              }
            }
            
            if ("Diagnosis Type" %in% names(value_dict)){
              icd_t <- input[[paste0('diag_type_', idx_temp)]]
              if (is.null(icd_t)){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Diagnosis type is not provided'))
                flag_temp_lst[j] <- 2
              } else if (icd_t == 'None'){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Diagnosis is not satified for the trial'))
              }
            }
            
            if ("Time Period within" %in% names(value_dict)){
              icd_t <- input[[paste0('diag_time_', idx_temp)]]
              if (icd_t == FALSE){
                flag_temp_lst[j] <- 0
                #unqualified <- append(unqualified, paste('Inclusion criteria', i, ': Diagnosis time is not satified for the trial'))
              }
            }
            
          } else if (temp[[j]][['template']] == 'Prescription'){
            
            # Drug --------------------------------------------------------------------
            
            if ("Drug Description contains" %in% names(value_dict)){
              dd <- input[[paste('drug_desc_', idx_temp)]]
              if (is.null(dd)){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria', i, ": Drug description is missing."))
              }
              if (dd == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ("Time Period within" %in% names(value_dict)) {
              dt <- input[[paste('drug_time_', idx_temp)]]
              if (dt == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Event'){
            
            # Event -------------------------------------------------------------------
            
            event <- input[[paste0('event_desc_', idx_temp)]]
            
            if (is.null(event)){
              undefined <- append(undefined, paste('Inclusion criteria', i, ": Event name is missing."))
              flag_temp_lst[j] <- 2
            } else if (event == 'None'){
              flag_temp_lst[j] <- 0
            } 
            
              
            if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
              value <- input[[paste0('event_val_', idx_temp)]]
              
              if (value == ''){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Event value is missing.'))
                flag_temp_lst[j] <- 2
              } else {
                value <- as.double(value)
                if ('Value from ( include )' %in% names(value_dict)){
                  if (value < as.double(value_dict[['Value from ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value from ( not include )' %in% names(value_dict)){
                  if (value <= as.double(value_dict['Value from ( not include )'])){
                    flag_temp_lst[j] <- 0
                  }
                }
                
                if ('Value to ( include )' %in% names(value_dict)) {
                  if (value > as.double(value_dict[['Value to ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value to ( not include )' %in% names(value_dict)){
                  if (value >= as.double(value_dict[['Value to ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
              }
            }
            
            
            
            if ('Time Period within' %in% names(value_dict)){
              et <- input[[paste0('event_time_', idx_temp)]]
              if (et == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
          } else if (temp[[j]][['template']] == 'Lab'){
            
            # Lab ---------------------------------------------------------------------
            if ('Lab Name contains' %in% value_dict){
              l_nm <- input[[paste0('lab_desc_', idx_temp)]]
              if (is.null(l_nm)){
                flag_temp_lst[j] <- 2
                undefined <- append(undefined, paste('Inclusion criteria:', i, 'Lab name is missing.'))
              } else if (l_nm == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
              value <- input[[paste0('lab_val_', idx_temp)]]
              if (value == ''){
                undefined <- append(undefined, paste('Inclusion criteria', i, ': Lab value is missing.'))
                flag_temp_lst[j] <- 2
              }else {
                value <- as.double(value)
                if ('Value from ( include )' %in% names(value_dict)){
                  if (value < as.double(value_dict[['Value from ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value from ( not include )' %in% names(value_dict)){
                  if (value <= as.double(value_dict['Value from ( not include )'])){
                    flag_temp_lst[j] <- 0
                  }
                }
                
                if ('Value to ( include )' %in% names(value_dict)) {
                  if (value > as.double(value_dict[['Value to ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value to ( not include )' %in% names(value_dict)){
                  if (value >= as.double(value_dict[['Value to ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
              }
            }
            
            if ('Time Period within' %in% names(value_dict)){
              lt <- input[[paste0('lab_time_', idx_temp)]]
              if (lt == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Order'){
            
            # Order -------------------------------------------------------------------
            on <- input[[paste0('order_name_', idx_temp)]]
            if (is.null(on)){
              undefined <- append(undefined, paste('Inclusion criteria', i, ': Order name is not provided.'))
              flag_temp_lst[j] <- 2
            } else if (on == 'None'){
              flag_temp_lst[j] <- 0
            }
            
            if ('Time Period within' %in% names(value_dict)){
              ot <- input[[paste0('order_time', idx_temp)]]
              if (ot == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
          }else if (temp[[j]][['Template']] == 'Encounter'){
            next
          }
        }
      }
      print(paste('flag_temp_lst:',flag_temp_lst))
      if (logic  == 'AND'){
        judge_flag <- 1
        for (j in c(1: length(temp))){
          judge_flag <- judge_flag * flag_temp_lst[j]
        }
        flag_lst[i] <- judge_flag
      } else {
        if (1 %in% flag_temp_lst){
          flag_lst[i] <- 1
        } else if (2 %in% flag_temp_lst){
          flag_lst[i] <- 2
        } else {
          flag_lst[i] <- 0
        }
      }
    } else {
      flag <- input[[paste0('txt_inclu_', idx)]]
      if (flag == FALSE){
        flag_lst[i] <- 0
      } else {
        flag_lst[i] <- 1
      }
    }
    #print(paste('flag_lst:', flag_lst))
  }
  
  for (i in c(1 : n_e)){
    
    # Exclusion ---------------------------------------------------------------
    idx <- paste0('exclu_', i)
    temp <- standard[['exclusion']][[i]][['mapped_templates']]
    print(paste('Exclu', i, '/', n_e))
    if (length(temp) > 0){
      flag_temp_lst <- rep(1, length(temp))
      logic <- standard[['exclusion']][[i]][['internal_logic']]
      print(logic)
      if ((logic != 'AND') & (logic != 'OR')){
        if (input[[paste0('txt_exclu_', idx)]]==FALSE){
          flag_lst[i+n_i] <- 1
        } else {
          flag_lst[i+n_i] <- 0
        }
      } else{
        for (j in c(1 : length(temp))){
          
          idx_temp <- paste0(idx, j)
          value_dict <- json.values(temp[[j]])
          
          print(temp[[j]][['template']])
          #print(value_dict)
          if (temp[[j]]['template'] == 'Demographic'){
            
            # Demographic -------------------------------------------------------------
            
            age <- input[[paste0('age_', idx_temp)]]
            age <- as.integer(age)
            sex <- input[[paste0('gdr_', idx_temp)]]
            race <- input[[paste0('race_', idx_temp)]]
            ethic <- input[[paste0('ethic_', idx_temp)]]
            
            if ('Age from ( include )' %in% names(value_dict)){
              if (age == ''){
                flag_temp_lst[j] <- 0
              }else if (age < as.integer(value_dict[['Age from ( include )']])){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Age to ( include )' %in% names(value_dict)){
              if (age == ''){
                flag_temp_lst[j] <- 0
              } else if (age > as.integer(value_dict[['Age to ( include )']])) {
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Race is' %in% names(value_dict)){
              if (race == 'Unknown'){
                flag_temp_lst[j] <- 0
              } else if (!(race %in% value_dict[['Race is']])){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Gender' %in% names(value_dict)){
              if (sex == 'Unknown'){
                flag_temp_lst[j] <- 0
              } else if (!(sex != value_dict[['Gender']])){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Ethic_Group is' %in% names(value_dict)) {
              if (ethic == ''){
                flag_temp_lst[j] <- 0
              } else if (!(ethic != value_dict[['Ethic_Group is']])){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Condition by Diagnosis Code'){
            
            # Diagnosis ---------------------------------------------------------------
            
            
            if ('Diagnosis Code is' %in% names(value_dict)){
              icd <- input[[paste0('diag_is_', idx_temp)]]
              if (is.null(icd)){
                flag_temp_lst[j] <- 0
              }else if (icd == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Diagnosis Code starts with' %in% names(value_dict)){
              icd_h <- input[[paste0('diag_like_', idx_temp)]]
              if (is.null(icd_h)){
                flag_temp_lst[j] <- 0
              }else if (icd_h == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ("Diagnosis Description contains" %in% names(value_dict)){
              icd_d <- input[[paste0('diag_desc_', idx_temp)]]
              if (is.null(icd_d)){
                flag_temp_lst[j] <- 0
              }else if (icd_d == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ("Diagnosis Type" %in% names(value_dict)){
              icd_t <- input[['Diagnosis type']]
              if (is.null(icd_t)){
                flag_temp_lst[j] <- 0
              } else if (icd_t == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ("Time Period within" %in% names(value_dict)){
              icd_t <- input[['Time Period within']]
              if (icd_t == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Prescription'){
            
            # Drug --------------------------------------------------------------------
            
            if ("Drug Description contains" %in% names(value_dict)){
              dd <- input[[paste('drug_desc_', idx_temp)]]
              if (dd == 'None'){
                flag_temp_lst[j] <- 0
              } else if (dd == ''){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ("Time Period within" %in% names(value_dict)) {
              dt <- input[[paste('Time Period within')]]
              if (dt == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Event'){
            
            # Event -------------------------------------------------------------------
            
            
            event <- input[[paste('event_desc_', idx_temp)]]
            if (is.null(event)){
              flag_temp_lst[j] <- 0
            }else if (event == 'None'){
              flag_temp_lst[j] <- 0
            }
            
            if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
              value <- input[[paste0('event_val_', idx_temp)]]
              
              if (value == ''){
                #undefined <- append(undefined, paste('Exclusion criteria', i, ': Lab value is missing.'))
                flag_temp_lst[j] <- 0
              } else {
                value <- as.double(value)
                if ('Value from ( include )' %in% names(value_dict)){
                  if (value < as.double(value_dict[['Value from ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value from ( not include )' %in% names(value_dict)){
                  if (value <= as.double(value_dict[['Value from ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
                
                if ('Value to ( include )' %in% names(value_dict)) {
                  if (value > as.double(value_dict[['Value to ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value to ( not include )' %in% names(value_dict)){
                  if (value >= as.double(value_dict[['Value to ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
              }
            }
            
            
            
            if ('Time Period within' %in% names(value_dict)){
              et <- input[[paste0('event_time_', idx_temp)]]
              if (et == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
          } else if (temp[[j]][['template']] == 'Lab'){
            print(value_dict)
            # Lab ---------------------------------------------------------------------
            if ('Lab Name contains' %in% names(value_dict)){
              l_nm <- input[[paste0('lab_desc_',idx_temp)]]
              if (is.null(l_nm)){
                flag_temp_lst[j] <- 0
                #undefined <- append(undefined, paste('Exclusion criteria:', i, 'Lab name is missing.'))
              } else if (l_nm == 'None'){
                flag_temp_lst[j] <- 0
              }
            }
            
            if ('Value from ( include )' %in% names(value_dict) | 'Value from ( not include )' %in% names(value_dict) | 'Value to ( include )' %in% names(value_dict) | 'Value to ( not include )' %in% names(value_dict)){
              value <- input[[paste0('lab_val_', idx_temp)]]
              
              if (value == ''){
                #undefined <- append(undefined, paste('Exclusion criteria', i, ': Lab value is missing.'))
                flag_temp_lst[j] <- 0
              } else {
                value = as.double(value)
                if ('Value from ( include )' %in% names(value_dict)){
                  if (value < as.double(value_dict[['Value from ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value from ( not include )' %in% names(value_dict)){
                  if (value <= as.double(value_dict[['Value from ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
                
                if ('Value to ( include )' %in% names(value_dict)) {
                  if (value > as.double(value_dict[['Value to ( include )']])){
                    flag_temp_lst[j] <- 0
                  }
                } else if ('Value to ( not include )' %in% names(value_dict)){
                  if (value >= as.double(value_dict[['Value to ( not include )']])){
                    flag_temp_lst[j] <- 0
                  }
                }
              }
            }
            
            if ('Time Period within' %in% names(value_dict)){
              lt <- input[[paste0('lab_time_', idx_temp)]]
              if (lt == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
            
          } else if (temp[[j]][['template']] == 'Order'){
            
            # Order -------------------------------------------------------------------
            on <- input[[paste0('order_name', idx_temp)]]
            if (is.null(on)){
              #undefined <- append(undefined, paste('Exclusion criteria', i, ': Order name is not provided.'))
              flag_temp_lst[j] <- 0
            } else if (on == 'None'){
              flag_temp_lst[j] <- 0
            }
            
            if ('Time Period within' %in% names(value_dict)){
              ot <- input[[paste0('order_time', idx_temp)]]
              if (ot == FALSE){
                flag_temp_lst[j] <- 0
              }
            }
          } else if (temp[[j]][['template']] == 'Encounter'){
            next
          }
        }
        print(flag_temp_lst)
        if (logic == 'AND'){
          if (0 %in% flag_temp_lst){
            judge_flag <- 1
          } else if (2 %in% flag_temp_lst){
            judge_flag <- 2
          } else {
            judge_flag <- 0
          }
          flag_lst[i+n_i] <- judge_flag
          
        } else {
          if (1 %in% flag_temp_lst){
            flag_lst[i+n_i] <- 0
          } else if (2 %in% flag_temp_lst){
            flag_lst[i+n_i] <- 2
          } else {
            flag_lst[i+n_i] <- 1
          }
        }
      }
    } else {
      flag <- input[[paste0('txt_exclu_',idx)]]
      if (flag == FALSE){
        flag_lst[i+n_i] <- 1
      } else {
        flag_lst[i+n_i] <- 0 
      }
    }
    #print(paste('flag_lst:', flag_lst))
  }
  
  pat_lst[[str(pat_id)]] <- flag_lst
  
  final <- 1
  print(flag_lst)
  unqualified <- c()
  for (i in c(1:length(flag_lst))){
    if (i <= n_i){
      if (flag_lst[i] == 0){
        unqualified <- append(unqualified, paste("Inclusion criteria", i, 'is not satisfied.'))
      }
    } else {
      if (flag_lst[i] == 0){
        unqualified <- append(unqualified, paste('The patient is excluded by Exclusion criteria', i-n_i))
      }
    }
    
    final <- final * flag_lst[i]
  }
  print(final)
  print(undefined)
  res_lst <- list("flag" = final, "undefined" = undefined, "unqualified"=unqualified, "flag_list"=flag_lst)
  return(res_lst)
}



