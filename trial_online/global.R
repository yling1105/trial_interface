library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyTime)
library(rjson)


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

datalist <- lapply(filelst, function(x) read_json(x))


# Function used for generating forms given one criteria --------------------------------------

json2form <- function(trial){
  
}