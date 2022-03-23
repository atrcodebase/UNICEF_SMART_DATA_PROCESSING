# Applying correction log
apply_log <- function(UNICEF_correction_log){
  for (rowi in 1:nrow(UNICEF_correction_log)){
    uuid_i <- UNICEF_correction_log$uuid[rowi]
    var_i <- UNICEF_correction_log$question[rowi]
    old_i <- UNICEF_correction_log$old_value[rowi]
    new_i <- UNICEF_correction_log$new_value[rowi]
    
    print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    #Find which sheet the variable belongs to
    if(var_i %in% colnames(main)){
      main[main$`_uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(hh_roster)){
      hh_roster[hh_roster$`_submission__uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(child)){
      child[child$uuid %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(preg_lact_wom)){
      preg_lact_wom[preg_lact_wom$uuid %in% uuid_i, var_i] <<- new_i
    }
  }
}

#Checks if correction is applied correctly
verify_log_changes <- function(raw_data, cleaned_data, identifier){
  uuid <- vector()
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  
  for(col_name in colnames(cleaned_data) ){
    
    for(i in 1:length(cleaned_data[[col_name]])) {
      id <- cleaned_data[[identifier]][i]
      oldVal <- raw_data[[col_name]][raw_data[[identifier]] %in% id]
      newVal <- cleaned_data[[col_name]][i]
      
      if(newVal %notin% oldVal){
        uuid <- c(uuid, id)
        question <- c(question, col_name)
        old_value <- c(old_value, oldVal)
        new_value <- c(new_value, newVal)
      }  
    }
  }
  log <- data.frame(uuid, question, old_value, new_value)
  return(log)
}

#Logs all the data points that are not translated
generate_translation_log <- function(data, sheet_i){
  question <- c()
  old_value <- c() 
  uuid <- c()
  sheet_name <- c()
  #checking each row of each column
  for(col_name in colnames(data)){
    for(i in 1:nrow(data)){
      cell_val <- data[[col_name]][i]
      
      if(is.character(cell_val)){
        if(Encoding(cell_val) %in% "UTF-8"){
          question <- c(question, col_name)
          old_value <- c(old_value, cell_val)
          sheet_name <- c(sheet_name, sheet_i)
          #because the uuid column name is different in sheets
          if(sheet_i %in% "ACO_SMART_Survey_2022_New_Round"){
            uuid <- c(uuid, data$`_uuid`[i])
          } else {
            uuid <- c(uuid, data$`_submission__uuid`[i])
          }
        }
      }
    }
  }
  log <- data.frame(question, old_value, new_value=NA, uuid, sheet_name, Remarks=NA) %>% unique()
}