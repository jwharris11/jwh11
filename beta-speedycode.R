#### Set up ####
remove(list = ls())
#### speedy_rename ####
speedy_rename <- function(varnames, df_name = "df", save_to_file = F, path = "") {
  line1 <- paste("df_name"," <- ", "df_name", " %>%", sep = "")
  line2a <- "rename("
  line2b <- paste('NEW_NAME', " = ", colnames(varnames[1]), ",",  sep = "")
  line3 <- paste('NEW_NAME', " = ", colnames(varnames[c(-1, -length(varnames))]), ",", sep = "")
  line4 <- paste('NEW_NAME', " = ", colnames(varnames[length(varnames)]), "\n", ")", sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(save_to_file == T & final_path_char != ".R") {
    warning('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }
  
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)
  if(save_to_file == T) {
    capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
  }
}

#### speedy_classes ####
speedy_classes <- function(data, save_to_file = F, path) {
  class_list <- unlist(lapply(data, class))
  class_list <- as.data.frame(class_list)
  line1 <- paste("data"," <- ", "data", " %>%", sep = "")
  line2a <- "mutate("
  line2b <- paste(colnames(data[1]), " = ", "as.", class_list[1,], "(", colnames(data[1]), ")", ",",  sep = "")
  line3 <- paste(colnames(data[c(-1, -length(data))]), " = ", "as.", class_list[2:(nrow(class_list)-1),], "(", colnames(data[c(-1, -length(data))]), ")", ",", sep = "")
  line4 <- paste(colnames(data[length(data)]), " = ", "as.", class_list[nrow(class_list),], "(", colnames(data[length(data)]), ")\n)", sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)
  if(save_to_file == T) {
    capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
  }
}

#### speedy_labels ####
speedy_labels <- function(column_names, df_name = "df", max_values = 5, questions_values = "both", save_to_file = F, path = "") {
  qline1 <- paste("df_name"," <- ", "df_name", " %>%", sep = "")
  qline2a <- "set_variable_labels("
  qline2b <- paste(column_names[1], " = ", '"QTEXT_HERE",', sep = "")
  qline3 <- paste(column_names[c(-1, -length(column_names))], " = ", '"QTEXT_HERE"', ",", sep = "")
  qline4 <- paste(column_names[length(column_names)], " = ", '"QTEXT_HERE"', ")", sep = "")
  qnewcode <- paste(qline3, "\n", sep = "")
  
  line1 <- paste("df_name"," <- ", "df_name", " %>%", sep = "")
  line2a <- "set_value_labels("
  line2b <- paste(column_names[1], " = ", 'c("NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345)', ",", sep = "")
  # extract the unique values from each variable and assign it to a number
  line3 <- paste(column_names[c(-1, -length(column_names))], " = ", 'c("NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345)', "," , sep = "")
  line4 <- paste(column_names[length(column_names)], " = ", 'c("NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345))', sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  if(is.character(column_names) == F) {
    stop("column_names requires a vector of character values")
  }
  
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(save_to_file == T & final_path_char != ".R") {
    warning('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }
  
  # if(questions_values != "both" |
  #    questions_values != "values" |
  #    questions_values != "questions") {
  #   stop("Must choose to display questions, values, or both. Default is both.")
  # }
  
  ## Pasting the code ##
  if(questions_values == "both") {
    cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4, " %>% ", "\n", "\n", line2a, "\n", line2b, "\n", newcode, line4)
    if(save_to_file == T) {
      capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4, " %>% ", "\n", "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
    }
  }
  else if(questions_values == "questions") {
    cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4)
    if(save_to_file == T) {
      capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4), file = path)
    }
  }
  else if(questions_values == "values") {
    cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)
    if(save_to_file == T) {
      capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
    }
  }
  else {
    stop("Must choose to display questions, values, or both. Default is both.")
  }
}
