#### Set up ####
remove(list = ls())
#### speedy_names ####
speedy_names <- function(varnames, df_name = "df", save_to_file = F, path) {
  line1 <- paste(df_name," <- ", df_name, " %>%", sep = "")
  line2a <- "rename("
  line2b <- paste('"NEW_NAME"', " = ", varnames[1], ",",  sep = "")
  line3 <- paste('"NEW_NAME"', " = ", varnames[c(-1, -length(varnames))], ",", sep = "")
  line4 <- paste('"NEW_NAME"', " = ", varnames[length(varnames)], ")", sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  if(is.character(varnames) == F) {
    stop("column_names requires a vector of character values")
  }
  
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(save_to_file == T & final_path_char != ".R") {
    warning('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }
  
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)
  if(save_to_file == T) {
    capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
  }
}

speedy_names(names(cars), save_to_file = T, path = "~/Desktop/Test4.R")

#### speedy_labels ####
speedy_labels <- function(column_names, df_name = "df", questions_values = "both", save_to_file = F, path) {
  qline1 <- paste(df_name," <- ", df_name, " %>%", sep = "")
  qline2a <- "set_variable_labels("
  qline2b <- paste(column_names[1], " = ", '"QTEXT_HERE",', sep = "")
  qline3 <- paste(column_names[c(-1, -length(column_names))], " = ", '"QTEXT_HERE"', ",", sep = "")
  qline4 <- paste(column_names[length(column_names)], " = ", '"QTEXT_HERE"', ")", sep = "")
  qnewcode <- paste(qline3, "\n", sep = "")
  
  line1 <- paste(df_name," <- ", df_name, " %>%", sep = "")
  line2a <- "set_value_labels("
  line2b <- paste(column_names[1], " = ", 'c("NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345,\n"NEW_LABEL" = 12345)', ",", sep = "")
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

speedy_labels(column_names = c("a", "b", "c"), questions_values = "both", save_to_file = F, path = "~/Desktop/test2.R")

#### speedy_mutating ####
speedy_mutate <- function(varnames, max_repeats = 5, df_name = "df", save_to_file = F, path) {
  library(dplyr)
  n_repeats <- rapply(cars,function(x)length(unique(x)))
  n_repeats <- as.data.frame(n_repeats)
  n_repeats <- n_repeats %>% 
    dplyr::mutate(n_repeats = as.numeric(n_repeats),
           n_repeats = case_when(n_repeats > 5 ~ 5,
                                 T ~ n_repeats))
  
  line1 <- paste("df_name"," <- ", "df_name", " %>%", sep = "")
  
  line2a <- paste("mutate(NEW_VARNAME", " = ", "case_when(", varnames[1], " == ", "VALUE", " ~ ", "NEW_VALUE", ",", sep = "")
  
  line2b1 <-  paste(replicate((n_repeats[1,] - 2), paste(varnames[1], " == ", "VALUE", " ~ ", "NEW_VALUE", ",", "\n",  sep = "")))
  
  line2b2 <- paste(varnames[1], " == ", "VALUE", " ~ ", "NEW_VALUE", "),",  sep = "")
  
  line3 <-  paste(replicate((n_repeats[1,] - 2), paste("NEW_VARNAME", " = ", "case_when(", varnames[c(-1, -length(varnames))], " == ", "VALUE", " ~ ", "NEW_VALUE)", ",", sep = "")))
  
  # line3 <- paste("NEW_VARNAME", " = ", "case_when(", varnames[c(-1, -length(varnames))], " == ", "VALUE", " ~ ", "NEW_VALUE)", ",", sep = "")
  
  line4 <- paste("NEW_VARNAME", " = ", "case_when(", varnames[length(varnames)], " == ", "VALUE", " ~ ", "NEW_VALUE))", sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  
  cat(line1, "\n", line2a, "\n", line2b1, line2b2, "\n", line3)

  # cat(line1, "\n", line2a, "\n", line2b, "\n", line2b1, "\n", line2b2, "\n", newcode, line4)
  
  
  if(save_to_file == T) {
    capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", line2b1, "\n", newcode, line4), file = path)
  }
}

speedy_mutate(c("mpg", "cyl", "vs", "am"), df_name = cars)

#### speedy_classes ####
speedy_classes <- function(varnames, df_name = "df", save_to_file = F, path) {
  line1 <- paste(df_name," <- ", df_name, " %>%", sep = "")
  line2a <- "mutate("
  line2b <- paste(varnames[1], " = ", "as.CLINC(", varnames[1], ")", ",",  sep = "")
  line3 <- paste(varnames[c(-1, -length(varnames))], " = ", "as.CLINC(", varnames[c(-1, -length(varnames))], ")", ",", sep = "")
  line4 <- paste(varnames[length(varnames)], " = ", "as.CLINC(", varnames[length(varnames)], "))", sep = "")
  newcode <- paste(line3, "\n", sep = "")
  
  if(is.character(varnames) == F) {
    stop("column_names requires a vector of character values")
  }
  
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)
  if(save_to_file == T) {
    capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path)
  }
}

speedy_classes(c("a","b", "c"))






