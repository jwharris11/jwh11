#### New speedy_labels function ####
col_names <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33a", "Q33b", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50", "Q51", "Q52a", "Q52b", "Q53", "Q54", "Q55")

speedy_labels <- function(column_names){
  for (val in column_names) {
    line1 <- paste(column_names, 
                   " = ", 
                   '"QuestionTextHere"', 
                   ", ",
                   sep = "")  
    
    line2 <- paste(column_names,
                   '= num_lab("VALUES_LABELS"),')
    newcode <- paste(line1,line2, "\n", sep = "\n")
    newcode <- noquote(newcode)
    cat("YourDataHere <- apply_labels(YourDataHere,\n", newcode)
  }
}

speedy_labels(column_names)
