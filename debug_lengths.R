#### Length function ####
debug_lengths <- function(col_positions_input,
                          widths_input,
                          col_names_input) {
  col_positions_length = length(col_positions_input)
  widths_length = length(widths_input)
  col_names_length = length(col_names_input)
  data <- cbind(col_positions_length,
                widths_length,
                col_names_length)
  data <- as.data.frame(data)
  print(data)
  if(col_positions_length != widths_length |
     col_positions_length != col_names_length |
     widths_length != col_names_length) warning('The lengths of col_positions, widths, and col_names MUST be equal to run the read_rpr function')
}

debug_lengths(col_positions_input = c(1,2,7,10,13,17,18,19,20,21,22,23,26,27,28,
                                      seq(from = 30,
                                          to = 77,
                                          by = 1)),
              widths_input = c(1,5,3,3,4,1,1,1,1,1,1,3,1,1,1, rep(1,48)),
              col_names_input = c("A1", "Respondent no.", "A3", "A4", "A5",
                                  "A6", "A7", "A8", "A9", "A10",
                                  "A11", "A12", "A13", "A14", "A15",
                                  "Q1", "Q2","Q3","Q4",
                                  "Q5", "Q6", "Q7", "Q8",
                                  "Q9","Q10", "Q11", "Q12",
                                  "Q13", "Q14", "Q15", "Q16", "Q17",
                                  "Q18", "Q19", "Q20", "Q21", "Q22",
                                  "Q23", "Q24", "Q25", "Q26","Q27",
                                  "Q28", "Q29","Q30","Q31","Q32","Q33",
                                  "Q34","Q35","Q36","Q37","Q38","Q39",
                                  "Q40","Q41","Q42","Q43","Q44","Q45",
                                  "Q46","Q47","Q48"))
