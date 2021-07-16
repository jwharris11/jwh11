#### speedy_varnames function ####
speedy_varnames <- function(first_letter, 
                            first_number,
                            last_number){
  if(is.numeric(first_number) == F) stop("Must provide a numeric value for first_number")
  if(is.numeric(last_number) == F) stop("Must provide a numeric value for last_number")
  if(is.character(first_letter) == F) stop("Must provide a character value for first_letter")
  data <- paste('\"',first_letter, seq(from = first_number, to = last_number, by = 1),'\"', ",", sep = "")
  data <- noquote(data)
  cat(data)
}

speedy_varnames("Q", 1, 50)

is.numeric("X")

