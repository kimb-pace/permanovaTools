#' Generate Output Matrix for Deriving Expected Mean Squares    

#' Derive EMS Input Matrix 
#'
#' This function assists with constructing the matrix used to derive the Expected Mean Squares (EMS) for custom (? come up with better word choice here) models. 
#' The resulting output matrix includes the fixed or random status of the variable, its number of levels, and the subscript associated with it. 
#' 
#' Constructs an input matrix used to derive Expected Mean Squares (EMS) for custom mixed-effects models. The matrix contains 
#' information about each model term's fixed/random status, number of levels, and subscript structure and how that determines the expected variance to be partitioned.
#' 
#' The matrix serves as a foundational input for subsequent EMS calculations using the \code{derive_ems} function from this package.
#'
#' @param terms This is a list of model term objects. Each object is a named list with the following elements: 
#'      \describe{
#'      \item{label}{Character. The label for the term ie Park = P. "Error" is the name for the error term.}
#'      \item{name}{Character. A common name for the term, ie Park}
#'      \item{type}{Character. Designate each term as "Fixed" or "Random"}
#'      \item{levels}{Character or numeric. The number of levels associated with each term, can be a number or letter}
#'      \item{main_subscripts}{Character. Subscripts directly associated with the term}
#'      \item{determinant_subscripts}{Character. Subscripts that are associated with nesting for a term}}
#'      
#' @return A character matrix where rows have metadata regarding the terms from the input data (fixed or random, levels, subscripts) 
#' followed by rows for each term with associated 0 and 1 entries based on parameters. 
#' \itemize{
#' \item{"Fixed or Random":} Indicates whether each term is fixed (F) or random (R) effect. 
#' \item{Number of Levels":} Number of levels for each subscript, represented by a letter.
#' \item{"Subscript":} The subscript names.
#' \item{"Rows per Term":} Entries are "0" if the term includes the subscript and is fixed, "1" if random, and the number of levels otherwise.}
#' 
#' @details 
#' This function is the first part of a two step process to derive the expected mean variance of a custom model. It builds an input matrix that is then 
#' used by the derive_ems function from this package to complete the process. The generated matrix encodes the model structure for interpretation of the derive_ems function. 
#' 
#' @examples
#' 
#' #no nesting 
#' terms <- list(
#'    list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
#'    list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
#'    list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "c"),
#'    list(name = "Esik", label = "Residual", subscripts = c("s", "i", "k"), type = "random", levels = "abc")))
#' 
#' derive_matrix(terms)
#'
#'#nesting 
#'terms <- list(
#'list(name = "Vs", label = "Viereck", main_subscripts = c("s"), determinant_subscripts = character(0), type = "fixed", levels = "a"),
#'list(name = "Pi", label = "Park", main_subscripts = c("i"), determinant_subscripts = character(0), type = "fixed", levels = "b"),
#'list(name = "Tk", label = "Time", main_subscripts = c("k"), determinant_subscripts = character(0), type = "random", levels = "c"),
#'list(name = "Rj(si)", label = "Plot", main_subscripts = c("j"), determinant_subscripts = c("s", "i"), type = "random", levels = "d"),
#'list(name = "Esik", label = "Error", main_subscripts = c("s", "i", "k"), determinant_subscripts = character(0), type = "random", levels = "abc"))
#'
#'derive_matrix(terms)
#'
#' @export 
#'
#' @details 
#' This function builds an input matrix to help derive Expected Mean Squares (EMS) following the derivation method described 
#' in Topic 10 Supplement from the University of New Hampshire course ANFS933. For detailed instructions on how to do this by hand or 
#' to further understand the mechanics behind this function, see: 
#' \url{insert bib citation once you figure that out}

#function start 
derive_matrix <- function(terms) {
  is_error <- sapply(terms, function(x) x$label == "Error") #first identify the error term in the input 
  
  #collect all subscripts from main and determinant 
  all_subscripts <- unique(unlist(lapply(terms[!is_error], function(x) {
    c(x$main_subscripts, x$determinant_subscripts)})))
  
  #assign fixed or random based on input 
  
  #determine if each subscript is fixed or random 
  fixed_or_random <- character(length(all_subscripts))
  names(fixed_or_random) <- all_subscripts
  for (s in all_subscripts) {
    for (term in terms) {
      if (term$label != "Error" && s %in% term$main_subscripts, term$determinant_subscripts)) {
        fixed_or_random[s] <- if (term$type == "fixed") "F" else "R"
        break}}}
  
  #get levels 
  levels_row <- character(length(all_subscripts))
  names(levels_row) <- all_subscripts
  for (s in all_subscripts) {
    for(term in terms) {
      if(term$label != error && s %in% c(term$main_subscripts, term$determinant_subscripts)) {
        levels_row[s] <- term$levels 
        break}}}
  
  #get the subscript names row 
  subscripts_row <- subscripts 
  
  #get the term names 
  term_names <- sapply(terms, function(x) x$name)
  
  #make the matrix
  output_matrix <- matrix("",
                          nrow = 3 + length(terms),
                          ncol = length(all_subscripts),
                          dimnames = list(
                            c("Fixed or Random", "Number of Levels", "Subscript", term_names)))
  #fill header rows 
  output_matrix["Fixed or Random", ] <- fixed_or_random 
  output_matrix["Number of Levels", ] <- levels_row 
  output_matrix["subscript"] <- all_subscripts 
  
  #fill in the cells of the matrix 
  subscript_type <- ifelse(fixed_or_random == "F", 0, 1)
  for (term in terms) {
    for (s in all_subscripts) {
      if s %in% term$main_subscripts) {
        output_matrix[row, s] <- "0"
      } else if (s %in% term$determinant_subscripts) {
        output_matrix[row, s] <- "1"}
  } else {
    output_matrix{term$name, s] <- levels_row[s]}}}
  
  return(output_matrix)
  }


#TO DO: 
#1. add in nesting to function script to accommodate more complex models
#2. finish roxygen documentation 
#3. remove loose script once function is complete 
#4. clean up final function script without all unnecessary dev comments 

#goal: 
Fixed or random:      F   F   R 
Number of levels:     a   b   c
Subscript:            s   i   k 
Vs 
Pi 
Tk 
Esik

#test calls 

#first make the output matrix with a simple model: Vs + Pi + Tk + Esik 

terms <- list(
  list(name = "Vs", label = "Viereck", subscripts = c("s"), type = "fixed", levels = "a"),
  list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
  list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "c"),
  list(name = "Esik", label = "Error", subscripts = c("s", "i", "k"), type = "random", levels = "abc"))

terms <- list(
  list(name = "Vs", label = "Viereck", main_subscripts = c("s"), determinant_subscripts = character(0), type = "fixed", levels = "a"),
  list(name = "Pi", label = "Park", main_subscripts = c("i"), determinant_subscripts = character(0), type = "fixed", levels = "b"),
  list(name = "Tk", label = "Time", main_subscripts = c("k"), determinant_subscripts = character(0), type = "random", levels = "c"),
  list(name = "Rj(si)", label = "Plot", main_subscripts = c("j"), determinant_subscripts = c("s", "i"), type = "random", levels = "d"),
  list(name = "Esik", label = "Error", main_subscripts = c("s", "i", "k"), determinant_subscripts = character(0), type = "random", levels = "abc"))

#draft input for complex model 
terms <- list(
  list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
  list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
  list(name = "X(si)j", label = "Plot", subscripts = c("j", "(s,i)"), type = "random", levels = "(ab)c"),
  list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "d"),
  list(name = "(VT)sk", label = "Interaction", subscripts = c("s", "k"), type = "random", levels = "ad"),
  list(name = "Esijk", label = "Residual", subscripts = c("s", "i", "j", "k"), type = "random", levels = "abcd"))
#NOT ready for this biz ^^^^ 

output_matrix <- derive_matrix(terms)
