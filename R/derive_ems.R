 #derive EMS function documentation 

#' Calculate Expected Mean Squares (EMS) from Output Matrix
#'
#' This function derives the partitioned variance for the Expected Mean Squares (EMS)
#' associated with each term in a model, using a matrix representation of 
#' subscripts, nesting, and factor types that is generated using the \code{generate_ems_matrix} function. 
#'
#' @param output_matrix A matrix where rows represent model terms,
#' and columns represent subscripts. The top three rows include:
#'   - "Fixed or random": indicates whether each model term is fixed ("F") or random ("R")
#'   - "Number of levels": the number of levels associated with each subscript, represented as a letter variable different from the subscript letter
#'   - "Subscript": the subscript associated with each term, representing the number of groups associated with the associated term 
#' @param terms A list where each element is a list containing:
#'   - \code{name}: the name of the term as it appears in the output matrix
#'   - \code{type}: either "fixed" or "random"
#' @return A named list of EMS expressions, one for each term in the model. 
#' @examples
#' \dontrun{
#' # Example output_matrix and terms
#' output_matrix <- matrix(c(
#'   "F", "F", "R",
#'   "a", "b", "c",
#'   "s", "i", "k",
#'   "0", "b", "c",
#'   "a", "0", "c",
#'   "a", "b", "1",
#'   "0", "0", "1"
#' ), nrow = 7, byrow = TRUE)
#' rownames(output_matrix) <- c("Fixed or random", "Number of levels", "Subscript",
#'                              "Vs", "Pi", "Tk", "Esik")
#' colnames(output_matrix) <- c("s", "i", "k")
#' terms <- list(
#'   list(name = "Vs", type = "fixed"),
#'   list(name = "Pi", type = "fixed"),
#'   list(name = "Tk", type = "random"),
#'   list(name = "Esik", type = "random")
#' )
#'
#' ems_list <- calculate_ems(output_matrix, terms)
#' print(ems_list)
#' }
#'
#' @export

#function script 
#input your matrix from above function when I finish it, and the terms which is a list that you specify with saying its fixed or random etc.
calculate_ems <- function(output_matrix, terms) {
  term_names <- rownames(output_matrix)[-(1:3)]  #skip header rows (ie the number of levels and fixed or random)
  subscripts <- colnames(output_matrix) #pulls the column names and assigns them as the subscripts 
  
  #create lookup for subscript types and levels, this row stores the number of levels for each subscript 
  levels_lookup <- output_matrix["Number of levels", ]
  #this will tell whether each subscript is fixed or random based on the term it's associated with 
  subscript_type <- output_matrix["Fixed or random", ]
  
  #create a named list to store EMS expressions for each term 
  ems_list <- list()
  
  for (term_name in term_names) {
    #get the current term's subscripts in the loop, does this loop for each model term in the input matrix 
    current_codes <- output_matrix[term_name,]
    current_subscripts <- subscripts[current_codes %in% c("0", "1")] 
    #extracts which subscripts are relevant (ie marked as 0 or 1 in the matrix) to the current loop through 
    
    #identify which columns will be used for that step (ie this column is not used for this row so it will be excluded, these will be kept, etc)
    columns_to_keep <- setdiff(subscripts, current_subscripts)
    #ie which will contribute to the ems of that term 
    
    #for each row, check if it contains all current_subscripts
    contributing_terms <- c()
    #does it contribute to the current EMS if so then proceed 
    for (other_term in term_names) {
      other_subscripts <- subscripts[other_codes %in% c("0", "1")]
      if (all(current_subscripts %in% other_subscripts)) {
        #build product of values from columns not in current_subscripts (ie those not "hidden" with a pencil like on the paper)
        visible_cols <- output_matrix[other_term, columns_to_keep]
        product_factors <- visible_cols[visible_cols != "" & visible_cols != "0"]
        product_expr <- paste(product_factors, collapse = " * ")
        #basically for the visible subscripts (columns outside the current term) it collects their number of levels and multiplies them together
        
        #determine if fixed or random
        this_type <- terms[[which(sapply(terms, function(x) x$name == other_term))]]$type
        scale_symbol <- if (this_type == "random") paste0("sigma^2[", other_term, "]") else paste0("phi[", other_term, "]")
        #assigns sigma2 to random variance and i can't figure out how to make it assign the summation notation easily so i just said phi. i'll fix this in an update soon
        
        #checks if scaling multiplier is needed, formats teh component correctly, and adds it to a list of terms that will be summed together to form the full EMS for that term 
        #product_expr is the string that is the product of used factor levels (NOT IN THE CURRENT TERMS SUBSCRIPTS) but in the contributing term 
        #ie the number of times you're multiplying the factor. ie if the subscript has x factor levels and htat gets carried down in that step of deriving EMS 
        #pull down scale symbol to determine what gets multiplied ie sigma2 for random and phi for fixed 
        if (product_expr != "") { #ie if there are levels that get multiplied the variance component should get multiplied by this 
          full_expr <- paste(scale_symbol, "*", product_expr)
        } else {
          full_expr <- scale_symbol} #if there arent any levels to get multiplied for htat term, then you use the variance term as is depending on fixed or random
        contributing_terms <- c(contributing_terms, full_expr)}} #join all contributing terms as a string to be joined together later 
    
    #convert to bquote expression for ease of translation, combines all terms associated with that variable with a + sign to form the full expression 
    #used bquote so it looks nicer and more readable 
    if (length(contributing_terms) > 0) {
      ems_list[[term_name]] <- bquote(.(paste(contributing_terms, collapse = " + ")))}}
  
  return(ems_list) #get output }
  
  #print results 
  ems_results <- calculate_ems(output_matrix, terms)
  print(ems_results)