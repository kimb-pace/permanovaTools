ideas to add: 
  
  1. add a highlighted line or two. highlight_term = NULL or otherwise ie if you want to bold a line to use as an adjusted F equation 
  
  2. need to fix the fixed effect summation formula from phi to the actual one. once you figure out how to code it here 
  #fix phi placeholder for correct summation symbol 
  if (this_type == "random") {
    scale_symbol <- bquote(sigma^2[.(other_term)]) #other term = not main term but one that contributes to EMS of main term 
  } else {
    scale_symbol <- bquote(frac(1, n[.other_term)} -1) *
      sum(alpha[i]^2, i ==1, n[.(other_term)]))
  }
  
  3. save as pdf? instaed of png. maybe have two options? 
    
    if (save_to_file) {
      if (tools::file_ext(filename) == "pdf") {
        pdf(filename, width = 10, height = 6)
      } else {
        png(filename, width = 1000, height = 600)
      } 
    }
  
  4. add in there the F equation if specifying separately. so maybe in the bottom or right you can pull the variance from the line and 
      just visually show it as x/x for fun 
      #input: 
      f_equation <- bquote(F = frac(.(ems_results[["Vs"]])), .(ems_results[["Esik"]])
                           
                           
Also To do: 
      #add in option for two F equations? 
      #option to add the model all pretty at the top using the input for build matrix!!!!!!! 
  
  #so it would be 

  **MODEL**
  **TERMS + PARTITIONED VARIANCE**
  **F EQUATIONS OF INTEREST*** 
                             
                           
                           
                           
#' Plot Expected Mean Squares (EMS) Expressions 
#' 
#' This function uses the output from the calculate_ems function and plots it nicely in the R plot window or saves it to files in your directory. It generates a clearer 
#' output that is easier to interpret, allowing for easier decision making and methods reporting. 
#' 
#' @param ems_results A named list of EMS expressions from calculate_ems function output 
#' @param title The title of the plot, ie the model if more than one output is desired to be plotted 
#' @param cex Font size 
#' @param highlight_term 
#' @param f_equation 
#' @param full_model 
#' 
#' @return A polished visual containing an organized, cleaned up display of the partitioned variance for a specified model. 
#'         This output contains the specified model, list of associated terms, partitioned variance components, and the correct F equations for each term in said model.  
#' 
#' @export
#' 
#' @example 
#' ems_results <- calculate_ems(output_matrix, terms) #generate the variance partitioning 
#' plot_ems(ems_results) #shows in viewer 
#' plot(ems_results, 
#'      save_to_file = TRUE, 
#'      filename = "name.png") 
#' 
#' @details This function exists simply to improve interpretation and reporting of variance partitioning, it is not essential to running an adjusted PERMANOVA. 
#'  


#concept work, options 
plot.new()
#create PNG to save the plot to 
png("EMS_output.png", width = 800, height = 400)

#make blank plot
par(mar = c(4, 4, 2, 2))  #adjust margins


#draw each expression with bquote formatting
i <- length(ems_results)
for (term in names(ems_results)) {
  text(0, i, labels = bquote(EMS[.(term)] == .(ems_results[[term]])), pos = 4, cex = 1.4)
  i <- i - 1}

#example console output to test function: 

#just random to make it easy 
ems_results <- list(
  Vs   = expression(b * c * sigma^2[Vs] + sigma^2[Esik]),
  Pi   = expression(a * c * sigma^2[Pi] + sigma^2[Esik]),
  Tk   = expression(a * b * sigma^2[Tk] + sigma^2[Esik]),
  Esik = expression(sigma^2[Esik])
)
#add in fixed? 
ems_results <- list(
  Vs   = expression(a * b* r *sum(frac(alpha[i]^2, (a-1)), i ==1, a) + sigma^2[Esik]),
  Pi   = expression(a * b* r *sum(frac(beta[j]^2, (c-1)), j ==1, c) + sigma^2[Esik]),
  Tk   = expression(a * b * sigma^2[Tk] + sigma^2[Esik]),
  Esik = expression(sigma^2[Esik])
)

#use: 
plot_ems(
  ems_results, 
  highlight_term = "Vs",
  f_equation = bquote(F == frac(.(ems_results[["Vs"]], .ems_results[["Esik"]]))),
 # file = "EMS_output_fixed.png", #removed save to file part of function because i hate it 
)
#ok this is ugly and hard to use in terms of F equation specification. need to incorporate the bquote frac so it does it internally 
#so that users can just be like f_equation = Vs/Esik and it'll do the conversion using that 
#maybe somehting like the run_permanova function using strsplit? terms <- strsplit(f equation, "/") and then 
#move the bquote(F == frac(.(ems_results[["Vs"]], .ems_results[["Esik"]]))) from the original input to the function like 
#f_expression <- bquote(F == frac etc) then add back in the text(0, 0.5, labels = f_equation, pos = 4, cex = cex, col = "darkgreen") from original? 

#maybe add in a fallback in case soemthing gets entered that doesnt work? ie it'll just put the terms as is if its something that doesnt convert like vs+pi instead of an error 
#else f_expression <- f_equation? 


#adding the fixed factor expression with the summation notation?? 
#create new png to write on 
png("output_with_equation.png", width = 800, height = 600)

#make a blank plot 
dev.off()
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

#add the equation 
text(
  x = 0.5, y = 0.5,
  expression(frac(1, a - 1) * sum(alpha[i]^2, i == 1, a)),
  cex = 2
)  


#input: 
f_equation <- bquote(F = frac(.(ems_results[["Vs"]])), .(ems_results[["Esik"]])


#function start: in progress 

#removed save to file aspects because it was getting too complicated 
plot_ems <- function(ems_results, 
                     title = "Expected Mean Squared Variance",
                     cex = 1.5, 
                     #save_to_file = FALSE,
                     #filename = "EMS_output.png",
                     highlight_term = NULL,
                     f_equation = NULL) {
  
#  if (save_to_file) {
#    if (tools::file_ext(filename) == "pdf") {
#      pdf(filename, width = 10, height = 6)
#    } else {
#    png(filename, width = 10, height = 6, units = "in", res = 200) #res 150-200 for pubs; removed hard sizing for adjustment purposes 
#    } #fixed issue with it not plotting in the viewer if you specify to save it lol oops
#  }

  par(mar = c(4, 4, 2, 2))
  n_terms <- length(ems_results)
  #make blank plot 
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, length(ems_results) + 1),
       xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", main = "Expected Mean Squares")
  #to add highlighting for specified terms: 
  i <- n_terms 
  for (term in names(ems_results)) {
    if (!is.null(highlight_term) && term == highlight_term) {
      #convert console expression to pretty expression bc ems_results is already a bquote expression: EASY PEASY
      text(0, i, labels = ems_results[[term]], pos = 4, cex = 1.4, font = 2, col = "red") 
    } else {
      text(0, i, labels = ems_results[[term]], pos = 4, cex = 1.4)
    }
    i <- i - 1
  }
  #show an F equation that is going to be adjusted 
  if (!is.null(f_equation)) {
    if (is.character(f_equation)) {
      terms <- strsplit(f_equation, "/")[[1]]
      if (length(terms) == 2) {
        num <- terms[1]
        den <- terms[2]
        f_expression <- bquote(F == frac(.(ems_results[[num]]), .(ems_results[[den]])))
      } else {
        f_expression <- f_equation
      }
    text(0, 0.5, labels = f_equation, pos = 4, cex = cex, col = "darkgreen")
  }
}


  

  
  
  
