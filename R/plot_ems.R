ideas to add: 
  
  1. add a highlighted line or two. highlight_term = NULL or otherwise ie if you want to bold a line to use as an adjusted F equation 
  #to add highlighting for specified terms: 
  if (!is.null(highlight_term) && term == highlight_term) {
    text(0, i, labels = ems_results[[term]], pos = 4, cex = 1.4, font = 2, col = "red")
  } else {
    text(0, i, labels = ems_results[[term]], pos = 4, cex = 1.4)
  }
  
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
                           

#' Plot Expected Mean Squares (EMS) Expressions 
#' 
#' This function uses the output from the calculate_ems function and plots it nicely in the R plot window or saves it to files in your directory. It generates a clearer 
#' output that is easier to interpret, allowing for easier decision making and methods reporting. 
#' 
#' @param ems_results A named list of EMS expressions from calculate_ems function output 
#' @param title The title of the plot, ie the model if more than one output is desired to be plotted 
#' @param cex font size 
#' @param save_to_file logical statement, if TRUE saves as a PNG file to your directory 
#' @param file_name Output file name (if = TRUE for save_to_file)
#' 
#' @return An output file (.png or .pdf) containing an organized, cleaned up display of the partitioned variance for a specified model. 
#'         This output contains the specified model, list of associared terms, partitioned variance components, and the correct F equations for each term in said model.  
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
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, length(ems_results) + 1),
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", main = "Expected Mean Squares")

#draw each expression with bquote formatting
i <- length(ems_results)
for (term in names(ems_results)) {
  text(0, i, labels = bquote(EMS[.(term)] == .(ems_results[[term]])), pos = 4, cex = 1.4)
  i <- i - 1}

#define the EMS expressions using bquote() so they look better 
ems_list <- list(
  Vs   = bquote(b * c * phi[Vs] + sigma^2[Esik]),
  Pi   = bquote(a * c * phi[Pi] + sigma^2[Esik]),
  Tk   = bquote(a * b * sigma^2[Tk] + sigma^2[Esik]),
  Esik = bquote(sigma^2[Esik])
)





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

#show an F equation that is going to be adjusted 

if (!is.null(f_equation)) {
  text(0, 0.5, labels = f_equation, pos = 4, cex = 1,2, col = "darkgreen")
}
#input: 
f_equation <- bquote(F = frac(.(ems_results[["Vs"]])), .(ems_results[["Esik"]])


#function start: in progress 

plot_ems <- function(ems_results, title = "Expected Mean Squared Variance",
                     cex = 1.5, save_to_file = FALSE,
                     filename = "EMS_output.png") {
  if (save_to_file) {
    if (tools::file_ext(filename) == "pdf") {
      pdf(filename, width = 10, height = 6)
    } else {
    png(filename, width = 1000, height = 600)
    } 
  }
  
  par(mar = c(4, 4, 2, 2))
  n_terms <- length(ems_results)
  



  

                     
  

  
  
  
