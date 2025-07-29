#PLOT EMS RESULTS PRETTY ?????? becasue this current output is ugly AF   >:( 

#does write tipsy edit sober hold up for writing code like it did for both of my thesises? theses. thesis x 2. i guess there is only 
#one way to find out! stay tuned lol 

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

#maybe turn plot_ems into a function to help with looking at results? 

#' Plot Expected Mean Squares (EMS) Expressions 
#' 
#' This function takes the output from the calculate_ems function and plots it nicely in the R plot window or saves it to files in your directory.
#' 
#' @param ems_results A named list of EMS expressions from calculate_ems function output 
#' @param title The title of the plot, ie the model if more than one output is desired to be plotted 
#' @param cex font size 
#' @param save_to_file logical statement, if TRUE saves as a PNG file to your directory 
#' @param file_name Output file name (if = TRUE for save_to_file)
#' 
#' @export
#' 
#' 
#'    



plot_ems <- function(ems_results, title = "Expected Mean Squared Variance",
                     cex = 1.5, save_to_file = FALSE,
                     filename = "EMS_output.png") {
  if (save_to_file) {
    png(filename, width = 1000, height = 600)
  } else {
    plot.new()}
  
  par(mar = c(4, 4, 2, 2))
  n_terms <- length(ems_results)
  
  #add in here the specifications from above: 
  
  
  
  
  
  
  call: 
    ems_results <- calculate_ems(output_matrix, terms)
  plot_ems(ems_results) #shows in viewer 
  plot(ems_results, save_to_file = TRUE, filename = "name.png")
  
  
  
  ideas to add: 
    
    1. add a highlighted line or two. highlight_term = NULL or otherwise ie if you want to bold a line to use as an adjusted F equation 
  2. need to fix the fixed effect summation formula from phi to the actual one. once you figure out how to code it here 
  3. save as pdf? instaed of png 
  4. add in there the F equation if specifying separately. so maybe in the bottom or right you can pull the variance from the line and 
  just visually show it as x/x for fun 