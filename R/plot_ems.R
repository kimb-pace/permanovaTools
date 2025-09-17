#' Plot Expected Mean Squares (EMS) Expressions 
#' 
#' This function uses the output from the calculate_ems function and plots it nicely in the R plot window or saves it to files in your directory. It generates a clearer 
#' output that is easier to interpret, allowing for easier decision making and methods reporting. 
#' 
#' @param ems_results A named list of EMS expressions from calculate_ems function output 
#' @param title The title of the plot, ie the model if more than one output is desired to be plotted 
#' @param cex Font size 
#' 
#' @return A polished visual containing an organized, cleaned up display of the partitioned variance for a specified model. 
#' 
#' @export
#' 
#' @example 
#' ems_results <- calculate_ems(output_matrix, terms) #generate the variance partitioning 
#' plot_ems(ems_results, 
#'         title = "Plot Name",
#'         cex = 1.2)
#' 
#' @details This function exists simply to improve interpretation and reporting of variance partitioning, it is not essential to running an adjusted PERMANOVA. 
#'  

#function: 

plot_ems <- function(ems_results, 
                     title = "Expected Mean Squared Variance",
                     cex = 1.5) {
  par(mar = c(4, 4, 2, 2))
  n_terms <- length(ems_results)
  
  #make blank plot 
  plot(1, type = "n", 
       xlim = c(0, 2), 
       ylim = c(0, n_terms + 2),
       xaxt = "n", yaxt = "n", bty = "n", 
       xlab = "", ylab = "", main = title)
  y_positions <- n_terms:1
  #column headers 
  text(0, n_terms + 1.5, labels = "Term", pos = 4, cex = cex, font = 2)
  text(0.8, n_terms + 1.5, labels = "Partitioned Variance Expression", pos = 4, cex = cex, font = 2)
  #loop through the terms from the input: 
  for (i in seq_along(ems_results)) {
    term_name <- names(ems_results)[i]
    term_value <- ems_results[[i]]
    #column 1: terms from model 
    text(0, y_positions[i], labels = term_name, pos = 4, cex = cex)
    #column 2: partitioned variance expression from input 
    text(0.8, y_positions[i], labels = term_value, pos = 4, cex = cex)
  }
}

#test call 
plot_ems(ems_results, 
         title = "Example plot, attempt 2",
         cex = 1.2)


#example console output so you can test the function: 
#just random to make it easy 
ems_results <- list(
  Vs   = expression(b * c * sigma^2[Vs] + sigma^2[Esik]),
  Pi   = expression(a * c * sigma^2[Pi] + sigma^2[Esik]),
  Tk   = expression(a * b * sigma^2[Tk] + sigma^2[Esik]),
  Esik = expression(sigma^2[Esik])
  
ems_results <- list(
  Vs = bquote(a * b * r * sum(alpha[i]^2/(a-1), i==1, a) + sigma^2[Esik]),
  Pi = bquote(a * b * r * sum(beta[j]^2/(c-1), j==1, c) + sigma^2[Esik]),
  Tk = bquote(a * b * sigma^2[Tk] + sigma^2[Esik]),
  Esik = bquote(sigma^2[Esik]))
  
terms <- list(
    list(name = "Vs", label = "Viereck.3", subscripts = c("s"), type = "fixed", levels = "a"),
    list(name = "Pi", label = "Park", subscripts = c("i"), type = "fixed", levels = "b"),
    list(name = "Tk", label = "Time", subscripts = c("k"), type = "random", levels = "c"),
    list(name = "Esik", label = "Residual", subscripts = c("s", "i", "k"), type = "random", levels = "abc"))
  
  #Attempt 1: UNSUCCESSFUL :( 
  #Notes to fix: everything lmao. this did NOT work. stay tuned  
  #but specifically, notes to self: 
  #1. model equation not showing up properly 
  #2. model terms not showing up alongside variance partition symbols 
  #3. the variance partitioning did show up correctly based on input but the subscripts need to be fixed so theyre in the correct spot
  #4. model is showing up overlaid on top of title 
  #5. F equation not showing up correctly: it's just putting the input as entered and not F = term variance / term variance 
  
  #Thoughts: will probably remove the F equation plotting option because it's broken and not necessary. It's already built into the 
  #adjust permanova function so it doesn't really need to be on this... it was just a side idea. 
  #will probably remove the highlight option too because I just don't see a need for it other than **I** like it lol. plus it didn't work anyways. 
  #also I guess the model at the top isn't necessary either.......... I will just remove it too :'( the point of this is to just 
  #clean up the output to make it easier to read, so... 
  
  #mike if you're reading this sorry for the brain dump but this is what happens when you leave me alone with free reign on the package all summer 
  #don't worry I'll clean it up 
  
  #ok gutting the function and making it JUST the terms and partitioned variance like it was originally before I flew too close to the sun. old code can be found in previous commits 
  
  #Attempt 2 with simplified function: SUCCESSFUL!! 
  #notes to fix: 
  #1. Make error E in terms column auto convert to greek letter? 
  #2. Fix subscripts, they're showing up as superscripts in teh variance expression 
  #3. add column names >>> Done 
  #4. fix term input so subscripts are proper 
