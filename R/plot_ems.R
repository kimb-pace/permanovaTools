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

#ok gutting the function and making it JUST the terms and partitioned variance like it was originally before I flew too close to the sun



#Attempt 2 with simplified function: SUCCESSFUL!! 
  #notes to fix: 
    #1. Make error E in terms column auto convert to greek letter? 
    #2. Fix subscripts, they're showing up as superscripts in teh variance expression 
    #3. add column names >>> Done 
    #4. fix term input so subscripts are proper 





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
  
  #add in fixed? 
  
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
  






  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#OLD CODE, KEEPING FOR NOW FOR DOCUMENTATION PURPOSES!  
  
  #full function that doesnt work and is getting the chopping block treatment, keeping in case I want to take anotehr stab at some of these add-ins 
  #function: 

plot_ems <- function(ems_results, 
                     title = "Expected Mean Squared Variance",
                     cex = 1.5, 
                     highlight_term = NULL,
                     f_equation = NULL,
                     terms = NULL) {
  par(mar = c(4, 4, 2, 2))
  n_terms <- length(ems_results)
  #make blank plot 
  plot(1, type = "n", 
       xlim = c(0, 1), 
       ylim = c(0, length(ems_results) + 1),
       xaxt = "n", yaxt = "n", bty = "n", 
       xlab = "", ylab = "", main = title)
  
  #add model formula from terms object generated in build_matrix function step
  if (!is.null(terms)) {
    error_term <- terms[[ which(sapply(terms, function(x) x$label == "Residual")) ]] #need to add tolower
    #get error term from terms bc it has all the subscripts for Y at beginning of equation
    y_subscripts <- paste0(error_term$main_subscripts, collapse = "") #paste0 instead of paste for no space 
    formula_str <- paste0("y_{", y_subscripts, "} == mu")
    for (t in terms) {
      if (tolower(t$label) == "residual") { #use tolower to standardize so Error and error are both read regardless of what is entered in terms 
        formula_str <- paste0(formula_str, " + \\varepsilon_{", y_subscripts, "}") #so you can have fancy symbol isntead of E
      } else {
      formula_str <- paste0(formula_str, " + ", t$name)
    }
    text(0, n_terms + 1.5, labels = formula_str, pos = 4,
         cex = 1.2, xpd = TRUE, parse = TRUE, col = "black") #xpd true to help with plotting long things and not clip them, parse = true for better expression of symbols 
  }
  
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
  
  #show F equation that is going to be adjusted (or multiple)
  if (!is.null(f_equation)) {
    if (is.character(f_equation)) f_equation <- as.character(f_equation)
    n_f <- length(f_equation) #number of entries 
    y_positions <- seq(0.5, 0.5 -0.3*(n_f-1), length.out = n_f) #stagger them downwards. maybe list? fixes issue of them overlapping 
      
    for (j in seq_along(f_equation)) { #basically just go down the list for x amount of them that are entered
      eq <- f_equation[j]
      f_expression <- f_equation #handles if someone inputs something that isnt in the expression 
                                  #explicitly or if the strign doesn't contain / that way it doesnt break
    if (grepl("/", eq)) { #grepl = for checking if patterns exist in a string. checks if input is A/B correctly in call 
    terms <- strsplit(f_equation, "/")[[1]]
      if (length(terms) == 2 && all(terms %in% names(ems_results))) {
        f_expression <- bquote(F == frac(.(ems_results[[terms[1]]]), .(ems_results[[terms[2]]])))
      } 
    text(0, 0.5, labels = f_equation, pos = 4, cex = cex, col = "darkgreen") 
    #fix issue where multuple F equations plotted on top of each other- need to make them stack 
  }
  }
}
  }
}
            
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
  
  
  Also To do: 
    #add in option for two F equations? 
    #option to add the model all pretty at the top using the input for build matrix!!!!!!! 
    
    #so it would be 
    
    **MODEL**
    **TERMS + PARTITIONED VARIANCE**
    **F EQUATIONS OF INTEREST*** 
    
    #so the input would be
    F_equation = Vs/Esik OR F_equation = c(Vs/Esik, Vs/Pi, etc.) > swap to vector with x possible entries 
  
  #maybe also remove highlight option? idk if it'll be useful or add anything that the F equation wouldn't already add 
  #maybe since it's already here I'll keep it in 
  #jury's still out 

           
#moving extra code to the bottom in case I want it later 

#removed save to file usage from funciton 

  #  if (save_to_file) {
  #    if (tools::file_ext(filename) == "pdf") {
  #      pdf(filename, width = 10, height = 6)
  #    } else {
  #    png(filename, width = 10, height = 6, units = "in", res = 200) #res 150-200 for pubs; removed hard sizing for adjustment purposes 
  #    } #fixed issue with it not plotting in the viewer if you specify to save it lol oops
  #  }
  
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
                     
                     