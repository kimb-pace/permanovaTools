#Package - putting it all together (adjusted_permanova, derive_ems, calculate_matrix, balance_dataframe) 



#install required packages to create a package 
install.packages(c("usethis", "devtools", "roxygen2", "testthat"))

#run this in console:  
usethis::create_package("path/to/Adjusted_Permanova")

#place all function files into the package directory once theyre done 
usethis::use_r("adjusted_permanova")
usethis::use_r("derive_ems")
usethis::use_r("balance")
usethis::use_r("calculate_matrix")

#use roxygen style comments above each function to document them 
----
  
  devtools::document()

#add dependencies 
usethis::use_package("dplyr")
usethis::use_package("vegan")

#include example data 
usethis::use_data_raw("beetle_data")
usethis::use_data(beetle_data, overwrite = TRUE)

#load and test package then test functions like normal 
devtools::load_all()

#run checks for problems 
devtools::check()

#install package locally 
devtools::install()






-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #saving data as the example data for package 
  
  #load data into R 
  beetle_matrix <- beetle_distance_matrix
beetle_env <- beetle_env

#save them for package use
usethis::use_data(beetle_matrix, beetle_env, overwrite = TRUE)

#document using Roxygen 
#' Example Distance Matrix
#'
#' A sample distance matrix representing dissimilarity among bark beetle disturbed sites.
#'
#' @format A matrix with X rows and X columns.
#' @usage data(beetle_matrix)
#' @examples
#' data(beetle_matrix)
"beetle_matrix"

#' Example Environmental Metadata
#'
#' A sample metadata dataframe with grouping factors such as sample year, vegetation subclass, and canopy cover percent.
#'
#' @format A data frame with X rows and X columns.
#' @usage data(beetle_env)
#' @examples
#' data(beetle_env)
"beetle_env"

#save both DFs in an .R file in the R/ directory of the package (R/data_matrix.R).

#' @examples
#' data(beetle_matrix)
#' data(beetle_env)
#' 
#' base_perm <- permute::how(within = permute::Within(type = "free"), blocks = beetle_env$Block)
#' corrected_struct <- permute::how(within = permute::Within(type = "none"), blocks = beetle_env$Plot)
#' 
#' do i need to add in base perm in here? like pre-loaded or does it get pulled in with the pre-load from the function... check on tjhis
#' 
#' adjusted_permanova(
#'   data = beetle_env,
#'   matrix = beetle_matrix,
#'   base_permutations = base_perm,
#'   corrected_F_equations = list("Group/Plot" = corrected_struct),
#'   terms = "Group + Plot"
#' )