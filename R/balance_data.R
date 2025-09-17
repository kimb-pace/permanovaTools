#' balance_data: Balance Plot Visits for PERMANOVA 
#'
#' This function subsets a data frame containing ecology data (specifically species composition data in the form of presence/absence, 
#' abundance, or quadrat frequency) to include a specified number of 
#' plots per sampling unit (ie years, months, parks, vegetation classes, etc) for balanced PERMANOVA using \code{adonis2} from the \code{vegan} package.
#' In principle, sampling designs are constructed to be balanced, so under ideal conditions no additional filtering is needed for analysis. 
#' In practice, however, unforseen factors can disrupt data collection and lead to missing or unusable samples. This function saves time by allowing users to set 
#' parameters to filter data without having to manually screen observations.  
#' If some plots have more visits than the desired number, this function selects a subset of years that are most similar to those in 
#' plots already balanced at the target number of visits. 
#' The subset is created based on similarity patterns across input data. This function can be used to balance visits either across years or within 
#' years (such as monthly samples). It selects visits so 
#' that the specified grouping parameters have an equal number of plots or return visits \code{n_visits}. Plots with fewer than the number of plots 
#' specified will be excluded, and plots with a greater 
#' number of visits are filtered to match the most common combination of return visits (units?) found among plots with the specified number of visits.
#' An optional manual override allows the user to decide to retain specific visits regardless of the similarity to other plots. 
#'
#' @param df A data frame containing plot and year information.
#' @param plot_col The name of the column in `df` that identifies plots.
#' @param balance_col The name of the column in `df` to balance (ie Year, Park, Vegetation Class)
#' @param year_col The name of the column in `df` that identifies years.
#' @param visit_col An optional column. The name of the column in the data frame that identifies unique visits (ie year_month). Required if "multiple_visits_per_year = TRUE" in call.
#' @param n_visits A number specifying the number of visits (years) to retain per plot.
#' @param multiple_visits_per_year Logical argument. If TRUE, balances visits selection within years (ie for monthly visits). If FALSE, balances visits by year. 
#' @param require_all_years Logical argument. If TRUE and "multiple_visits_per_year" = TRUE, this ensures that selected visits span all available uears for each plot. FALSE is default. 
#' @param manual_selection Optional named list specifying plots as names and vectors of years to retain. This overrides automatic selection for specified plots.
#'
#' @return A list with 
#' \describe{
#'  \item{filtered_df}{The filtered version of the data frame that retains only the selected visits for plots with exactly `n_visits`.
#'  \item{summary_table}{A summary table containing info about which visits were retained per plot, including whether they were auto- or manually selected}
#'
#' @examples
#' #Assume you have a data frame called 'veg_df' with columns "Plot" and "Year". Plots range from 1-4 visits each and you want to retain two visits of your choice 
#' balanced_dataframe <- balance_visits(
#'   df = veg_df,
#'   plot_col = "Plot",
#'   balance_col = "Year",
#'   year_col = "Year",
#'   n_visits = 2)
#'   
#' #Assume you have a data frame called 'veg_df' with columns "Plot" and "Month". Plots range from 7-12 visits within the year and you want to retain 7 months for each plot. 
#' balanced_months <- balance_visits(
#' df = veg_df,
#' plot_col = "Plot",
#' balance_col = "Month",
#' n_visits = 7)  
#' 
#' #Assume you want to make sure that specific visits for Plots 4 and 5 are retained (example justification: lichen data was also collected those years and you want to compare results). 
#' # With manual override for specific plots 
#' manual_years <- list(
#'   "Plot4" = c(2012, 2015),
#'   "Plot5" = c(2013, 2016))
#' 
#' #Now that you've specified which visits you want, include that in the call: 
#' balanced_manual <- balance_visits(
#'   df = veg_df,
#'   plot_col = "Plot",
#'   year_col = "Year",
#'   balance_col = "Year"
#'   n_visits = 2,
#'   manual_selection = manual_years)
#'   
#' #Balance visits across multiple months per year, retaining 7 visits per plot 
#' balanced_months <- balance_visits(
#' df = veg_df,
#' plot_col = "Plot",
#' year_col = "Year",
#' visit_col = "Visit_Identifier, whatever it's called in your data frame", #ie year_month, 2020_06 
#' n_visits = 7, 
#' multiple_visits_per_year = TRUE,
#' require_all_years = TRUE)
#' 
#'@importFrom dplyr select distinct group_by summarize rename mutate filter arrange inner_join bind_rows 
#'@importFrom tidyr unnest 
#'@importFrom tibble tibble 
#'@importFrom purr map map_chr 
#'@importFrom rlang sym .data 
#'
#'
#' @export    

#note: run devtools::document() once all of this is complete 


#balancing function 
balance_visits <- function(df, 
                           plot_col = "Plot", 
                           year_col = "Sample_Year", 
                           visit_col = NULL,
                           balance_col = "Sample_Year",
                           n_visits = 2,
                           multiple_visits_per_year = FALSE,
                           require_all_years = FALSE,
                           manual_selection = NULL) { #use null if you aren't manually selecting plots, use manual_plots in selection if you are choosing specific ones
  #this makes a df that has one row per plot and a list of all years that plot was visited. 
  #basically the summary table from the building dataframes section, which is what I used to hand select and balance things. 
  #this way you can look at the years and specify if you want a manual override or to let it handle it auto-style 
  
  visit_id_col <- if (!is.null(visit_col) && multple_visits_per_year) visit_col else year_col 
  
  #create a summary of whatever specified units you want to balance by 
  plot_units <- df %>%
    dplyr::select(.data[[plot_col]], .data[[balance_col]]) %>%
    distinct() %>%
    group_by(.data[[plot_col]]) %>%
    summarize(Balance_Units = list(sort(unique(.data[[balance_col]]))), .groups = "drop")
  
  #i guess I need to change all visits to units now? that might be the issue honestly. or at least one issue. follow up*****
  
  #if only want to have a visit_col and not a year col, for ease of use to accomodate multiple types of dataframes:
  #this assumes your visit_col is in the format YYYY_MM ie 2020_06 
  get_year <- function(v) {
    if (is.numeric(v)) return (v)
    as.numeric(substr(v,1,4))
  }
  
  plot_units <- df %>%
    dplyr::select(.data[[plot_col]], .data[[visit_id_col]]) %>% #selects just the two columns of interest from the dataframe, one for plotIDs and one for sample years.
    #used .data[[name]] instead of actual column name so that you can have variation in what you call the column to have flexibility in future use 
    distinct() %>%
    group_by(.data[[plot_col]]) %>%
    summarize(Years = list(sort(unique(.data[[visit_id_col]]))), .groups = "drop")
  
  #manual override handling of selection 
  manual_df <- NULL #creates items for manual selection if used. this is the longform version of selected years if added 
  manual_summary <- NULL #this is the labeled version of the years for if you want to visualize/print it, for readability 
  manual_plots <- character() #names of plots hand selected, skips auto-selection for them as it sifts through. makes an empty vector to store names of plots taht were manually selected. 
  #use this to exclude plots from automatic processing later on 
  if (!is.null(manual_selection)) { #if null the rest will be skipped!!!!
    manual_df <- tibble::tibble( #turns named list of years into a tibble for ease of use 
      !!plot_col := names(manual_selection),
      Selected_Visits = manual_selection
    ) %>%
      tidyr::unnest(cols = Selected_Visits) %>% #expands each row into multiple rows (longform table) ie plot1 c(2018, 2020) into plot1 2018
      # plot1 2020 
      dplyr::rename(Selected_Visit = Selected_Visits) #since now its long form instead of c(year1, year2) rename column to reflect that 
    manual_summary <- manual_df %>%
      mutate(Selection_Type = "Manual") #adds manual as the selection type for designation 
    manual_plots <- unique(manual_df[[plot_col]]) #grabs list of plot names in this section for later use 
  }
  
  #remove manually selected plots from being part of the auto-selection process if there are some specified in call, so they 
  #don't get processed twice (manual and auto)
  plot_visits <- plot_visits %>%
    filter(!(!!rlang::sym(plot_col) %in% manual_plots)) #keep rows where the plot column is NOT in the list of manual plots; tidy eval
  #categorize plots based on how many visits they have 
  #plots woth the desired number of visits: 
  exact_plots <- plot_visits %>%
    filter(lengths(Visits) == n_visits)
  #plots with more than the desired number of visits (need to be decreased)
  over_plots <- plot_years %>%
    filter(lengths(Visits) > n_visits)
  #plots with less than teh number of visits (will be excluded)
  under_plots <- plot_years %>%
    filter(lengths(Visits) < n_visits)
  #figure out what the common year combos are among plots with the correct number of visits, to have it match in its auto-selection 
  #this will guide the auto-selection's decisions on the over_plots 
  common_combos <- exact_plots$Visits %>%
    purrr::map_chr(~ paste(sort(.x), collapse = "_")) %>% #takes the vector of years and sorts them in ascending order, and then joins them as a list with underscores. 
    #ie c(2012, 2014) would become 2012_2014. used purr from tidyverse to return a character vector so it's easier to tally up with table. 
    #used laaply() originally but had to unlist and it got messy down the line and this is just easier. 
    #i also tried sapply() but this just worked better because map_chr() guarantees it returns as a character vector instead of sapply() assuming it's a character but sometimes listing it 
    #as something else and giving me an error. I have been FIGHTING with this function all summer 
    table() %>% #creates a frequency table for the year combinations to see what the common ones are 
    sort(decreasing = TRUE) #sort from msot common to least common 
  top_combo <- strsplit(names(common_combos)[1], "_")[[1]] 
  if (is.numeric(unlist(plot_visits$Visits))) top_combo <- as.numeric(top_combo) #decide what the most common one is, split it back from the string into the OG format 
  #%>% as.numeric() 
  
  #originally was this, retaining for now commented out just in case I want to change it back 
  #common_combos <- table(
  #sapply(exact_plots$Years, function(x) paste(sort(x), collapse = "_"))
  # ) %>%
  #sort(decreasing = TRUE)
  
  #auto select closest matching years from the over_plots list of plots 
  #basically this generates all combinations of n_visits and then picks the closest to the most similar combination using absolute year differences 
  over_plots <- over_plots %>%
    mutate(
      Selected_Visits = purrr::map(Visits, function(vs) { #vs = visit, #for each element of years, applies function to select n_visits subset and purrr:map returns a list 
        combs <- combn(vs, n_visits, simplify = FALSE) 
        
        #if specified to use all years 
        if(require_all_years) {
          all_years <- unique(get_year(vs))
          required_n_years <- length(all_years)
          combs <- Filter(function(x) length(unique(get_year(x))) == required_n_years, combs)
          if (length(combs) == 0 {
            warning("No combos found that span all years")
            combs <- combn(vs, n_visits, simplify = FALSE)}}
        #combn to generate all combos of n_visits years from the full list of years, 'simplify = false' to get a list of vectors 
        combs[which.min(sapply(combs, function(x) sum(abs(sort(x) - top_combo))))][[1]]})) #sort(x) to put years and top combo in order, abs(sortx-topcombo) 
      #calculates the difference between the combos of years ie (2012, 2014) and (2012, 2015) would return something like (0,1) and then these are aded together
      #so the lower the overall sum the closer the combo of years is to top combo if that makes sense (used which.min to find the lowest)
      
      #combine the exact_plots and the auto-selected plots once everything is selected and sorted 
      selected_auto <- bind_rows(
        exact_plots %>% mutate(Selected_Visits = Visits),
        over_plots) %>%
        select(!!plot_col, Selected_Visits) %>%
        tidyr::unnest(cols = Selected_Visits) %>% #so each year becomes its own row again 
        dplyr::rename(Selected_Year = Selected_Visits) %>% #changes the year column name to reflect that 
        mutate(Selection_Type = "Auto") #adds a selection type column for tracking if you want to knwo which plots were auto selected and which were manual 
      
      #add in manual selections if specified, optional if nothing is specified in the call 
      if (!is.null(manual_summary)) {
        all_selected <- bind_rows(selected_auto, manual_summary)
      } else {
        all_selected <- selected_auto}
      
      #filter dataframe with plot criteria outlined above now that the specification has been made 
      filtered_dataframe <- df %>%
        dplyr::inner_join(all_selected,
                          by = setNames(c(plot_col, visit_id_col), c(plot_col, "Selected_Visit")))
      
      #generate summary table with selected plots 
      summary_table <- all_selected %>%
        arrange(.data[[plot_col]], Selected_Visit) #used .data for flexibility 
      
      #print results with clarity in whats what
      cat("balance Summary\n")
      cat("Plots with exact/trimmed visits (auto):", length(unique(selected_auto[[plot_col]])), "\n")
      if (!is.null(manual_summary)) {
        cat("Plots with manual override:", length(manual_plots), "\n")}
      if (nrow(under_plots) > 0) {
        cat("Dropped", nrow(under_plots), "plot(s) with fewer than", n_visits, "visits:\n")
        print(under_plots[[plot_col]])}
      return(list(
        filtered_dataframe = filtered_dataframe,
        summary_table = summary_table))}}}

#Example Calls 

#balance by year
result <- balance_visits(
  df = quad_abundance_df_vascular_filtered,
  plot_col = "Plot",
  year_col = "Sample_Year",
  n_visits = 2,
  manual_selection = list(
    "KATM_2009_01_S996" = c(2012, 2014),
    "LACL_2010_01_S995" = c(2014, 2019))) #or manual_selection = NULL 

#balance by month 
balance_visits(df = quad_abundance_df_vascular_filtered,
               plot_col = "Plot",
               year_col = "Sample_Year",
               visit_col = "Month_Year", #ie 2020_06
               n_visits = 3,
               multiple_visits_per_year = TRUE,
               require_all_years = TRUE)









