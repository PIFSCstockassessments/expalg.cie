
#' Type of day indices corresponding to a vector of database type of day entries
#' 
#' @param type_of_days vector of strings or single string for type of day ("WD" or "WE")
#'
#' @return The type of day indices for the input string(s) (1 = "WD", 2 = "WE")
get_days = function(type_of_days) {
  match(type_of_days, c("WD", "WE"))
}

#' Charter status index corresponding to a database charter entry
#' 
#' @param charter a single string or boolean for charter status
#' @return Charter indices for the input string/boolean (1 = charter, 2 = non-charter)
get_charter = function(charter) {
  if(charter == TRUE || charter == "T" || charter == "TRUE") {
    return(1)
  }
  else if(charter == FALSE || charter == "F" || charter == "FALSE") {
    return(2)
  }
  else { #TODO: Should this be a error message?
    warning(paste("Charter ", charter, " not valid"))
    return(-1) 
  }
}

#' Charter status indices corresponding to a vector of database charter entries
#' 
#' @param charters vector of strings/boolean or single string/boolean for charter status
#' @return Charter indices for the input string(s)/boolean(s) (1 = charter, 2 = non-charter)
get_charters = function(charters) {
  unlist(lapply(charters, get_charter))
}

# #' Port key indexes
# #' 
# #' @param port_fks vector of integers for port keys
# #' @return Port indices for the input integer(s)
# get_ports = function(port_fks) {
#   match(port_fks, ports_refer)
# }

# #' Method key indexes
# #' 
# #' @param method_fks vector of integers for method keys
# #' @return Method indices for the input integer(s)
# get_methods = function(method_fks) {
#   match(method_fks, methods_refer)
# }

#' Convert Data frame of Array Entries to Array of entries
#' 
#' Convert a Data Frame of array entries (as returned by \code{dplyr::summarise}) to an Array containing the entries
#' 
#' @param x data frame where each row represents the index of a value, along with the value itself (as returned by the summarise method in library dplyr)
#' @param l list of reference vectors whose lengths give the dimensions of the output array (first vector length first dimension of the output, etc.)
#' @md
#' @return Array whose dimensions match the dimensions of l and whose entries represent data for the corresponding 
#'   indices from x. Non-specified entries are assumed to be zero. For example, if inputs are:
#' 
#'   ```
#'           x = 1 1 7     l = list(c(1,2), c(1,7,9))
#'               1 2 5             ^        ^
#'               2 2 1             |        |
#'               2 3 2             |        second dimension is of length length(c(1,7,9)) = 3
#'               ^ ^ ^             first dimension is of length length(c(1,2)) = 2
#'               | | |
#'               | | values for each index
#'               | second dimension indices
#'               first dimension indices
#'  
#'           Then the output will be:
#'  
#'           res = [7 5 0  <-- 2 x 3 array with values as specified by the last column of x, and other entries zero
#'                  0 1 2]
#'  ```
#'  
#' @export
#'  
df_to_array = function(x, l) {
  # initialize dimensions of result matrix based on reference vectors given
  res = array(0, sapply(l, function(f) length(f)))
  
  indices_df = data.matrix(x[, -length(x)]) # get rid of the last column, which holds the counts
  indices = t(apply(indices_df, 1, function(f) as.vector(f))) # indices corresponding to each count
  values = x[[length(x)]] # get only the last column, which holds the counts
  
  res[indices] = values
  
  return(res)
}

#' Run Expansion Algorithm 
#' 
#' "CIE_" input data *.csv files are read, and passed to the expansion calculations.
#' 
#' @param year year of the expansion
#' @param pool_f Pool interviews when fewer than 3 are available for a stratum
#' @param species NA to expand for all species, or a vector of SPECIES_PK to only include specific species
#' 
#' @return two data frames containing all expansion and species composition fields for each expansion stratum
#' 
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @importFrom dplyr mutate filter
#' @importFrom rlang .data .env
#'
#' @noRd
#' 
run_expansion = function(year, pool_f, species) {

  # Throw error message if 'pool_f' was not a logical string  
  if(is.na(pool_f)){
    stop("pool_f is not a logical type")
  }
  
  sample_days = read.csv(system.file("extdata","CIE_sample_days_allyears.csv", package= "expalg.cie"), stringsAsFactors = F)
  sample_days_current_year = filter(sample_days, sample_days$YEAR == year)
  
 # Ma used MySQL package to read MySQL tables into R and use write.csv to create the csv files for use in this program 
  
 # Ma joined the two tables with MySQL in R and output the result as "data/CIE_bl_allyears.csv" 
  bl = read.csv(system.file("extdata","CIE_bl_allyears_pub.csv", package="expalg.cie"), stringsAsFactors = F)
  bl_current_year = filter(bl, substr(bl$SAMPLE_DATE, 1, 4) == year)
  
  p1 = read.csv(system.file("extdata","CIE_p1_allyears.csv", package ="expalg.cie"), stringsAsFactors = F)
  p1_current_year = filter(p1, p1$YEAR == year)
  
  days = read.csv(system.file("extdata","CIE_days_allyears.csv", package ="expalg.cie"), stringsAsFactors = F)
  days_current_year = filter(days, days$YEAR == year)
  
  interviews_raw = read.csv(system.file("extdata","CIE_interviews_raw_pub.csv", package ="expalg.cie"), stringsAsFactors = F)
  interviews_raw = mutate(interviews_raw, YEAR = strtoi(substr(interviews_raw$SAMPLE_DATE, 1, 4)))
  catch_raw = read.csv(system.file("extdata","CIE_catch_raw.csv", package ="expalg.cie"), stringsAsFactors = F)
  # 6/23/20 added by TM
  if(!is.na(species)) {
    catch_raw = filter(catch_raw, catch_raw$SPECIES_FK %in% species)
    for(i in 1:nrow(interviews_raw)) {
      interview_key = interviews_raw[i, "INTERVIEW_PK"]
      interviews_raw[i, "TOT_EST_KGS"] = sum(filter(catch_raw, catch_raw$INTERVIEW_FK == interview_key)$EST_KGS, na.rm = T)
    }
  }
  interviews_current_year = filter(interviews_raw, interviews_raw$YEAR == year)
  catch_current_year = filter(catch_raw, substr(catch_raw$INTERVIEW_FK, 1, 4) == year)
  
  iwc = read.csv(system.file("extdata","CIE_iwc_allyears.csv", package ="expalg.cie"), stringsAsFactors = F)
  iwc_current_year = filter(iwc, substr(iwc$SAMPLE_DATE, 1, 4) == year)
  
  reference_raw = read.csv(system.file("extdata","CIE_reference_raw.csv", package ="expalg.cie"), stringsAsFactors = F)

  
  ## ***vectors to define the indices for levels of each survey stratum***
  # days_refer:
  # vector whose length is the number of types of day to use when type of day is a dimension of interest
  days_refer <- c(1, 2)

  # charters_refer:
  # vector whose length is the number of charter statuses to use when charter status is a dimension of interest
  charters_refer <- c(1, 2)
  
  # ports_refer:
  # vector whose length is the number of ports to use when port is a dimension of interest
  # the length is obtained by finding all of the different port keys referenced in data
  ports_refer <- sort(unique(c(sample_days_current_year$PORT_FK, interviews_current_year$PORT_FK, bl_current_year$PORT_FK, p1_current_year$PORT_FK)), decreasing = FALSE)
  
  # methods_refer:
  # vector whose length is the number of methods to use when method is a dimension of interest
  # the length is obtained by finding all of the different method keys referenced in data
  methods_refer <- sort(unique(c(interviews_current_year$METHOD_FK, bl_current_year$METHOD_FK, p1_current_year$METHOD_FK)), decreasing = FALSE)
  
  
  # remove duplicate columns
  bl_current_year = bl_current_year[!duplicated(names(bl_current_year))]
  # create columns for the strata indices to make later manipulations easier
  sample_days_current_year = mutate(sample_days_current_year, port = match(.data$PORT_FK, ports_refer), day = get_days(.data$TYPE_OF_DAY))
  bl_current_year = mutate(bl_current_year, port = match(.data$PORT_FK, ports_refer), day = get_days(.data$TYPE_OF_DAY), method = match(.data$METHOD_FK, methods_refer), charter = get_charters(.data$CHARTER_F))
  p1_current_year = mutate(p1_current_year, port = match(.data$PORT_FK, ports_refer), method = match(.data$METHOD_FK, methods_refer))
  
  #***Total days***
  # [TYPE_OF_DAY]
  days = c(sum(days_current_year$NUM_WD), sum(days_current_year$NUM_WE))
  
  #***Sample days***
  # [PORT_FK, TYPE_OF_DAY]
  sample_days = df_to_array(summarise(group_by(sample_days_current_year, .data$port, .data$day), count = n()), list(ports_refer, days_refer))
  
  #***Boat log***
  # [PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F]
  # number of trips with known fishing method
  bl = array(0, c(length(ports_refer), length(days_refer), length(methods_refer), length(charters_refer)))
  # sum of squared number of trips with known fishing method, aggregated by sample date
  bl2 = array(0, c(length(ports_refer), length(days_refer), length(methods_refer), length(charters_refer)))
  # [PORT_FK, TYPE_OF_DAY, CHARTER_F]
  # number of trips with unknown fishing method
  bl_unknown_method = array(0, c(length(ports_refer), length(days_refer), length(charters_refer)))
  # number of trips with known fishing status
  bl_known_fished = array(0, c(length(ports_refer), length(days_refer), length(charters_refer)))
  # number of trips with unknown fishing status
  bl_unknown_fished = array(0, c(length(ports_refer), length(days_refer), length(charters_refer)))
  
  # boat log entries with known fishing method (these entries are used in the main calculations)
  subsample_bl_days = filter(bl_current_year, .data$METHOD_FK != 0, .data$METHOD_FK != 99, .data$FISHED == "Y")
  # boat log entries with unknown fishing method
  subsample_bl_unknown_method_days = filter(bl_current_year, .data$METHOD_FK == 0 | .data$METHOD_FK == 99, .data$FISHED == "Y")
  # boat log entries with known fishing status
  subsample_bl_known_fished_days = filter(bl_current_year, !((.data$FISHED != "Y" & .data$FISHED != "N") | is.na(.data$FISHED)))
  # boat log entries with unknown fishing status
  subsample_bl_unknown_fished_days = filter(bl_current_year, (.data$FISHED != "Y" & .data$FISHED != "N") | is.na(.data$FISHED))

  if(nrow(subsample_bl_days) > 0) {
    bl = df_to_array(summarise(group_by(subsample_bl_days, .data$port, .data$day, .data$method, .data$charter), trips = n()), list(ports_refer, days_refer, methods_refer, charters_refer))
    bl2_temp = summarise(group_by(subsample_bl_days, .data$port, .data$day, .data$method, .data$charter, .data$SAMPLE_DATE), square = (n()) ^ 2) # squares of the number of trips on each sample day
    bl2_temp = aggregate(bl2_temp$square, by = list(port = bl2_temp$port, day = bl2_temp$day, method = bl2_temp$method, charter = bl2_temp$charter), FUN = sum) # sum over sample days
    bl2 = df_to_array(bl2_temp, list(ports_refer, days_refer, methods_refer, charters_refer))
  }
  
  if(nrow(subsample_bl_unknown_method_days) > 0) {
    bl_unknown_method = df_to_array(summarise(group_by(subsample_bl_unknown_method_days, .data$port, .data$day, .data$charter), trips = n()), list(ports_refer, days_refer, charters_refer))
  }

  if(nrow(subsample_bl_known_fished_days) > 0) {
    bl_known_fished = df_to_array(summarise(group_by(subsample_bl_known_fished_days, .data$port, .data$day, .data$charter), trips = n()), list(ports_refer, days_refer, charters_refer))
  }
    
  if(nrow(subsample_bl_unknown_fished_days) > 0) {
    bl_unknown_fished = df_to_array(summarise(group_by(subsample_bl_unknown_fished_days, .data$port, .data$day, .data$charter), trips = n()), list(ports_refer, days_refer, charters_refer))
  }

  #***Temporal adjustment factor p1***
  # Note: Separate adjustment factors are given for each quarter, so to compute an annual adjustment factor these quarterly factors are averaged
  # [PORT_FK, METHOD_FK]
  # sum of charter P1 values
  charter_p1 = df_to_array(summarise(group_by(p1_current_year, .data$port, .data$method), p1 = sum(.data$CHARTER_P1)), list(ports_refer, methods_refer))
  # sum of non-charter P1 values
  non_charter_p1 = df_to_array(summarise(group_by(p1_current_year, .data$port, .data$method), p1 = sum(.data$NON_CHARTER_P1)), list(ports_refer, methods_refer))
  # number of charter/non-charter P1 values
  count = df_to_array(summarise(group_by(p1_current_year, .data$port, .data$method), count = n()), list(ports_refer, methods_refer))
  # calculate average charter P1 values
  charter_p1 = charter_p1 / count
  # calculate average non-charter P1 values
  non_charter_p1 = non_charter_p1 / count
  # [PORT_FK, METHOD_FK, CHARTER_F]
  # average P1 values
  p1 = array(c(charter_p1, non_charter_p1), dim = c(length(ports_refer), length(methods_refer), 2))
  
  #***Spatial adjustment factor p2***
  iwc_representative = 0
  iwc_unsampled = 0
  if(year < 1989) { # Agana is the only sampled port and reference port
    iwc_representative = sum((filter(iwc_current_year, .data$PORT_FK == 1))$NUM_VESSEL)
    iwc_unsampled = sum((filter(iwc_current_year, .data$PORT_FK != 1))$NUM_VESSEL)
    # The following line computes iwc_unsampled the same way as the VFP code, which is incorrect
#    iwc_unsampled = sum((filter(iwc_current_year, .data$PORT_FK > 3))$NUM_VESSEL)
  }
  else if(year < 1995) { # Agana and Merizo are the sampled ports and reference ports
    iwc_representative = sum((filter(iwc_current_year, .data$PORT_FK == 1 | .data$PORT_FK == 3))$NUM_VESSEL)
    iwc_unsampled = sum((filter(iwc_current_year, .data$PORT_FK != 1 & .data$PORT_FK != 3))$NUM_VESSEL)
    # The following line computed iwc_unsampled the same way as the VFP code, which is incorrect
#    iwc_unsampled = sum((filter(iwc_current_year, .data$PORT_FK > 3))$NUM_VESSEL)
  }
  else { # Agana, Agat, and Merizo are the sampled ports and Agat and Merizo are the reference ports
    iwc_representative = sum((filter(iwc_current_year, .data$PORT_FK == 2 | .data$PORT_FK == 3))$NUM_VESSEL)
    iwc_unsampled = sum((filter(iwc_current_year, .data$PORT_FK > 3))$NUM_VESSEL)
  }
  p2 = ifelse(iwc_representative > 0, iwc_unsampled / iwc_representative, 0)
  
  res = df_expansion(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, interviews_current_year, interviews_raw, catch_current_year, catch_raw, methods_refer, ports_refer, reference_raw)
  
  return(res)
}


#' Run Expansion Algorithm 
#' 
#' @note Whether to pool interviews when fewer than 3 are available for a stratum
#'
#' @param pool_f defaults to T, which indicates that interview pooling is used; change to F if pooling should not be used
#' @param species NA to expand for all species, or a vector of SPECIES_PK to only include specific species
#' @param start_year the first year of the expansion to run (earliest available year is 1982)
#' @param end_year the last year of the expansion to run (most recent available year is 2019)
#'
#' @return List containing 2 data frames:
#' \describe{
#' \item{bb_exp}{Total catch for each estimation domain}
#' \item{bb_spc}{Total catch by species for each estimation domain}
#' } 
#'
#' @export
#' 
run_expalg <- function(pool_f=TRUE,
      species=NA, 
      start_year=1982,
      end_year=2019) {
  expansion = data.frame()
  species_composition = data.frame()
  for(year in start_year:end_year) {
    res = run_expansion(year, as.logical(pool_f), species)
    expansion = rbind(expansion, res[[1]])
    species_composition = rbind(species_composition, res[[2]])
  }
  
  return(list(bb_exp=expansion,bb_spc=species_composition))
}