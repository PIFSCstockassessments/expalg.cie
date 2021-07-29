
#' Expansion and Species Composition of an expansion domain
#'
#' @param year Year of the expansion
#' @param pool_f Whether to pool interviews when fewer than 3 are available for a domain
#' @param species a vector of species or all species (when spcies = NA)
#' @param bl boat log entries ...
#' @param bl2 squared counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])
#' @param bl_unknown_method counts of boat log entries with unknown fishing method for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_unknown_fished counts of boat log entries with unknown fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_known_fished counts of boat log entries with known fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param p1 adjustment factors to account for missed vessels outside of shift times for each domain ([PORT_FK, METHOD_FK, CHARTER_F])
#' @param p2 adjustment factor to account for vessels outside of the sampled ports
#' @param sample_days the number of sample days for each domain ([PORT_FK, TYPE_OF_DAY])
#' @param days the number of calendar days for each domain ([TYPE_OF_DAY])
#' @param interviews interviews for the expansion period
#' @param interviews_raw all interviews for all years, for use when pooling
#' @param catch interview catch data for the expansion period
#' @param catch_raw interview catch data for all years
#' @param methods_refer vector to convert method keys to method indices
#' @param ports_refer vector to convert port keys to port indices
#' @param reference reference values for use when insufficient interviews are available for a domain
#' 
#' @return Two data frames containing all expansion and species composition fields for each expansion domain
#' @export
#' 
#' @importFrom rlang .data
#' 
df_expansion = function(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, interviews, interviews_raw, catch, catch_raw, methods_refer, ports_refer, reference) {
  expansion = data.frame()
  species_composition = data.frame()
  
  for(m in 1:dim(bl)[3]) { # go through methods
    res = df_method_expansion(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, interviews, interviews_raw, catch, catch_raw, m, methods_refer[m], ports_refer, filter(reference, .data$METHOD_FK == methods_refer[m]))
    
    expansion = rbind(expansion, res[[1]])
    species_composition = rbind(species_composition, res[[2]])
  }

  return(list(expansion, species_composition))
}

#' Survey Zone key corresponding to port
#' 
#' @param port_fk Single integer for port key
#' 
#' @return The survey zone key corresponding to the input port key
get_survey_zone = function(port_fk) {
  if(port_fk == 1) {
    # Agana Boat Basin
    22 # SURVEY_ZONE_PK for Agana Boat Basin-A1994
  }
  else if(port_fk == 2) {
    # Agat Marina
    23 # SURVEY_ZONE_PK for Agat Marina-A1994
  }
  else if(port_fk == 3) {
    # Merizo Pier
    21 # SURVEY_ZONE_PK for Merizo Pier-A1994
  }
  else {
    warning(paste("Port ", port_fk, " not valid"))
    -1
  }
}

#' Variance of the mean of a Average
#' 
#' @param x A vector of values
#' 
#' @returns Variance of the average of those values
#' @importFrom stats var
#' 
#' @export
#' 
calc_var_average = function(x) {
  x = x[is.finite(x)]
  return(var(x)/length(x))
}

#' Variance of the quotient of two variables
#' 
#' @param x vector of values, of the same length of y
#' @param y vector of values, of the same length of x
#' 
#' @importFrom stats var cov
#' @return The variance of the quotient of the two variables (variable corresponding to first vector divided by variable
#'         corresponding to second vector)
#' 
#' @export
#'          
calc_var_quotient = function(x, y) {
  good_indices = is.finite(x) && is.finite(y)
  x = x[good_indices]
  y = y[good_indices]
  mean1 = mean(x)
  mean2 = mean(y)
  var1 = var(x)/length(x)
  var2 = var(y)/length(y)
  cov12 = cov(x, y)/sqrt(length(x)*length(y))
  
  return((mean1 / mean2) ^ 2 * (var1 / (mean1) ^ 2 - 2 * cov12 / (mean1 * mean2) + var2 / (mean2) ^ 2))
}
#the function calc_var_quotient is modified in May 2019 by Ma

#' Variance of product x and y
#' 
#' @param x Point estimate of x 
#' @param var_x Variance of x
#' @param y Point estimate of y
#' @param var_y Variance of y
#' 
#' @return the variance of the product of the two variables
#' 
#' @export
#' 
calc_var_product = function(x, var_x, y, var_y) {
  return(var_x * y ^ 2 + var_y * x ^ 2 - var_x * var_y)
}

#' Calculate expansion fields as a Data Frame
#'
#' @param year year of the expansion
#' @param pool_f whether to pool interviews when fewer than 3 are available for a domain
#' @param species a vector of species or all species (when species = NA)
#' @param bl counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])
#' @param bl2 squared counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])
#' @param bl_unknown_method counts of boat log entries with unknown fishing method for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_unknown_fished counts of boat log entries with unknown fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_known_fished counts of boat log entries with known fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param p1 adjustment factors to account for missed vessels outside of shift times for each domain ([PORT_FK, METHOD_FK, CHARTER_F])
#' @param p2 adjustment factor to account for vessels outside of the sampled ports
#' @param sample_days the number of sample days for each domain ([PORT_FK, TYPE_OF_DAY])
#' @param days the number of calendar days for each domain ([TYPE_OF_DAY])
#' @param interviews interviews for this domain
#' @param interviews_raw all interviews for all years, for use when pooling
#' @param method_index vector index of the current method
#' @param method_key database key of the current method
#' @param ports_refer vector to convert port keys to port indices
#' @param reference reference values for the current method
#' @param port_index vector index of the current port
#' @param type_of_day_index vector index of the current type of day
#' @param charter_index vector index of the current charter status
#' @param other_ports boolean for whether this expansion is for the other (unsampled) ports (survey_zone = 24)
#'
#' @return Data frame with a single row containing all expansion fields for the current domain
#' @export
#' 
calc_df = function(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, interviews, interviews_raw, method_index, method_key, ports_refer, reference, port_index, type_of_day_index, charter_index, other_ports) {
  type_of_day = ifelse(type_of_day_index == 1, "WD", "WE")
  process_type = "R" # default process type to "R" (reference) unless sufficient interviews are found to be available
  fishery_type = ifelse(charter_index == 1, "C", "N")
  port_fk = ifelse(other_ports, NA, ports_refer[port_index])
  survey_zone_fk = ifelse(other_ports, 24, get_survey_zone(port_fk))
  method_fk = method_key
  sys_create_time = Sys.time()
  sys_modify_time = Sys.time()
  hash = NA
  
  
  period_type = "Y"
  quarter = 1
  
  exp_pk = paste0(period_type,
                  year,
                  sprintf("%02d", quarter),
                  sprintf("%04d", method_fk),
                  sprintf("%03d", survey_zone_fk),
                  type_of_day,
                  fishery_type,
                  process_type)
  
  num_calendar_days = days[type_of_day_index]
  num_sample_days = sample_days[port_index, type_of_day_index]
  num_kn_method_trip = ifelse(other_ports, 0, sum(bl[port_index, type_of_day_index, , charter_index]))
  num_unkn_method_trip = ifelse(other_ports, 0, bl_unknown_method[port_index, type_of_day_index, charter_index])
  num_kn_fished_trip = ifelse(other_ports, 0, bl_known_fished[port_index, type_of_day_index, charter_index])
  num_unkn_fished_trip = ifelse(other_ports, 0, bl_unknown_fished[port_index, type_of_day_index, charter_index])
  num_trip = ifelse(other_ports, 0, bl[port_index, type_of_day_index, method_index, charter_index])
  num_trip2 = ifelse(other_ports, 0, bl2[port_index, type_of_day_index, method_index, charter_index])
  
  # Initialize the other fields to NA since, depending on the inputs, not all may be calculable
  num_interview = NA
  num_interview_pooled = NA
  num_days_fished = NA
  sum_fisher = NA
  sum_fisher2 = NA
  avg_fisher = NA
  var_avg_fisher = NA
  sum_hour = NA
  sum_hour2 = NA
  avg_hour = NA
  var_avg_hour = NA
  sum_fisher_hour = NA
  sum_fisher_hour2 = NA
  avg_fisher_hour = NA
  var_avg_fisher_hour = NA
  sum_gear = NA
  sum_gear2 = NA
  avg_gear = NA
  var_avg_gear = NA
  sum_tot_kgs = NA
  sum_tot_kgs2 = NA
  sum_est_trip = NA
  sum_est_trip2 = NA
  avg_est_trip = NA
  var_est_trip = NA
  std_est_trip = NA
  cpue_trip = NA
  var_cpue_trip = NA
  std_cpue_trip = NA
  cpue_hour = NA
  var_cpue_hour = NA
  std_cpue_hour = NA
  cpue_gear = NA
  var_cpue_gear = NA
  std_cpue_gear = NA
  cpue_fisher_hour = NA
  var_cpue_fisher_hour = NA
  std_cpue_fisher_hour = NA
  exp_trip = NA
  var_exp_trip = NA
  std_exp_trip = NA
  exp_hour = NA
  var_exp_hour = NA
  std_exp_hour = NA
  exp_fisher = NA
  var_exp_fisher = NA
  std_exp_fisher = NA
  exp_fisher_hour = NA
  var_exp_fisher_hour = NA
  std_exp_fisher_hour = NA
  exp_gear = NA
  var_exp_gear = NA
  std_exp_gear = NA
  exp_tot_kgs = NA
  var_exp_tot_kgs = NA
  std_exp_tot_kgs = NA
  flag_pooled = NA
  
  # If we can calculate an A1 value, use it. Otherwise, just use A1 = 1, which will have no effect as an adjustment factor
  a1 = ifelse(num_kn_fished_trip == 0, 1, (num_kn_fished_trip + num_unkn_fished_trip) / num_kn_fished_trip)
  # If we can calculate an A2 value, use it. Otherwise, just use A2 = 1, which will have no effect as an adjustment factor
  a2 = ifelse(num_kn_method_trip == 0, 1, (num_kn_method_trip + num_unkn_method_trip) / num_kn_method_trip)
  # If a valid P1 value is available, use it. Otherwise, just use P1 = 1, which will have no effect as an adjustment factor
  p1_val = 0
  if(!is.nan(p1[port_index, method_index, charter_index]) && p1[port_index, method_index, charter_index] > 0) {
    p1_val = p1[port_index, method_index, charter_index]
  }
  else {
    if(fishery_type == "C") {
      p1_val = 1
    }
    else if(!is.na(port_fk)){
      if(port_fk == 1) {
        p1_val = 0.98
      }
      else if(port_fk == 2) {
        p1_val = 0.95
      }
      else if(port_fk == 3) {
        p1_val = 0.8
      }
      else {
        p1_val = 1
      }
    }
    else {
      p1_val = 1
    }
  }

  num_interview = nrow(interviews)
  num_interview_no_pool = num_interview
  
  if(num_interview >= 3) {
    # If sufficient interviews are available
    
    process_type = "G"
  }
  else if(num_interview < 3 && pool_f) {
    # If insufficient interviews are available and pooling is to be used
    
    # Call the pooling function and get the pooled interviews back
   # interviews_pooled = pool_interviews(interviews_raw, year, method_key, type_of_day_index, ports_refer[port_index], charter_index, FALSE)[[1]]
    interviews_pooled = pool_interviews(interviews_raw, year, method_key, type_of_day_index, port_fk, charter_index, other_ports)[[1]]
    flag_pooled = pool_interviews(interviews_raw, year, method_key, type_of_day_index, port_fk, charter_index, other_ports)[[2]]
    # Ma changed the last argument (above) from FALSE to other_ports and ports_refer[port_index] to port_fk 
    if(is.data.frame(interviews_pooled) && nrow(interviews_pooled) >= 3) {
      # If pooling was able to generate at least 3 interviews
      process_type = "G"
      interviews = interviews_pooled
      num_interview_pooled = nrow(interviews)
      print("port_fk&index"); print(c(port_fk, ports_refer[port_index])) # use port_fk, not ports_refer[port_index], to track ports because other_ports were also assgined a port_index ranging from 1 to 2
      print("Pooled interview # for EXP"); print(num_interview_pooled)
      num_interview = num_interview_pooled # use pooled interviews for all calculations
    }
  }
  
  if(process_type == "R" && nrow(reference) != 0) {
    # If we are using reference values and these reference values are available
    # the reference table could be updated
    num_interview = reference$NUM_INT
    num_days_fished = 1
    sum_fisher = reference$SUM_FISHER
    sum_fisher2 = sum_fisher ^ 2
    avg_fisher = sum_fisher / num_interview
    sum_hour = reference$SUM_HOUR
    sum_hour2 = sum_hour ^ 2
    avg_hour = sum_hour / num_interview
    sum_fisher_hour = reference$SUM_FISHER_HOUR
    sum_fisher_hour2 = sum_fisher_hour ^ 2
    avg_fisher_hour = sum_fisher_hour / num_interview
    sum_gear = 1
    sum_gear2 = sum_gear ^ 2
    avg_gear = sum_gear / num_interview
    sum_tot_kgs = reference$SUM_KGS
    sum_tot_kgs2 = sum_tot_kgs ^ 2
    sum_est_trip = num_trip * a1 * a2 / p1_val
    if(other_ports && fishery_type != "C") {
      sum_est_trip = bl[port_index, type_of_day_index, method_index, charter_index] * a1 * a2 / p1_val * p2
    }
    sum_est_trip2 = num_trip2 * (a1 * a2 / p1_val) ^ 2
    if(other_ports && fishery_type != "C") {
      sum_est_trip2 = bl2[port_index, type_of_day_index, method_index, charter_index] * (a1 * a2 / p1_val * p2) ^ 2
    }
    avg_est_trip = sum_est_trip / num_sample_days
    if(num_sample_days > 1) {
      var_est_trip = (sum_est_trip2 - sum_est_trip ^ 2 / num_sample_days) / (num_sample_days * (num_sample_days - 1))
      std_est_trip = 100 * var_est_trip ^ 0.5 / avg_est_trip
    }
    cpue_trip = sum_tot_kgs / num_interview
    cpue_hour = sum_tot_kgs / sum_hour
    cpue_gear = sum_tot_kgs / sum_gear
    cpue_fisher_hour = sum_tot_kgs / sum_fisher_hour
    exp_trip = avg_est_trip * num_calendar_days
    if(num_sample_days > 1) {
      var_exp_trip = var_est_trip * num_calendar_days ^ 2
      std_exp_trip = 100 * var_exp_trip ^ 0.5 / exp_trip # This is 100* coefficient of variation (CV)
    }
    exp_hour = avg_hour * exp_trip
    exp_fisher = avg_fisher * exp_trip
    exp_fisher_hour = avg_fisher_hour * exp_trip
    exp_gear = avg_gear * exp_trip
    exp_tot_kgs = cpue_trip * exp_trip
  }
  else if(process_type == "G") {
    # If we are using the available interviews for the domain
    
    num_days_fished = sum(interviews$NUM_DAYS_FISHED)
    sum_fisher = sum(interviews$NUM_FISHER)
    sum_fisher2 = sum(interviews$NUM_FISHER ^ 2)
    avg_fisher = sum_fisher / num_interview
    sum_hour = sum(interviews$HOURS_FISHED)
    sum_hour2 = sum(interviews$HOURS_FISHED ^ 2)
    avg_hour = sum_hour / num_interview
    sum_fisher_hour = sum(interviews$NUM_FISHER * interviews$HOURS_FISHED)
    sum_fisher_hour2 = sum((interviews$NUM_FISHER * interviews$HOURS_FISHED) ^ 2)
    avg_fisher_hour = sum_fisher_hour / num_interview
    sum_gear = sum(interviews$NUM_GEAR)
    sum_gear2 = sum(interviews$NUM_GEAR ^ 2)
    avg_gear = sum_gear / num_interview
    sum_tot_kgs = sum(interviews$TOT_EST_KGS)
    sum_tot_kgs2 = sum(interviews$TOT_EST_KGS ^ 2)
    sum_est_trip = num_trip * a1 * a2 / p1_val
    if(other_ports && fishery_type != "C") {
      sum_est_trip = bl[port_index, type_of_day_index, method_index, charter_index] * a1 * a2 / p1_val * p2
    }
    sum_est_trip2 = num_trip2 * (a1 * a2 / p1_val) ^ 2
    if(other_ports && fishery_type != "C") {
      sum_est_trip2 = bl2[port_index, type_of_day_index, method_index, charter_index] * (a1 * a2 / p1_val * p2) ^ 2
    }
    avg_est_trip = sum_est_trip / num_sample_days
    if(num_sample_days > 1) {
      var_est_trip = (sum_est_trip2 - sum_est_trip ^ 2 / num_sample_days) / (num_sample_days * (num_sample_days - 1))
      std_est_trip = 100 * var_est_trip ^ 0.5 / avg_est_trip
    }
    cpue_trip = sum_tot_kgs / num_interview
    cpue_hour = sum_tot_kgs / sum_hour
    cpue_gear = sum_tot_kgs / sum_gear
    cpue_fisher_hour = sum_tot_kgs / sum_fisher_hour
    exp_trip = avg_est_trip * num_calendar_days
    if(num_sample_days > 1) {
      var_exp_trip = var_est_trip * num_calendar_days ^ 2
      std_exp_trip = 100 * var_exp_trip ^ 0.5 / exp_trip
    }
    exp_hour = avg_hour * exp_trip
    exp_fisher = avg_fisher * exp_trip
    exp_fisher_hour = avg_fisher_hour * exp_trip
    exp_gear = avg_gear * exp_trip
    exp_tot_kgs = cpue_trip * exp_trip
      
    var_avg_fisher = calc_var_average(interviews$NUM_FISHER)
    var_avg_hour = calc_var_average(interviews$HOURS_FISHED)
    var_avg_fisher_hour = calc_var_average(interviews$NUM_FISHER * interviews$HOURS_FISHED)
    var_avg_gear = calc_var_average(interviews$NUM_GEAR)
    var_cpue_trip = calc_var_average(interviews$TOT_EST_KGS)
    std_cpue_trip = 100 * var_cpue_trip ^ 0.5 / cpue_trip # "std" = 100*CV in the block here
    var_cpue_hour = calc_var_quotient(interviews$TOT_EST_KGS, interviews$HOURS_FISHED)
    std_cpue_hour = 100 * var_cpue_hour ^ 0.5 / cpue_hour
    var_cpue_gear = calc_var_quotient(interviews$TOT_EST_KGS, interviews$NUM_GEAR)
    std_cpue_gear = 100 * var_cpue_gear ^ 0.5 / cpue_gear
    var_cpue_fisher_hour = calc_var_quotient(interviews$TOT_EST_KGS, interviews$NUM_FISHER * interviews$HOURS_FISHED)
    std_cpue_fisher_hour = 100 * var_cpue_fisher_hour ^ 0.5 / cpue_fisher_hour
    var_exp_hour = calc_var_product(exp_trip, var_exp_trip, avg_hour, var_avg_hour)
    std_exp_hour = 100 * var_exp_hour ^ 0.5 / exp_hour
    var_exp_fisher = calc_var_product(exp_trip, var_exp_trip, avg_fisher, var_avg_fisher)
    std_exp_fisher = 100 * var_exp_fisher ^ 0.5 / exp_fisher
    var_exp_fisher_hour = calc_var_product(exp_trip, var_exp_trip, avg_fisher_hour, var_avg_fisher_hour)
    std_exp_fisher_hour = 100 * var_exp_fisher_hour ^ 0.5 / exp_fisher_hour
    var_exp_gear = calc_var_product(exp_trip, var_exp_trip, avg_gear, var_avg_gear)
    std_exp_gear = 100 * var_exp_gear ^ 0.5 / exp_gear
    var_exp_tot_kgs = calc_var_product(exp_trip, var_exp_trip, cpue_trip, var_cpue_trip)
    std_exp_tot_kgs = 100 * var_exp_tot_kgs ^ 0.5 / exp_tot_kgs
    
    if(!is.na(num_interview_pooled)) {
      # If we pooled interviews, num_interview currently represents the pooled number of interviews since
      # that number was needed in the above computations
      
      num_interview = num_interview_no_pool # restore num_interview to the original number of interviews without pooling
    }
  }
  
  # Create the data frame to return
  df = data.frame(EXP_PK = exp_pk,
                  PERIOD_TYPE = period_type,
                  YEAR = year,
                  QUARTER = quarter,
                  TYPE_OF_DAY = type_of_day,
                  PROCESS_TYPE = process_type,
                  FISHERY_TYPE = fishery_type,
                  PORT_FK = port_fk,
                  SURVEY_ZONE_FK = survey_zone_fk,
                  METHOD_FK = method_fk,
                  NUM_CALENDAR_DAYS = num_calendar_days,
                  NUM_SAMPLE_DAYS = num_sample_days,
                  NUM_INTERVIEW = num_interview,
                  NUM_INTERVIEW_POOLED = num_interview_pooled,
                  NUM_DAYS_FISHED = num_days_fished,
                  NUM_TRIP = num_trip,
                  NUM_KN_METHOD_TRIP = num_kn_method_trip,
                  NUM_UNKN_METHOD_TRIP = num_unkn_method_trip,
                  NUM_KN_FISHED_TRIP = num_kn_fished_trip,
                  NUM_UNKN_FISHED_TRIP = num_unkn_fished_trip,
                  SUM_EST_TRIP = sum_est_trip,
                  SUM_EST_TRIP2 = sum_est_trip2,
                  SUM_HOUR = sum_hour,
                  SUM_HOUR2 = sum_hour2,
                  SUM_FISHER = sum_fisher,
                  SUM_FISHER2 = sum_fisher2,
                  SUM_FISHER_HOUR = sum_fisher_hour,
                  SUM_FISHER_HOUR2 = sum_fisher_hour2,
                  SUM_GEAR = sum_gear,
                  SUM_GEAR2 = sum_gear2,
                  SUM_TOT_KGS = sum_tot_kgs,
                  SUM_TOT_KGS2 = sum_tot_kgs2,
                  AVG_FISHER = avg_fisher,
                  AVG_HOUR = avg_hour,
                  AVG_FISHER_HOUR = avg_fisher_hour,
                  AVG_GEAR = avg_gear,
                  AVG_EST_TRIP = avg_est_trip,
                  EXP_TRIP = exp_trip,
                  EXP_HOUR = exp_hour,
                  EXP_FISHER = exp_fisher,
                  EXP_FISHER_HOUR = exp_fisher_hour,
                  EXP_GEAR = exp_gear,
                  EXP_TOT_KGS = exp_tot_kgs,
                  CPUE_TRIP = cpue_trip,
                  CPUE_HOUR = cpue_hour,
                  CPUE_FISHER_HOUR = cpue_fisher_hour,
                  CPUE_GEAR = cpue_gear,
                  VAR_AVG_FISHER = var_avg_fisher,
                  VAR_AVG_HOUR = var_avg_hour,
                  VAR_AVG_FISHER_HOUR = var_avg_fisher_hour,
                  VAR_AVG_GEAR = var_avg_gear,
                  VAR_EST_TRIP = var_est_trip,
                  VAR_EXP_TRIP = var_exp_trip,
                  VAR_EXP_HOUR = var_exp_hour,
                  VAR_EXP_FISHER = var_exp_fisher,
                  VAR_EXP_FISHER_HOUR = var_exp_fisher_hour,
                  VAR_EXP_GEAR = var_exp_gear,
                  VAR_EXP_TOT_KGS = var_exp_tot_kgs,
                  VAR_CPUE_TRIP = var_cpue_trip,
                  VAR_CPUE_HOUR = var_cpue_hour,
                  VAR_CPUE_GEAR = var_cpue_gear,
                  VAR_CPUE_FISHER_HOUR = var_cpue_fisher_hour,
                  STD_EST_TRIP = std_est_trip,
                  STD_EXP_TRIP = std_exp_trip,
                  STD_EXP_HOUR = std_exp_hour,
                  STD_EXP_FISHER = std_exp_fisher,
                  STD_EXP_FISHER_HOUR = std_exp_fisher_hour,
                  STD_EXP_GEAR = std_exp_gear,
                  STD_EXP_TOT_KGS = std_exp_tot_kgs,
                  STD_CPUE_TRIP = std_cpue_trip,
                  STD_CPUE_HOUR = std_cpue_hour,
                  STD_CPUE_GEAR = std_cpue_gear,
                  STD_CPUE_FISHER_HOUR = std_cpue_fisher_hour,
                  SYS_CREATE_TIME = sys_create_time,
                  SYS_MODIFY_TIME = sys_modify_time,
                  FLAG_POOLED = flag_pooled,
                  HASH = hash)
  
  return(df)
}

#' Expansion and Species composition of a fishing method
#'
#' @param year year of the expansion
#' @param pool_f whether to pool interviews when fewer than 3 are available for a domain
#' @param species a vector of species or all species (when species = NA)
#' @param bl counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])
#' @param bl2 squared counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])
#' @param bl_unknown_method counts of boat log entries with unknown fishing method for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_unknown_fished counts of boat log entries with unknown fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param bl_known_fished = counts of boat log entries with known fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])
#' @param p1 adjustment factors to account for missed vessels outside of shift times for each domain ([PORT_FK, METHOD_FK, CHARTER_F])
#' @param p2 adjustment factor to account for vessels outside of the sampled ports
#' @param sample_days the number of sample days for each domain ([PORT_FK, TYPE_OF_DAY])
#' @param days the number of calendar days for each domain ([TYPE_OF_DAY])
#' @param interviews interviews for the expansion period
#' @param interviews_raw all interviews for all years, for use when pooling
#' @param catch interview catch data for the expansion period
#' @param catch_raw interview catch data for all years
#' @param method_index vector index of the current method
#' @param method_key database key of the current method
#' @param ports_refer vector to convert port keys to port indices
#' @param reference = reference values for the current method
#' 
#' @importFrom dplyr filter
#' @return Two data frames with rows containing all expansion and species composition fields for each domain of the current method
#' @export
#' 
#' @importFrom rlang .data
#' @importFrom dplyr summarise group_by n inner_join
#' 
df_method_expansion = function(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, interviews, interviews_raw, catch, catch_raw, method_index, method_key, ports_refer, reference) {
  domain_interviews_m = filter(interviews, .data$METHOD_FK == method_key) # restrict interviews to the current method
  
  expansion = data.frame()
  species_composition = data.frame()
  
	dimensions = dim(bl_unknown_method)
	
	sampled_ports = c()
	if(year < 1989) {
	  sampled_ports = c(1)
	}
	else if(year < 1995) {
	  sampled_ports = c(1, 3)
	}
	else {
	  sampled_ports = c(1, 2, 3)
	}
	
	for(i in 1:dimensions[1]) { # Go through the ports
		if(ports_refer[i] %in% sampled_ports) {
		  # If the current port is one of the sampled ports
		  
		  domain_interviews_mp = filter(domain_interviews_m, .data$PORT_FK == ports_refer[i]) # restrict interviews to the current method and port
      for(j in 1:dimensions[2]) { # Go through the types of day
			  domain_interviews_mpd = filter(domain_interviews_mp, .data$TYPE_OF_DAY == ifelse(j == 1, "WD", "WE")) # restrict interviews to the current method, port, and type of day
				for(k in 1:dimensions[3]) { # Go through the charter statuses
					if(sum(bl[i, j, , k]) > 0 && sample_days[i, j] > 0) { # If data has been collected for the current domain
					  domain_interviews_mpdc = filter(domain_interviews_mpd, .data$CHARTER_F == ifelse(k == 1, T, F)) #!! For CHARTER_F, change "T" and "F" to T and F with csv input files
					  num_interview = nrow(domain_interviews_mpdc)
					  
					  df_g = calc_df(year, pool_f, species, bl, bl2, bl_unknown_method, bl_unknown_fished, bl_known_fished, p1, p2, sample_days, days, domain_interviews_mpdc, interviews_raw, method_index, method_key, ports_refer, reference, i, j, k, F)
				    expansion = rbind(expansion, df_g)
				    
				    
				    #added block in May 2019 for pooling intervews for SPC
				    if(num_interview < 3 && pool_f) { # num_interview is restored to the number of interview_nopool at the end of calc_df
				      # If insufficient interviews are available and pooling is to be used
				      
				      # Call the pooling function and get the pooled interviews back
				      interviews_pooled = pool_interviews(interviews_raw, year, method_key, j, ports_refer[i], k, FALSE)[[1]] # j and k are type of day index and charter index
				      if(is.data.frame(interviews_pooled) && nrow(interviews_pooled) >= 1) { # keep any pooling results for SPC and changed >=3 to >= 1
				        # If pooling was able to generate at least 3 interviews
				       # process_type = "G"
				        domain_interviews_mpdc = interviews_pooled
				        print("SPCpoolingPort&Interview#"); print(ports_refer[i]); print(nrow(domain_interviews_mpdc))# print # of rows to make sure pooling works
				       num_interview = nrow(domain_interviews_mpdc)
				      }
				    }
				    
				    
				    
					  catch_df = data.frame() # species catch information
					  if(num_interview > 0) {
					    for(l in 1:num_interview) {
					      catch_interview = filter(catch_raw, .data$INTERVIEW_FK == domain_interviews_mpdc[l,]$INTERVIEW_PK) # all species catch data from all matched interviews
					      
					      catch_df = rbind(catch_df, catch_interview)
					    }
					  }
					  
					  if(nrow(catch_df) > 0) {
					    # if there was any catch for the current domain interviews
					    
					    # 6/23/20 added by TM
					    species_df = data.frame()
					    species_list = unique(catch_df$SPECIES_FK)
					    if(is.na(species)) {
					      num_interview = nrow(domain_interviews_mpdc)
					      sum_hour = sum(domain_interviews_mpdc$HOURS_FISHED)
					      sum_gear = sum(domain_interviews_mpdc$NUM_GEAR)
					      sum_fisher_hour = sum(domain_interviews_mpdc$NUM_FISHER * domain_interviews_mpdc$HOURS_FISHED)
					      
					      for(l in 1:length(species_list)) {
					        specie = species_list[l]
					        
					        all_catch_specie = rep.int(0, nrow(domain_interviews_mpdc))
					        for(m in 1:nrow(domain_interviews_mpdc)) {
					          catch_entries = filter(catch_df, .data$SPECIES_FK == specie & .data$INTERVIEW_FK == domain_interviews_mpdc[m, "INTERVIEW_PK"])
					          if(nrow(catch_entries) > 0) {
					            all_catch_specie[m] = sum(catch_entries$EST_KGS)
					          }
					        }
					        
					        sum_tot_kgs = sum(all_catch_specie)
					        cpue_trip = sum_tot_kgs / num_interview
					        cpue_hour = sum_tot_kgs / sum_hour
					        cpue_gear = sum_tot_kgs / sum_gear
					        cpue_fisher_hour = sum_tot_kgs / sum_fisher_hour
					        var_cpue_trip = calc_var_average(all_catch_specie)
					        std_cpue_trip = 100 * var_cpue_trip ^ 0.5 / cpue_trip # std = 100*CV
					        var_cpue_hour = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$HOURS_FISHED)
					        std_cpue_hour = 100 * var_cpue_hour ^ 0.5 / cpue_hour
					        var_cpue_gear = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$NUM_GEAR)
					        std_cpue_gear = 100 * var_cpue_gear ^ 0.5 / cpue_gear
					        var_cpue_fisher_hour = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$NUM_FISHER * domain_interviews_mpdc$HOURS_FISHED)
					        std_cpue_fisher_hour = 100 * var_cpue_fisher_hour ^ 0.5 / cpue_fisher_hour
					        # added on July 2, 2020
					        var_kgs_caught = calc_var_product(df_g$EXP_TRIP, df_g$VAR_EXP_TRIP, cpue_trip, var_cpue_trip)
					        species_df = rbind(species_df, data.frame(SPECIES_FK = specie,
					                                                  CPUE_TRIP = cpue_trip,
					                                                  CPUE_HOUR = cpue_hour,
					                                                  CPUE_GEAR = cpue_gear,
					                                                  CPUE_FISHER_HOUR = cpue_fisher_hour,
					                                                  VAR_CPUE_TRIP = var_cpue_trip,
					                                                  VAR_KGS_CAUGHT = var_kgs_caught,
					                                                  STD_CPUE_TRIP = std_cpue_trip,
					                                                  VAR_CPUE_HOUR = var_cpue_hour,
					                                                  STD_CPUE_HOUR = std_cpue_hour,
					                                                  VAR_CPUE_GEAR = var_cpue_gear,
					                                                  STD_CPUE_HOUR = std_cpue_hour,
					                                                  VAR_CPUE_FISHER_HOUR = var_cpue_fisher_hour,
					                                                  STD_CPUE_FISHER_HOUR = std_cpue_fisher_hour))
					      }
					    }
					    
					    # group catch by species and calculate summary fields
  					  catch_by_species = summarise(group_by(catch_df, .data$SPECIES_FK), num_interview = n(), kgs_caught = sum(.data$EST_KGS), num_kept = sum(.data$NUM_KEPT), price_lb = mean(.data$PRICE_LB))
					  
  					  species_composition_domain = data.frame(SPC_PK = paste0(substring(df_g$EXP_PK, 1, 14),
  					                                                           sprintf("%05d", catch_by_species$SPECIES_FK),
  					                                                           substring(df_g$EXP_PK, 15, 18)),
  					                                           EXP_FK = df_g$EXP_PK,
  					                                           FLAG_POOLED = df_g$FLAG_POOLED,
  					                                           SPECIES_FK = catch_by_species$SPECIES_FK,
  					                                           NUM_INTERVIEW = catch_by_species$num_interview,
  					                                           NUM_INTERVIEW_POOLED = num_interview,
  					                                           KGS_CAUGHT = catch_by_species$kgs_caught / sum(catch_by_species$kgs_caught) * df_g$EXP_TOT_KGS,
#  					                                           VAR_LBS_CAUGHT = NA,
#  					                                           STD_LBS_CAUGHT = NA,
#  					                                           LBS_SOLD = NA,
  					                                           NUM_KEPT = catch_by_species$num_kept,
  					                                           PRICE_LB = catch_by_species$price_lb,
  					                                           SYS_CREATE_TIME = Sys.time(),
  					                                           SYS_MODIFY_TIME = Sys.time(),
  					                                           HASH = NA)
  					  
  					  # 6/23/20 added by TM
  					  if(is.na(species)) {
  					    species_composition_domain = inner_join(species_composition_domain, species_df, by = c("SPECIES_FK" = "SPECIES_FK"))
  					  }
  					    
  					  species_composition = rbind(species_composition, species_composition_domain)
					  }
					}
				}
			}
		}
	}
	
	for(j in 1:dimensions[2]) { # Go through the types of day
	  # These 'rep' values will hold the combined data for the representative ports
	  bl_rep = bl
	  bl2_rep = bl2
	  bl_unknown_method_rep = bl_unknown_method
	  bl_unknown_fished_rep = bl_unknown_fished
	  bl_known_fished_rep = bl_known_fished
	  p1_rep = p1
	  sample_days_rep = sample_days
	  port1_index = which(ports_refer == 1) # array index for port 1 (Agana)
	  port2_index = which(ports_refer == 2) # array index for port 2 (Agat)
	  port3_index = which(ports_refer == 3) # array index for port 3 (Merizo)
	  
	  port_index = 0 # array index to use information from in the expansion
	  domain_interviews_mpd = NA
	  
	  if(year < 1989) { # Agana is the only sampled port and reference port
	    port_index = port1_index
	    domain_interviews_mpd = filter(domain_interviews_m, .data$PORT_FK == 1, .data$TYPE_OF_DAY == ifelse(j == 1, "WD", "WE")) # restrict interviews to the current method, port, and type of day
	  }
	  else if(year < 1995) { # Agana and Merizo are the sampled ports and reference ports
	    # Combine the data for PORT_FK = 1 and PORT_FK = 3 into PORT_FK = 1, and make entries for PORT_FK = 3 zero.
	    # This allows use to use the same method as with the single-port expansions, by simply passing it port key 1 as 
	    # the port parameter.
	    bl_rep[port1_index, , ,] = bl[port1_index, , ,] + bl[port3_index, , ,]
	    bl_rep[port3_index, , ,] = 0
	    bl2_rep[port1_index, , ,] = bl2[port1_index, , ,] + bl2[port3_index, , ,]
	    bl2_rep[port3_index, , ,] = 0
	    bl_unknown_method_rep[port1_index, ,] = bl_unknown_method[port1_index, ,] + bl_unknown_method[port3_index, ,]
	    bl_unknown_method_rep[port3_index, ,] = 0
	    bl_unknown_fished_rep[port1_index, ,] = bl_unknown_fished[port1_index, ,] + bl_unknown_fished[port3_index, ,]
	    bl_unknown_fished_rep[port3_index, ,] = 0
	    bl_known_fished_rep[port1_index, ,] = bl_known_fished[port1_index, ,] + bl_known_fished[port3_index, ,]
	    bl_known_fished_rep[port3_index, ,] = 0
	    p1_rep[port1_index, ,] = (p1[port1_index, ,] + p1[port3_index, ,]) / 2
	    p1_rep[port3_index, ,] = 0
	    sample_days_rep[port1_index,] = max(sample_days[port1_index,], sample_days[port3_index,])
	    sample_days_rep[port3_index,] = 0
	    
	    port_index = port1_index
	    domain_interviews_mpd = filter(domain_interviews_m, .data$PORT_FK == 1 | .data$PORT_FK == 3, .data$TYPE_OF_DAY == ifelse(j == 1, "WD", "WE")) # restrict interviews to the current method, port, and type of day
	  }
	  else { # Agana, Agat, and Merizo are the sampled ports and Agat and Merizo are the reference ports
  	  # Combine the data for PORT_FK = 2 and PORT_FK = 3 into PORT_FK = 2, and make entries for PORT_FK = 3 zero.
  	  # This allows use to use the same method as with the single-port expansions, by simply passing it port key 2 as 
  	  # the port parameter.
  	  bl_rep[port2_index, , ,] = bl[port2_index, , ,] + bl[port3_index, , ,]
  	  bl_rep[port3_index, , ,] = 0
  	  bl2_rep[port2_index, , ,] = bl2[port2_index, , ,] + bl2[port3_index, , ,]
  	  bl2_rep[port3_index, , ,] = 0
  	  bl_unknown_method_rep[port2_index, ,] = bl_unknown_method[port2_index, ,] + bl_unknown_method[port3_index, ,]
  	  bl_unknown_method_rep[port3_index, ,] = 0
  	  bl_unknown_fished_rep[port2_index, ,] = bl_unknown_fished[port2_index, ,] + bl_unknown_fished[port3_index, ,]
  	  bl_unknown_fished_rep[port3_index, ,] = 0
  	  bl_known_fished_rep[port2_index, ,] = bl_known_fished[port2_index, ,] + bl_known_fished[port3_index, ,]
  	  bl_known_fished_rep[port3_index, ,] = 0
  	  p1_rep[port2_index, ,] = (p1[port2_index, ,] + p1[port3_index, ,]) / 2
  	  p1_rep[port3_index, ,] = 0
  	  sample_days_rep[port2_index,] = max(sample_days[port2_index,], sample_days[port3_index,])
  	  sample_days_rep[port3_index,] = 0
  	  
  	  port_index = port2_index
  	  domain_interviews_mpd = filter(domain_interviews_m, .data$PORT_FK == 2 | .data$PORT_FK == 3, .data$TYPE_OF_DAY == ifelse(j == 1, "WD", "WE")) # restrict interviews to the current method, port, and type of day
	  }
	  
	  for(k in 1:dimensions[3]) { # Go through the charter statuses
	    if(sum(bl_rep[port_index, j, , k]) > 0 && sample_days[port_index, j] > 0) { # If data has been collected for the current domain, changed "bl" to "bl_rep"
	      domain_interviews_mpdc = filter(domain_interviews_mpd, .data$CHARTER_F == ifelse(k == 1, T, F)) # !! For csv file input change "T"&"F" to T & F for CHARTER_F
	      num_interview = nrow(domain_interviews_mpdc)
	      
        df_other_ports_g = calc_df(year, pool_f, species, bl_rep, bl2_rep, bl_unknown_method_rep, bl_unknown_fished_rep, bl_known_fished_rep, p1_rep, p2, sample_days_rep, days, domain_interviews_mpdc, interviews_raw, method_index, method_key, ports_refer, reference, port_index, j, k, T)
        expansion = rbind(expansion, df_other_ports_g)

        # pooling interviews for SPC, check with Toby about assinging NA to port!
        if(num_interview < 3 && pool_f) {
          # If insufficient interviews are available and pooling is to be used

          # Call the pooling function and get the pooled interviews back
          interviews_pooled = pool_interviews(interviews_raw, year, method_key, j, NA, k, TRUE)[[1]] # port = NA and unsampled_ports = TRUE, j&k are indices
          if(is.data.frame(interviews_pooled) && nrow(interviews_pooled) >= 1) { # keep pooling results for SPC, changed >=3 to >=1
            # If pooling was able to generate at least 3 interviews
            # process_type = "G"
            domain_interviews_mpdc = interviews_pooled
            print("OtherPortSPCpoolingInterview#"); print(nrow(domain_interviews_mpdc))
            # num_interview_pooled = nrow(interviews)
            num_interview = nrow(domain_interviews_mpdc) # use pooled interviews for all calculations
          }
        }
        
	      catch_df = data.frame() # species catch information
	      if(num_interview > 0) {
	        for(l in 1:num_interview) {
	          catch_interview = filter(catch_raw, .data$INTERVIEW_FK == domain_interviews_mpdc[l,]$INTERVIEW_PK) # all species catch data for the current interview
	          
	          catch_df = rbind(catch_df, catch_interview)
	        }
	      }
	      
	      if(nrow(catch_df) > 0) {
	        # if there was any catch for the current domain interviews
	        
	        # 6/23/20 added by TM
	        species_df = data.frame()
	        species_list = unique(catch_df$SPECIES_FK)
	        if(is.na(species)) {
	          num_interview = nrow(domain_interviews_mpdc)
	          sum_hour = sum(domain_interviews_mpdc$HOURS_FISHED)
	          sum_gear = sum(domain_interviews_mpdc$NUM_GEAR)
	          sum_fisher_hour = sum(domain_interviews_mpdc$NUM_FISHER * domain_interviews_mpdc$HOURS_FISHED)
	          
	          for(l in 1:length(species_list)) {
	            specie = species_list[l]
	            
	            all_catch_specie = rep.int(0, nrow(domain_interviews_mpdc))
	            for(m in 1:nrow(domain_interviews_mpdc)) {
	              catch_entries = filter(catch_df, .data$SPECIES_FK == specie & .data$INTERVIEW_FK == domain_interviews_mpdc[m, "INTERVIEW_PK"])
	              if(nrow(catch_entries) > 0) {
	                all_catch_specie[m] = sum(catch_entries$EST_KGS)
	              }
	            }
	            
	            sum_tot_kgs = sum(all_catch_specie)
	            cpue_trip = sum_tot_kgs / num_interview
	            cpue_hour = sum_tot_kgs / sum_hour
	            cpue_gear = sum_tot_kgs / sum_gear
	            cpue_fisher_hour = sum_tot_kgs / sum_fisher_hour
	            var_cpue_trip = calc_var_average(all_catch_specie)
	            std_cpue_trip = 100 * var_cpue_trip ^ 0.5 / cpue_trip # std = 100*CV
	            var_cpue_hour = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$HOURS_FISHED)
	            std_cpue_hour = 100 * var_cpue_hour ^ 0.5 / cpue_hour
	            var_cpue_gear = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$NUM_GEAR)
	            std_cpue_gear = 100 * var_cpue_gear ^ 0.5 / cpue_gear
	            var_cpue_fisher_hour = calc_var_quotient(all_catch_specie, domain_interviews_mpdc$NUM_FISHER * domain_interviews_mpdc$HOURS_FISHED)
	            std_cpue_fisher_hour = 100 * var_cpue_fisher_hour ^ 0.5 / cpue_fisher_hour
	            var_kgs_caught = calc_var_product(df_other_ports_g$EXP_TRIP, df_other_ports_g$VAR_EXP_TRIP, cpue_trip, var_cpue_trip)
	            species_df = rbind(species_df, data.frame(SPECIES_FK = specie,
	                                                      CPUE_TRIP = cpue_trip,
	                                                      CPUE_HOUR = cpue_hour,
	                                                      CPUE_GEAR = cpue_gear,
	                                                      CPUE_FISHER_HOUR = cpue_fisher_hour,
	                                                      VAR_CPUE_TRIP = var_cpue_trip,
	                                                      VAR_KGS_CAUGHT = var_kgs_caught,
	                                                      STD_CPUE_TRIP = std_cpue_trip,
	                                                      VAR_CPUE_HOUR = var_cpue_hour,
	                                                      STD_CPUE_HOUR = std_cpue_hour,
	                                                      VAR_CPUE_GEAR = var_cpue_gear,
	                                                      STD_CPUE_HOUR = std_cpue_hour,
	                                                      VAR_CPUE_FISHER_HOUR = var_cpue_fisher_hour,
	                                                      STD_CPUE_FISHER_HOUR = std_cpue_fisher_hour))
	          }
	        }
	        
	        # group catch by species and calculate summary fields
  	      catch_by_species = summarise(group_by(catch_df, .data$SPECIES_FK), num_interview = n(), kgs_caught = sum(.data$EST_KGS), num_kept = sum(.data$NUM_KEPT), price_lb = mean(.data$PRICE_LB))
  	      
  	      species_composition_domain_other = data.frame(SPC_PK = paste0(substring(df_other_ports_g$EXP_PK, 1, 14),
  	                                                                     sprintf("%05d", catch_by_species$SPECIES_FK),
  	                                                                     substring(df_other_ports_g$EXP_PK, 15, 18)),
  	                                                     EXP_FK = df_other_ports_g$EXP_PK,
  	                                                     FLAG_POOLED = df_other_ports_g$FLAG_POOLED,
  	                                                     SPECIES_FK = catch_by_species$SPECIES_FK,
  	                                                     NUM_INTERVIEW = catch_by_species$num_interview,
  	                                                     NUM_INTERVIEW_POOLED = num_interview,
  	                                                     KGS_CAUGHT = catch_by_species$kgs_caught / sum(catch_by_species$kgs_caught) * df_other_ports_g$EXP_TOT_KGS,
  #	                                                     VAR_LBS_CAUGHT = NA,
  #	                                                     STD_LBS_CAUGHT = NA,
  #	                                                     LBS_SOLD = NA,
  	                                                     NUM_KEPT = catch_by_species$num_kept,
  	                                                     PRICE_LB = catch_by_species$price_lb,
  	                                                     SYS_CREATE_TIME = Sys.time(),
  	                                                     SYS_MODIFY_TIME = Sys.time(),
  	                                                     HASH = NA)
	      
  	      # 6/23/20 added by TM
  	      if(is.na(species)) {
  	        species_composition_domain_other = inner_join(species_composition_domain_other, species_df, by = c("SPECIES_FK" = "SPECIES_FK"))
  	      }
  	      
  	      species_composition = rbind(species_composition, species_composition_domain_other)
	      }
	    }
	  }
	}
	
	return(list(expansion, species_composition))
}