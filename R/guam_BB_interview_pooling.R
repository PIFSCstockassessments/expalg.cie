#' Perform interview pooling for a domain
#' 
#' Function to conduct interview pooling. It is called by code in the file \code{"R/guam_ BB_ expansion.R"}.
#'      
#' @param interviews All interviews for all years
#' @param year Year of the expansion
#' @param method Fishing method of the current stratum expansion
#' @param type_of_day Type of day of the current stratum expansion
#' @param port Port of the current stratum expansion
#' @param charter Charter status of the current stratum expansion
#' @param unsampled_ports Boolean representing whether the current pooling is for unsampled ports
#' 
#' @return data frame containing the pooled interviews for the current stratum; if pooling could not produce at least 3 interviews, NA is returned
#'
pool_interviews = function(interviews, year, method, type_of_day, port, charter, unsampled_ports) {
	type_of_day_str = ""
	if(type_of_day == 1) {
		type_of_day_str = "WD"
	}
	else {
		type_of_day_str = "WE"
	}

	charter_str = ""
	if(charter == 1) {
		charter_str = TRUE # from "T" to True for csv file
	}
	else {
		charter_str = FALSE # from "F" to FALSE for csv file
	}

	interviews_current_year = interviews[interviews$YEAR == year,]
	interviews_no_pool = interviews_current_year[(interviews_current_year$METHOD_FK == method) & (interviews_current_year$TYPE_OF_DAY == type_of_day_str) & (interviews_current_year$CHARTER_F == charter_str),]
	if(unsampled_ports) {
		interviews_no_pool = interviews_no_pool[interviews_no_pool$PORT_FK == 2 | interviews_no_pool$PORT_FK == 3,]
	}
	else {
		interviews_no_pool = interviews_no_pool[interviews_no_pool$PORT_FK == port,]
	}

	if(nrow(interviews_no_pool) >= 3) {
		return(list(interviews_no_pool, "None"))
	}

	# D: Pool data from other type of day
	pool_d = interviews_current_year[(interviews_current_year$METHOD_FK == method) & (interviews_current_year$CHARTER_F == charter_str),]
	if(unsampled_ports) {
		pool_d = pool_d[pool_d$PORT_FK == 2 | pool_d$PORT_FK == 3,]
	}
	else {
		pool_d = pool_d[pool_d$PORT_FK == port,]
	}
	
	if(nrow(pool_d) >= 3) {
		return(list(pool_d, "D"))
	}
	
	if(method == 4){
		# S: For method 4, pool methods 5 and 6
		pool_s = interviews_current_year[(interviews_current_year$METHOD_FK >= 4) & (interviews_current_year$METHOD_FK <= 6) & (interviews_current_year$CHARTER_F == charter_str),]
		if(unsampled_ports) {
			pool_s = pool_s[pool_s$PORT_FK == 2 | pool_s$PORT_FK == 3,]
		}
		else {
			pool_s = pool_s[pool_s$PORT_FK == port,]
		}		

		if(nrow(pool_s) >= 3) {
			return(list(pool_s, "S"))
		}
	}
	
	if(unsampled_ports || (port == 1 || port == 2)) {
		# P: For Agana Boat Basin (port 1), pool Agat Marina (port 2), and vice versa
		pool_p = interviews_current_year[interviews_current_year$CHARTER_F == charter_str,]
		if(unsampled_ports) {
			pool_p = pool_p[(pool_p$PORT_FK >= 1) & (pool_p$PORT_FK <= 3),]
		}
		else {
			pool_p = pool_p[(pool_p$PORT_FK >= 1) & (pool_p$PORT_FK <= 2),]
		}

		if(method == 4) {
			pool_p = pool_p[(pool_p$METHOD_FK >= 4) & (pool_p$METHOD_FK <= 6),]
		}
		else {
			pool_p = pool_p[pool_p$METHOD_FK == method,]
		}
		
		if(nrow(pool_p) >= 3) {
			return(list(pool_p, "P"))
		}
	}
	
	if((unsampled_ports || port == 3) && (method == 1 || method == 3)) {
		# Q: For trolling (method 1) and atulai night light (method 3) at Merizo Pier (port 3), pool Agana
		# Boat Basin (port 1) and Agat Marina (port 2)
		pool_q = interviews_current_year[(interviews_current_year$PORT_FK >= 1) & (interviews_current_year$PORT_FK <= 3) & (interviews_current_year$METHOD_FK == method) & (interviews_current_year$CHARTER_F == charter_str),]
		
		if(nrow(pool_q) >= 3) {
			return(list(pool_q, "Q"))
		}
	}

	# apply D, then S and P or Q, if applicable
	temp = interviews[interviews$CHARTER_F == charter_str,]
	
	if(method == 4) {
		temp = temp[(temp$METHOD_FK >= 4) & (temp$METHOD_FK <= 6),]
	}
	else {
		temp = temp[temp$METHOD_FK == method,]
	}
	
	if(unsampled_ports) {
		temp = temp[temp$PORT_FK >= 1 & temp$PORT_FK <= 3,]
	}
	else if(port == 1 || port == 2) {
		temp = temp[(temp$PORT_FK >= 1) & (temp$PORT_FK <= 2),]
	}
	else if(port == 3) {
		if(method != 1 && method != 3) {
			temp = temp[temp$PORT_FK == port,]
		}
	}
	
	years = sort(unique(temp$YEAR), decreasing = T)
	years = years[years <= year]
	
	if(length(years) >= 1) { # years to pool from
	  for(y in 1:length(years)) {
	    i = temp[temp$YEAR %in% years[1:y],]
	    
	    if(nrow(i) >= 3) {
	      return(list(i, paste0("-", year - years[y])))
	    }
	    else if(y == length(years)) { # pooled all previous years and still not enough interviews
	      return(list(i, "All Years"))
	    }
	  }
	}
	else { # no past/present years to pool from
	  temp = temp[temp$YEAR %in% years,]
	  return(list(temp, "No Past/Present Interviews"))
	}
}


