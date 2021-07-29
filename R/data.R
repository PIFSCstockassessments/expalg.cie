
#' Guam Survey Expansion Dataset
#'
#' Total catch for each estimation domain. See the vignette *"Guam Boat Based Expansion Database Fields"* for more information.
#' 
#' @docType data
#'
#' @format A data.frame with 8023 observations with 77 variables:
#' \describe{
#' \item{EXP_PK}{The private key associated with a particular domain expansion.} 
#' \item{PERIOD_TYPE}{The time span of the expansion, with the only option being **Y** for annual} 
#' \item{YEAR}{The four digit year of the expansion}
#' \item{QUARTER}{The quarter of the expansion, with the only option being **1** for an annual expansion.}
#' \item{TYPE_OF_DAY}{The type of day of the expansion, either **WD** for weekday or **WE** for weekend/holiday.}
#' \item{PROCESS_TYPE}{The source of interview data for the expansion, either **G** for actual interviews or **R** for reference values.}
#' \item{FISHERY_TYPE}{The charter status of the expansion, either **C** for charter or **N** for non-charter.}
#' \item{PORT_FK}{The database key corresponding to the port of the expansion}
#' \item{SURVEY_ZONE_FK}{The database key corresponding to the survey zone of the expansion} 
#' \item{METHOD_FK}{The database key corresponding to the method key of the expansion} 
#' \item{NUM_CALENDAR_DAYS}{The total number of days within the expansion period.} 
#' \item{NUM_SAMPLE_DAYS}{The number of boat log sampling days within the expansion period.} 
#' \item{NUM_INTERVIEW}{The number of boat log entries (excluding `METHOD_FK`) where the fishing method was known.}
#' \item{NUM_INTERVIEW_POOLED}{The number of boat log entries (excluding `METHOD_FK`) where the fishing method was unknown.} 
#' \item{NUM_DAYS_FISHED}{The total number of fishing days represented by interviews collected for the current domain.} 
#' \item{NUM_TRIP}{The number of boat log entries of a specfic fishing method for the current domain.} 
#' \item{NUM_KN_METHOD_TRIP}{The number of boat log entries (excluding `METHOD_FK`) where the fishing method was known.} 
#' \item{NUM_UNKN_METHOD_TRIP}{The number of boat log entries (excluding `METHOD_FK`) where the fishing method was unknown} 
#' \item{NUM_KN_FISHED_TRIP}{The number of boat log entries (excluding `METHOD_FK`) where it was known whether the trip was for fishing or not.} 
#' \item{NUM_UNKN_FISHED_TRIP}{The number of boat log entries (excluding `METHOD_FK`) where it was unknown whether the trip was for fishing or not.} 
#' \item{SUM_EST_TRIP}{The estimated total number of trips on sampling days for the current domain.} 
#' \item{SUM_EST_TRIP2}{The sum of the estimated square number of trips, aggregated by sample date.} 
#' \item{SUM_HOUR}{The total number of fishing hours represented on interviews for the current domain.} 
#' \item{SUM_HOUR2}{The sum of squares of the number of fishing hours represented on interviews for the current domain.} 
#' \item{SUM_FISHER}{The total number of fishers represented on interviews for the current domain.} 
#' \item{SUM_FISHER2}{The sum of squares of the number of fishers represented on interviews for the current domain.} 
#' \item{SUM_FISHER_HOUR}{The total number of fisher hours represented on interviews for the current domain.} 
#' \item{SUM_FISHER_HOUR2}{The sum of squares of the number of fisher hours represented on interviews for the current domain.} 
#' \item{SUM_GEAR}{The total number of fishing gears represented on interviews for the current domain.} 
#' \item{SUM_GEAR2}{The sum of squares of the number of fishing gears represented on interviews for the current domain.} 
#' \item{SUM_TOT_KGS}{The total catch represented on interviews for the current domain. Measured in kg.} 
#' \item{SUM_TOT_KGS2}{The sum of squares of the catch represented on interviews for the current domain. Measured in kg^2.} 
#' \item{AVG_FISHER}{The average number of fishers represented on interviews for the current domain.} 
#' \item{AVG_HOUR}{The average number of fishing hours represented on interviews for the current domain.} 
#' \item{AVG_FISHER_HOUR}{The average number of fisher hours represented on interviews for the current domain.} 
#' \item{AVG_GEAR}{The average number of fishing gears represented on interviews for the current domain.} 
#' \item{AVG_EST_TRIP}{The average estimated number of trips on sampling days for the current domain.} 
#' \item{EXP_TRIP}{The expanded number of trips for the current domain.} 
#' \item{EXP_HOUR}{The expanded number of fishing hours for the current domain.} 
#' \item{EXP_FISHER}{The expanded number of fishers for the current domain.} 
#' \item{EXP_FISHER_HOUR}{The expanded number of fisher-hours for the current domain. Measured in (fisher * hour).} 
#' \item{EXP_GEAR}{The expanded number of fishing gears for the current domain.} 
#' \item{EXP_TOT_KGS}{The expanded catch for the current domain.} 
#' \item{CPUE_TRIP}{The average catch-per-trip from interviews for the current domain.} 
#' \item{CPUE_HOUR}{The average catch-per-hour from interviews for the current domain. Measured in kg/hr.} 
#' \item{CPUE_FISHER_HOUR}{The average catch-per-fisher-hour from interviews for the current domain. Measured in kg/(fisher * hour).} 
#' \item{CPUE_GEAR}{The average catch-per-gear from interviews for the current domain. Measured in kg/gear.} 
#' \item{VAR_AVG_FISHER}{The variance of the number of fishers represented on interviews for the current domain.} 
#' \item{VAR_AVG_HOUR}{The variance of the number of fishing hours represented on interviews for the current domain.} 
#' \item{VAR_AVG_FISHER_HOUR}{The variance of the number of fisher hours represented on interviews for the current domain.} 
#' \item{VAR_AVG_GEAR}{The variance of the number of fishing gears represented on interviews for the current domain.} 
#' \item{VAR_EST_TRIP}{The variance of the estimated number of trips per sampling day for the current domain.} 
#' \item{VAR_EXP_TRIP}{The variance of the expanded number of trips for the current domain.} 
#' \item{VAR_EXP_HOUR}{The variance of the expanded number of fishing hours for the current domain.} 
#' \item{VAR_EXP_FISHER}{The variance of the expanded number of fishers for the current domain.} 
#' \item{VAR_EXP_FISHER_HOUR}{The variance of the expanded number of fisher-hours for the current domain. Measured in (fisher * hour)^2 } 
#' \item{VAR_EXP_GEAR}{The variance of the expanded number of fishing gears for the current domain.} 
#' \item{VAR_EXP_TOT_KGS}{The variance of the expanded catch for the current domain. Measured in kg^2.} 
#' \item{VAR_CPUE_TRIP}{The variance of the catch-per-trip from interviews for the current domain. Measured in (kg/trip)^2.} 
#' \item{VAR_CPUE_HOUR}{The variance of the catch-per-hour from interviews for the current domain. Measured in (kg/hr)^2.} 
#' \item{VAR_CPUE_GEAR}{The variance of the catch-per-gear from interviews for the current domain. Measured in (kg/gear)^2.} 
#' \item{VAR_CPUE_FISHER_HOUR}{The variance of the catch-per-fisher-hour from interviews for the current domain. Measured in (kg/(fisher * hour))^2.} 
#' \item{STD_EST_TRIP}{The coefficient of variation of the average number of trips per sampling days for the current domain.} 
#' \item{STD_EXP_TRIP}{The coefficient of variation of the expanded number of trips for the current domain.} 
#' \item{STD_EXP_HOUR}{The coefficient of variation of the expanded number of fishing hours for the current domain.} 
#' \item{STD_EXP_FISHER}{The coefficient of variation of the expanded number of fishers for the current domain.} 
#' \item{STD_EXP_FISHER_HOUR}{The coefficient of variation of the expanded number of fisher-hours for the current domain. Measured in (fisher * hour).} 
#' \item{STD_EXP_GEAR}{The coefficient of variation of the expanded number of fishing gears for the current domain.} 
#' \item{STD_EXP_TOT_KGS}{The coefficient of variation of the expanded catch for the current domain. Measured in kg.} 
#' \item{STD_CPUE_TRIP}{The coefficient of variation of the catch-per-trip from interviews for the current domain. Measured in kg/trip.} 
#' \item{STD_CPUE_HOUR}{The coefficient of variation of the catch-per-hour from interviews for the current domain. Measured in kg/hr.} 
#' \item{STD_CPUE_GEAR}{The coefficient of variation of the catch-per-gear from interviews for the current domain. Measured in kg/gear.} 
#' \item{STD_CPUE_FISHER_HOUR}{The coefficient of variation of the catch-per-fisher-hour from interviews for the current domain. Measured in kg/(fisher * hour).} 
#' \item{SYS_CREATE_TIME}{System Time Entry was created.} 
#' \item{SYS_MODIFY_TIME}{System Time Entry was modified.} 
#' \item{FLAG_POOLED}{Flag to include pooled Interviews.} 
#' \item{HASH}{Hash signature.}
#' }
#' 
#' @aliases guamBB_expansion guamexpansion
#' @md
"g_bb_exp"


#' Guam Species Composition Dataset
#'
#' Representing the total catch by species for each estimation domain. See the vignette *"Guam Boat Based Expansion Database Fields"* for more information.
#' 
#' @docType data
#'
#' @format A data.frame with 72049 observations with 25 variables:
#' \describe{
#' \item{SPC_PK}{The private key associated with a particular species catch expansion.} 
#' \item{EXP_FK}{The private key associated with a particular domain expansion.} 
#' \item{FLAG_POOLED}{Flag to include pooled Interviews.} 
#' \item{SPECIES_FK}{The foreign key associated with a particular species.} 
#' \item{NUM_INTERVIEW}{The number of interviews of the current domain that included catch of the current species.} 
#' \item{NUM_INTERVIEW_POOLED}{The number of boat log entries (excluding `METHOD_FK`) where the fishing method was unknown.} 
#' \item{KGS_CAUGHT}{The estimated expanded catch for the current species _s_ in the current domain. } 
#' \item{NUM_KEPT}{The number of specimens of the current species _s_ that were recorded in interviews of the current domain.} 
#' \item{PRICE_LB}{Price per weight} 
#' \item{SYS_CREATE_TIME}{System Time Entry was created.} 
#' \item{SYS_MODIFY_TIME}{System Time Entry was modified.} 
#' \item{HASH}{Hash signature.} 
#' \item{CPUE_TRIP}{The average catch-per-trip from interviews for the current domain.} 
#' \item{CPUE_HOUR}{The average catch-per-hour from interviews for the current domain. Measured in kg/hr.} 
#' \item{CPUE_GEAR}{The average catch-per-gear from interviews for the current domain. Measured in kg/gear.} 
#' \item{CPUE_FISHER_HOUR}{The average catch-per-fisher-hour from interviews for the current domain. Measured in kg/(fisher * hour).} 
#' \item{VAR_CPUE_TRIP}{The variance of the catch-per-trip from interviews for the current domain. Measured in (kg/trip)^2. } 
#' \item{VAR_KGS_CAUGHT}{The variance of the estimated expanded catch for the current species _s_ in the current domain.} 
#' \item{STD_CPUE_TRIP}{The coefficient of variation of the catch-per-trip from interviews for the current domain. Measured in kg/trip.} 
#' \item{VAR_CPUE_HOUR}{The variance of the catch-per-hour from interviews for the current domain. Measured in (kg/hr)^2.} 
#' \item{STD_CPUE_HOUR}{The coefficient of variation of the catch-per-hour from interviews for the current domain. Measured in kg/hr.} 
#' \item{VAR_CPUE_GEAR}{The variance of the catch-per-gear from interviews for the current domain. Measured in (kg/gear)^2.} 
#' \item{STD_CPUE_HOUR.1}{The coefficient of variation of the catch-per-hour from interviews for the current domain. Measured in kg/hr.} 
#' \item{VAR_CPUE_FISHER_HOUR}{The variance of the catch-per-fisher-hour from interviews for the current domain. Measured in (kg/(fisher * hour))^2.} 
#' \item{STD_CPUE_FISHER_HOUR}{The coefficient of variation of the catch-per-fisher-hour from interviews for the current domain. Measured in kg/(fisher * hour).}
#' }
#'
#'@aliases guamBB_species guamspecies
#'@md
"g_bb_spc"