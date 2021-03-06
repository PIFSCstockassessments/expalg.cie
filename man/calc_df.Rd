% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guam_BB_expansion.R
\name{calc_df}
\alias{calc_df}
\title{Calculate expansion fields as a Data Frame}
\usage{
calc_df(
  year,
  pool_f,
  species,
  bl,
  bl2,
  bl_unknown_method,
  bl_unknown_fished,
  bl_known_fished,
  p1,
  p2,
  sample_days,
  days,
  interviews,
  interviews_raw,
  method_index,
  method_key,
  ports_refer,
  reference,
  port_index,
  type_of_day_index,
  charter_index,
  other_ports
)
}
\arguments{
\item{year}{year of the expansion}

\item{pool_f}{whether to pool interviews when fewer than 3 are available for a domain}

\item{species}{a vector of species or all species (when species = NA)}

\item{bl}{counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])}

\item{bl2}{squared counts of boat log entries with known fishing method for each domain ([PORT_FK, TYPE_OF_DAY, METHOD_FK, CHARTER_F])}

\item{bl_unknown_method}{counts of boat log entries with unknown fishing method for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])}

\item{bl_unknown_fished}{counts of boat log entries with unknown fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])}

\item{bl_known_fished}{counts of boat log entries with known fishing status for each domain ([PORT_FK, TYPE_OF_DAY, CHARTER_F])}

\item{p1}{adjustment factors to account for missed vessels outside of shift times for each domain ([PORT_FK, METHOD_FK, CHARTER_F])}

\item{p2}{adjustment factor to account for vessels outside of the sampled ports}

\item{sample_days}{the number of sample days for each domain ([PORT_FK, TYPE_OF_DAY])}

\item{days}{the number of calendar days for each domain ([TYPE_OF_DAY])}

\item{interviews}{interviews for this domain}

\item{interviews_raw}{all interviews for all years, for use when pooling}

\item{method_index}{vector index of the current method}

\item{method_key}{database key of the current method}

\item{ports_refer}{vector to convert port keys to port indices}

\item{reference}{reference values for the current method}

\item{port_index}{vector index of the current port}

\item{type_of_day_index}{vector index of the current type of day}

\item{charter_index}{vector index of the current charter status}

\item{other_ports}{boolean for whether this expansion is for the other (unsampled) ports (survey_zone = 24)}
}
\value{
Data frame with a single row containing all expansion fields for the current domain
}
\description{
Calculate expansion fields as a Data Frame
}
