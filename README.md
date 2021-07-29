# expalg.cie

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/PIFSCstockassessments/expalg.cie/workflows/R-CMD-check/badge.svg)](https://github.com/PIFSCstockassessments/expalg.cie/actions)
  <!-- badges: end -->

Small-scale nearshore fisheries in the United States territories of Guam, the Commonwealth of the Northern Mariana Islands (CNMI), and American Samoa target reef, bottom, and nearshore pelagic species. These fisheries are of fundamental importance to subsistence fishers, critical in local cultural traditions, and provide a source of income and recreation to the local population. The R package **expalg.cie** is used to estimate annual catch and effort from boat-based fishing surveys in Guam.

**expalg.cie** also reproduces the case study presented in the NOAA Tech Memo _"Shore-based and boat-based fishing surveys in Guam, the CNMI, and American Samoa: Survey design, expansion algorithm, and a case study"_.

# Package Installation
```r
library(remotes)
install_github("PIFSCstockassessments/expalg.cie@*release") #Latest Release 
```

# Usage 

The following instructions describe the steps to produce expansion outputs.

Run `run_expalg()` with the following parameters to execute the expansion algorithm: 

|Parameter    | Description                      |
|:----- | :--------------------------------|
|`pool_f`     | defaults to `TRUE`, which indicates that interview pooling is used; change to `False` if pooling should not be used |
|`species`    | defaults to `NA`, which indicates that the expansion is performed for all species; change to a vector of species keys if only specific species should be included   |
|`start_year` | The first year of the expansion to run (earliest available year is 1982)  |
|`end_year`   | The last year of the expansion to run (most recent available year is 2019)|

### Data Tables

`run_expalg()` contains all the methods necessary to run the Guam boat-based expansion algorithm. `run_expalg()` imports reads in eight CSV data files embedded in the installed package `extdata` subdirectory:

- CIE_sample_days_all_years.csv
- CIE_reference_raw.csv
- CIE_iwc_allyears.csv
- CIE_catch_raw.csv
- CIE_interviews_raw_pub.csv
- CIE_days_allyears.csv
- CIE_p1_allyears_pub.csv
- CIE_bl_allyears.csv


## Example 

The package includes results from the Guam boat-based expansion representing total catch (`expalg.cie::g_bb_exp`) and catch by species (`expalg.cie::g_bb_spc`). The vignette _"Guam Boat Based Expansion Database Fields"_ describes the fields for the total catch and catch by species tables.


```r
#Write R data object to csv
write.csv(expalg.cie::g_bb_exp, file="G_BB_EXP.csv")
write.csv(expalg.cie::g_bb_spc, file="G_BB_SPC.csv")
```

The main function wrapper `run_expalg()` will return a list of two data frames, `bb_exp` (representing "expansion": the total catch for each estimation domain) and `bb_spc` (representing "species_composition": the total catch by species for each estimation domain). The data frames can be manipulated and output as desired. 

```r
library(expalg.cie)
guam <-run_expalg() # load w/ defaults

# Set "Exapansion" or "Species Composition" data frames as indvidual R data.tables
guam_expansion <- guam$bb_exp
guam_species <- guam$bb_spc

# Write data.frames to file
write.csv(guam_expansion, file="out_guam_exp.csv")
write.csv(guam_species, file="out_guam_spc.csv")
```



# GitHub Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by servicemark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
