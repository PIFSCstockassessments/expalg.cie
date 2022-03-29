# expalg.cie 1.1.0 (development version)

* Updated 2019 data for `CIE_days_allyears`. 
  * R example package data files `g_bb_exp` and `g_bb_spc` were updated with the `CIE_days_allyears` datatable revision.
* Revised README to distinguish output methods between input datafiles (`extdata`) and its own package data file.
* 
* Minor Documentation Updates and Fixes.

# expalg.cie 1.0.1

* Fixed `pool_f` parameter from `run_expalg()` to handle R logical vector strings.
  * Added check for non-logical `pool_f` strings. 
* Documentation Updates and Fixes
  
# expalg.cie 1.0.0

* Production ready version for Center of Independent Experts (CIE) review.
* Documentation updates and cleanup code.
* Removed unused `magrittr` import, added `markdown` to suggests
* Set R dependency to R version 3.5.0 and up.

# expalg.cie 0.9.3

* Renamed to **expalg.cie** (previously known as **expalg**)
* Dataset updates
* Documentation updates and fixes

# expalg 0.9.2

* Documentation updates 
* Renamed `expalg() -> run_expalg()` to disambigiuty with `expalg`, used for package documentation.
* Removed `run_expansion()`from `NAMESPACE`.
* Added a `NEWS.md` file to track changes to the package.

# expalg 0.9.1

* The `_refer` parameters were removed.from the function wrappers `expalg()` and `run_expansion()`. The update will not affect any calculations made in version 0.9.0.  

# expalg 0.9.0

* First Version
