
# wisclabmisc 0.1.1.9000 (dev version)

* Suppress warnings from `sessioninfo::session_info()` in `mem_gamlss()`
* Update regex for `tocs_item()` 
* Add `compute_overlap_rate()`
* `compute_empirical_roc()` works with multilevel data (like a rating scale) if
  the two levels to be compared are provided in `levels`.
* `compute_empirical_roc()` updated to support aliases for `direction`: 
  `"case-low"`/`"control-high"` and `"case-high"`/`"control-low"`. These forms
  help if you think about the direction or comparison as "which group gets a 
  low/high score?" 

# wisclabmisc 0.1.1

* Added `file_rename_with()` and `file_replace_name()`.
* Added `info_surprisal()` and friends for computing entropy.
* Deprecated `check_sample_centiles()` deprecated. Use newly added 
  `check_model_centiles()` instead.
* Added `check_computed_centiles()` for calibrating precomputed centiles 
  (allows examining centiles outside of GAMLSS).

# wisclabmisc 0.1.0

* Package features up to November 2024
