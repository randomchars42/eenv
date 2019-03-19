# eenv 1.6.0

  * add `test_simple_predictions` and `scan_simple_predictions`
  * mark `scan_crosstables`, `scan_rocs` and `calc_crosstables` deprecated
  * simplify and enhance `test_roc_empiric`
  * fix bug not respecting ties in `test_roc_empiric`

# eenv 1.5.0

  * add `eenv_signif_steps` and redefine `eenv_signif_bottom`
  * change default param `steps` in `ggplot_annotate_signif` to respect `eenv_signif_bottom`

# eenv 1.4.0

  * add `test_wilcoxon_for_groups`, `ggplot_annotate_signif` and `extract_p_values_from_result_list`
  * fix `scan_crosstables`

# eenv 1.3.0

  * add `get_variable_by_groups`, `plot_qq_for_groups`, `test_friedman`
  * add `scan_crosstables`, `calc_crosstables`, `scan_rocs`
  * make format_* return "NA" if `NA` is given
  * new dependency: `tidyr` >= 0.8.2

# eenv 1.2.0

  * add `calc_sequential_difference`
  
# eenv 1.1.1

  * make `characteristic_get` more robust

# eenv 1.1.0

  * re-add dependencies: `gmodels` (for CrossTable) and `readr` (for reading files the modern way)
  * add functions:
      * `calc_cv`
      * `plate_check_discrepant_multiples`
      * `generate_warnings` (internal)
      * `calc_roc_empiric`
      * `test_\*`-functions
      * `characteristic_\*`-functions
      * `plot_qq`
  * `plates_read`: warn if the user wants to use a reserved name in `addditional_vars`

# eenv 1.0.1

  * fix "invalid font" issue by specifying "Arial" as default font
  * get ready for and depend upon ggplot2 >= 3.0.0

# eenv 1.0.0

  * new dependency: `bioset >= 0.2.0`, which provides sets_read
  * renamed `sets_read` into `plates_read`
      * it's now only a wrapper around `bioset::sets_read`, providing no new functionality
      * by default it reads "plate_#NUM#.csv" and takes `plates` as parameter for the number of plates to read
  * added new args to plates_read
      * `dec` to denote character for decimal point
      * `write_data` to indicate whether to write the calculated data
      * `use_written_data` if run next time, use writtten data? (= Caching)
  * rename exported objects from `global_\*` to `eenv_\*`
  * rename `create_theme` to `create_eenv_theme` 
  * set theme as default in `.onAttach`, no need to add `eenv_theme` to plot
  * fix small error in `create_eenv_theme`: add margins to strips (e.g. in facet_wrap)

# eenv 0.1.1

  * set global `options` in `.onAttach()`
      * `scipen = 18`
      * `digits = 2`

# eenv 0.1.0

  * initial release
