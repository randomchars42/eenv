# eenv 1.0.0

  * new dependency: `bioset >= 0.2.0`, which provides sets_read
  * renamed `sets_read` into `plates_read`
      *  it's now oly a wrapper around `bioset::sets_read`, providing no new functionality.
  * added new args to plates_read
      * `dec` to denote character for decimal point
      * `write_data` to indicate whether to write the calculated data
      * `use_written_data` if run next time, use writtten data? (= Caching)
  * set theme as default in .onAttach, no need to add global_theme to plot
  * rename exported objects from `global_\*` to `eenv_\*`
  * rename `create_theme` to `create_eenv_theme` 

# eenv 0.1.1

  * set global options in .onAttach()
      * scipen: 18
      * digits: 2

# eenv 0.1.0

  * initial release
