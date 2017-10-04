#
# this script needs to be called like
# source("/path/to/script/setup.R", chdir = TRUE)
#

open_files = FALSE

if (open_files) {
  file.edit("./packages.R")
  file.edit("./vars.R")
  file.edit("./themes.R")
  file.edit("./functions.R")
}

rm(open_files)

source("./packages.R")
source("./vars.R")
source("./themes.R")
source("./functions.R")