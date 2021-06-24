# Install project dependencies (managed in DESCRIPTION
if (!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_deps(dependencies=NA, upgrade="never")
pkgs = setdiff(remotes::local_package_deps(".", dependencies=NA), "targets")

library(targets)
tar_option_set(packages=pkgs)


# End this file with a list of target objects.
list(
  tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),
  tar_target(summary, summ(data)) # Call your custom functions as needed.
)
