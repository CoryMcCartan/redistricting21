# Install project dependencies (managed in DESCRIPTION
if (!"remotes" %in% installed.packages()) install.packages("remotes")
pkgs = setdiff(remotes::local_package_deps(".", dependencies=NA), "targets")
if (!all(pkgs %in% installed.packages()))
    remotes::install_deps(dependencies=NA, upgrade="never")
options(tidyverse.quiet=T, dplyr.summarise.inform=F)

library(targets)
suppressMessages(library(here))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
library(rlang)
tar_option_set(packages=pkgs)

source(here("R/utils.R"))

analyses = get_analyses() %>%
    filter(in_progress)

# load the analysis scripts
analyses$slug %>%
    lapply(function(slug) Sys.glob(here("R", slug, "*.R"))) %>%
    unlist() %>%
    lapply(source) %>%
    invisible()

# recipe for one analysis. uses metaprogramming
analysis_targets = function(slug) {
    make_sym = function(x) sym(paste0(slug, "_", x))
    fn_download = make_sym("download")
    fn_prepare = make_sym("prepare")
    fn_setup_map = make_sym("setup_map")
    fn_simulate = make_sym("simulate")
    fn_analyze = make_sym("analyze")
    obj_raw = make_sym("raw_files")
    obj_proc = make_sym("proc_files")
    obj_maps = make_sym("maps")
    obj_sims = make_sym("sims")
    obj_sum = make_sym("sum_files")

    list(
        tar_target_raw(as.character(obj_raw),
                       expr((!!fn_download)())),
        tar_target_raw(as.character(obj_proc),
                       expr((!!fn_prepare)(!!obj_raw))),
        tar_target_raw(as.character(obj_maps),
                       expr((!!fn_setup_map)(!!obj_proc))),
        tar_target_raw(as.character(obj_sims),
                       expr((!!fn_simulate)(!!obj_maps))),
        tar_target_raw(as.character(obj_sum),
                       expr((!!fn_analyze)(!!obj_sims, !!obj_maps)))
    )
}

# create a list of all analyses' steps
unlist(lapply(analyses$slug, analysis_targets), recursive=FALSE)

