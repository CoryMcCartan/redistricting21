# Run analysis components which aren't up-to-date.
# Run whole pipeline if `run_all=TRUE` (not default)
run_analysis = function(state, type="cd", stage="final", run_all=FALSE) {
    # load packages
    options(tidyverse.quiet=T, dplyr.summarise.inform=F)
    suppressMessages(library(here))
    suppressMessages(library(cli))
    suppressMessages(library(stringr))

    slug = str_glue("{state}_{type}_{stage}")
    path_r <- str_glue("R/{slug}/")
    if (!dir.exists(path_r)) stop("Analysis `", slug, "` does not exist.")

    cli_h1("Running {.field {slug}}")
    devtools::load_all(here("."))

    analysis = get_analyses() %>%
        filter(.data$state==state, .data$type==type, .data$stage==stage)
    if (analysis$in_progress[1])
        cli_alert("Analysis {.field {slug}} in progress.")

    run_env = new.env()
    Sys.glob(here("R", slug, "*.R")) %>%
        lapply(source, local=run_env)

    shp_path = get("shp_path", run_env)
    if (!file.exists(here(shp_path)) || run_all) {
        cli_alert_info("Generating input files.")
        raw_paths = get("download_files", run_env)()
        cli_alert_success("Files downloaded.")
        get("prepare", run_env)(raw_paths)
        cli_alert_success("Input files processed.")
    }

    sim_path = here("data", state, str_glue("{slug}_sims.rds"))
    if (file.exists(sim_path) && !run_all) {
        sims = read_rds(sim_path)
    } else {
        sims = get("simulate", run_env)(shp_path)
        cli_alert_success("Plans simulated.")
        write_rds(sims, sim_path, compress="xz")
    }

    get("analyze", run_env)(sims)
    cli_alert_success("Plans analyzed.")
    invisible()
}

# Install project dependencies (managed in DESCRIPTION)
install_deps = function() {
    if (!"devtools" %in% installed.packages()) install.packages("devtools")

    gh_pkgs = c(blockpop="CoryMcCartan/blockpop",
                dataverse="iqss/dataverse-client-r")
    gh_to_install = gh_pkgs[-which(names(gh_pkgs) %in% installed.packages())]
    if (length(gh_to_install > 0)) {
        cat("Installing special packages...\n")
        remotes::install_github(gh_to_install)
    }

    pkgs = setdiff(remotes::local_package_deps(".", dependencies=NA), "targets")
    if (!all(pkgs %in% installed.packages())) {
        cat("Installing packages...\n")
        remotes::install_deps(dependencies=NA, upgrade="never")
    }
}
