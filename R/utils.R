# Initialize a new analysis
init = function(state, type="cd", stage="final", overwrite=F) {
    slug = str_glue("{state}_{type}_{stage}")
    copyright = format(Sys.Date(), "\u00A9 %B %Y")

    path_r <- str_glue("R/{slug}/")
    if (dir.exists(path_r) & !overwrite)
        stop("Analysis `", slug, "` already exists.\n",
             "  Pass `overwrite=TRUE` to overwrite.")
    dir.create(path_r, showWarnings=F)
    cli_alert_success("Creating '{path_r}'")
    dir.create(path_data <- str_glue("data/{state}/"), showWarnings=F)
    cli_alert_success("Creating '{path_data}'")
    dir.create(path_raw <- str_glue("data-raw/{state}/"), showWarnings=F)
    cli_alert_success("Creating '{path_raw}'")

    write_file(as.character(Sys.Date()),
               path_ip <- str_glue("R/{slug}/in_progress"))
    cli_alert_success("Creating '{path_ip}'")

    templates = Sys.glob(here("R/template/*.R"))

    proc_template = function(path) {
        new_basename = str_replace(basename(path), ".R", str_c("_", slug, ".R"))
        new_path = here(path_r, new_basename)
        read_file(path) %>%
            str_replace_all("``SLUG``", slug) %>%
            str_replace_all("``STATE``", state) %>%
            str_replace_all("``state``", str_to_lower(state)) %>%
            str_replace_all("``COPYRIGHT``", copyright) %>%
            write_file(new_path)
        cli_li("Creating '{path_r}{new_basename}'")
        new_path
    }

    cli_alert_info("Copying scripts from templates...")
    cli_ul()
    new_paths = purrr::map(templates, proc_template)
    cli_end()

    cli_alert_success("Initialization complete.")

    if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
        purrr::map(new_paths, rstudioapi::navigateToFile)
        rstudioapi::navigateToFile(new_paths[[1]])
    }
    invisible(NULL)
}


# Download `url` to path. Backend-agnostic (currently `httr`)
download = function(url, path) {
    dir = dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive=TRUE)
    if (!file.exists(path))
        httr::GET(url = url, httr::write_disk(path))
}

# Remove large objects from a `redist_plans` object.
clean_plans = function(pl) {
    as_tibble(pl) %>%
        `attr<-`("plans", NULL) %>%
        `attr<-`("prec_pop", NULL) %>%
        `attr<-`("merge_idx", NULL) %>%
        `attr<-`("wgt", NULL)
}

# Return a data frame of analyses conducted
get_analyses = function() {
    slugs = setdiff(list.dirs("R", recursive=F, full.names=FALSE), "template")
    d = tibble(slug=slugs) %>%
        separate(slug, c("state", "type", "stage"), sep="_", remove=F, extra="drop")
    d$in_progress = file.exists(here("R", slugs, "in_progress"))
    d
}

# Make a map of analyses
summarize_analysis_status = function() {
    usa = tigris::states(cb=TRUE, resolution="20m") %>%
        tigris::shift_geometry() %>%
        select(state=STUSPS, name=NAME, fips=GEOID, geometry)
    analyses = get_analyses()

    PAL = c(`Analyzed`="#465177", `In progress`="#E4C22B")
    p = left_join(usa, analyses, by="state") %>%
        mutate(status = if_else(in_progress, "In progress", "Analyzed")) %>%
    ggplot(aes(fill=status)) +
        geom_sf(color="black", size=0.25) +
        scale_fill_manual(values=PAL, na.translate=FALSE) +
        labs(fill=NULL) +
        theme_void() +
        theme(legend.position="bottom")
    ggsave("images/summary.svg", plot=p, width=7, height=5)
}


# Imports so that dev environment matches targets environment
#' @import here
#' @import cli
#' @import dplyr
#' @import readr
#' @import tidyr
#' @importFrom purrr map_dbl map_dfr
#' @import stringr
#' @import forcats
#' @import ggplot2
#' @import redist
#' @import geomander
#' @import sf
#' @import rmapshaper
#' @import wacolors
#' @import patchwork
NULL
