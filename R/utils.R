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
    else
        list(status_code=200)
}

# Remove large objects from a `redist_plans` object.
clean_plans = function(pl) {
    as_tibble(pl) %>%
        `attr<-`("plans", NULL) %>%
        `attr<-`("prec_pop", NULL) %>%
        `attr<-`("merge_idx", NULL) %>%
        `attr<-`("wgt", NULL)
}

# downloads data for state `abbr` to `folder/{abbr}_2020_*.csv` and returns path to file
download_redistricting_file = function(abbr, folder) {
    abbr = tolower(abbr)
    url_vtd = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                     "main/census-vest-2020/", abbr, "_2020_vtd.csv")
    url_block = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                       "main/census-vest-2020/", abbr, "_2020_block.csv")

    path = paste0(folder, "/", basename(url_vtd))
    resp = download(url_vtd, path)
    if (resp$status_code == "404")  {
        path = paste0(folder, "/", basename(url_block))
        resp = download(url_block, path)
        if (resp$status_code == "404")  {
            stop("No files available for ", abbr)
        }
    }
    path
}

# adds precinct shapefile geometry to downloaded data
join_vtd_shapefile = function(data) {
    geom_d = PL94171::pl_get_vtd(data$state[1]) %>%
        select(GEOID20, area_land=ALAND20, area_water=AWATER20, geometry)
    left_join(data, geom_d, by="GEOID20") %>%
        sf::st_as_sf()
}
# adds block shapefile geometry to downloaded data
join_block_shapefile = function(data) {
    geom_d = tigris::blocks(data$state[1], year=2020) %>%
        select(GEOID20, area_land=ALAND20, area_water=AWATER20, geometry)
    left_join(data, geom_d, by="GEOID20") %>%
        sf::st_as_sf()
}


# Return a data frame of analyses conducted
get_analyses = function() {
    slugs = setdiff(list.dirs(here("R"), recursive=F, full.names=FALSE), "template")
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
#' @import ggrepel
NULL
