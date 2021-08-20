# downloads data for state `abbr` to `folder/{abbr}_2020_*.csv` and returns path to file
download_redistricting_file = function(abbr, folder) {
    abbr = tolower(abbr)
    url_vtd = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                     "main/census-vest-2020/", abbr, "_2020_vtd.csv")
    url_block = paste0("https://raw.githubusercontent.com/alarm-redist/census-2020/",
                       "main/census-vest-2020/", abbr, "_2020_block.csv")

    path = paste0(folder, "/", basename(url_vtd))
    resp = download(url_vtd, path)
    #if (!is.list(resp) || resp != 0) {
    #    path = paste0(folder, "/", basename(url_block))
    #    resp = download(url_block, path)
    #    if (resp != 0)  {
    #        stop("No files available for ", abbr)
    #    }
    #}
    path
}

join_shapefile = function(data) {
    geom_d = PL94171::pl_get_vtd(data$state[1]) %>%
        dplyr::select(GEOID20, area_land=ALAND20, area_water=AWATER20, geometry)
    dplyr::left_join(data, geom_d, by="GEOID20") %>%
        sf::st_as_sf()
}
