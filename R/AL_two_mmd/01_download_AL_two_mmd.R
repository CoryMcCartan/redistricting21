# Download data for `AL_two_mmd` analysis
# © August 2021

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download_files = function() {
    abbr <- 'AL'

    # download, read, and add shapefile
    vtd <- download_redistricting_file(abbr, stringr::str_glue('data/{abbr}')) %>%
        read_csv() %>%
        join_shapefile()

    saveRDS(vtd, stringr::str_glue('data/{abbr}/{abbr}_vtd_20.rds'))

    # return list of paths to downloaded file
    list(
        shp = shp_path
    )

}
