# Prepare data for ```SLUG``` analysis
# ``COPYRIGHT``

# Download necessary files for analysis
``SLUG``_download = function() {
    shp_url = ""
    shp_path = "data-raw/``STATE``/" # don't use here()
    download(shp_url, shp_path)

    baf_url = ""
    baf_path = "data-raw/``STATE``/"
    download(baf_url, baf_path)

    # return a named vector of downloaded file paths
    c(shp=shp_path, baf=baf_path)
}

# Compile raw data into a final shapefile for analysis
``SLUG``_prepare = function(paths) {
    ``state``_shp = read_sf(here(paths$shp))

    # preparation and processing code

    ``state``_final_shp = ...

    path = "data/``STATE``/"
    write_rds(``state``_final_shp, here(path), compress="xz")

    # return a named vector of processed file paths
    c(shp=path)
}
