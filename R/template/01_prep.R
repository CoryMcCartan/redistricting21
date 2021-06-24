# Prepare data for ```SLUG``` analysis
# ``COPYRIGHT``

``SLUG``_shp_path = "data/``STATE``/"

# Download necessary files for analysis and
# compile raw data into a final shapefile for analysis
``SLUG``_prepare = function(paths) {
    if (file.exists(``SLUG``_shp_path)) return(``SLUG``_shp_path)

    shp_url = ""
    shp_path = "data-raw/``STATE``/" # don't use here()
    download(shp_url, here(shp_path))

    baf_url = ""
    baf_path = "data-raw/``STATE``/"
    download(baf_url, here(baf_path))

    ``state``_shp = read_sf(here(shp_path)) %>%
        ms_simplify(keep=0.04, keep_shapes=TRUE)

    # preparation and processing code

    ``state``_final_shp = ...

    write_rds(``state``_final_shp, here(``SLUG``_shp_path), compress="xz")

    # return path to processed file
    ``SLUG``_shp_path
}
