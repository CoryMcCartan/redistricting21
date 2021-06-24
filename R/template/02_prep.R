# Prepare data for ```SLUG``` analysis
# ``COPYRIGHT``

shp_path = "data/``STATE``/``SLUG``_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    ``state``_shp = read_sf(here(paths$shp)) %>%
        ms_simplify(keep=0.04, keep_shapes=TRUE)

    # preparation and processing code

    ``state``_final_shp = ...

    write_rds(``state``_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
