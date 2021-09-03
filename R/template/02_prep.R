# Prepare data for ```SLUG``` analysis
# ``COPYRIGHT``

shp_path = "data/``STATE``/``SLUG``_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    ``state``_final_shp = read_csv(here(paths$data), col_types=cols(GEOID20="c")) %>%
        join_vtd_shapefile() %>%
        ms_simplify(keep=0.04, keep_shapes=TRUE)

    ``state``_final_shp$adj = redist.adjacency(``state``_final_shp)

    write_rds(``state``_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
