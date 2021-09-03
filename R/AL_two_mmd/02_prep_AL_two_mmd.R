# Prepare data for `AL_two_mmd` analysis
# © June 2021

shp_path = "data/AL/AL_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    vtd <- readRDS(paths$shp)

    vtd <- vtd %>% mutate(adj = redist.adjacency(.))


    write_rds(vtd, here(shp_path), compress = 'xz')

    # return path to processed file
    shp_path
}
