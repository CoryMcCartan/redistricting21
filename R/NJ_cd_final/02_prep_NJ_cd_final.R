# Prepare data for `NJ_cd_final` analysis
# © January 2022

shp_path = "data/NJ/NJ_cd_final_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    nj_final_shp <- get_alarm('NJ')



    nj_final_shp <- nj_final_shp %>%
        ms_simplify(keep = 0.04, keep_shapes=TRUE)

    nj_final_shp$adj <- adjacency(nj_final_shp)

    baf <- read_csv(paths$baf)

    write_rds(nj_final_shp, here(shp_path), compress = "xz")

    # return path to processed file
    shp_path
}
