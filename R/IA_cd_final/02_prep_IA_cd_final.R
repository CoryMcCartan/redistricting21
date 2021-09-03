# Prepare data for `IA_cd_final` analysis
# © August 2021

shp_path = "data/IA/IA_cd_final_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    ia_final_shp = read_csv(here(paths$data), col_types=cols(GEOID20="c")) %>%
        join_vtd_shapefile() %>%
        ms_simplify(keep=0.04, keep_shapes=TRUE) #%>%
        group_by(state, county) %>%
        summarize(across(where(is.numeric), sum), is_coverage=TRUE) %>%
        ungroup() %>%
        st_transform(3425)

    ia_final_shp$adj = redist.adjacency(ia_final_shp)

    write_rds(ia_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
