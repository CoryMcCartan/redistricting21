# Prepare data for `WA_cd_prelim` analysis
# © October 2021

shp_path = "data/WA/WA_cd_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    wa_final_shp = read_csv(here(paths$data), col_types=cols(GEOID20="c")) %>%
        join_vtd_shapefile()

    wa_final_shp = as.data.frame(wa_final_shp) %>%
        st_as_sf() %>%
        ms_simplify(keep=0.08, keep_shapes=TRUE)

    wa_final_shp$adj = redist.adjacency(wa_final_shp)
    wa_final_shp = st_make_valid(wa_final_shp)

    for (name in names(paths$plans)) {
        d_plan = read_sf(paths$plans[[name]])
        wa_final_shp[[name]] = d_plan$DISTRICTN[geo_match(wa_final_shp, d_plan)]
    }
    wa_final_shp = relocate(wa_final_shp, dem_house:rep_senate, .after=vtd)

    write_rds(wa_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
