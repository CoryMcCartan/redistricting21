# Prepare data for `OR_cd_prelim` analysis
# © September 2021

shp_path = "data/OR/OR_cd_prelim_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    or_blocks = read_csv(here(paths$blocks), col_types=cols(GEOID20="c"))
    or_geom = tigris::tracts("OR", year=2020) %>%
        select(GEOID20=GEOID, area_land=ALAND, area_water=AWATER, geometry) %>%
        rmapshaper::ms_simplify(0.05, keep_shapes=TRUE)

    or_plan_a = read_csv(here(paths$plan_a), col_names=c("GEOID20", "cd_a"), col_types="ci")
    or_plan_b = read_csv(here(paths$plan_b), col_names=c("GEOID20", "cd_b"), col_types="ci")

    or_final_shp = or_blocks %>%
        left_join(or_plan_a, by="GEOID20") %>%
        left_join(or_plan_b, by="GEOID20") %>%
        mutate(GEOID20 = str_sub(GEOID20, 1, 11)) %>%
        group_by(GEOID20) %>%
        summarize(county = county[1],
                  cd_a = as.integer(names(which.max(tapply(pop, cd_a, sum)))),
                  cd_b = as.integer(names(which.max(tapply(pop, cd_b, sum)))),
                  across(pop:arv_16, sum)) %>%
        left_join(or_geom, by="GEOID20") %>%
        st_as_sf()

    or_final_shp$adj = redist.adjacency(or_final_shp)

    write_rds(or_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
