# Prepare data for `CO_cd_staff` analysis
# © September 2021

shp_path = "data/CO/CO_cd_staff_vtd_20.rds"

# compile raw data into a final shapefile for analysis
prepare = function(paths) {
    co_final_shp = read_csv(here(paths$data), col_types=cols(GEOID20="c")) %>%
        join_vtd_shapefile() %>%
        ms_simplify(keep=0.04, keep_shapes=TRUE)
    co_final_shp$vtd[co_final_shp$GEOID20 == "08053027001"] = "053001"

    vtd_baf = PL94171::pl_get_baf("CO", "VTD", cache_to=here("data-raw/CO/co_vtd_baf.rds"))$VTD %>%
        transmute(GEOID20=BLOCKID, county_code=COUNTYFP, vtd=DISTRICT)
    distr_baf = read_csv(here(paths$baf), col_names=c("GEOID20", "cd"), col_types="ci")
    co_counties = tigris::fips_codes %>%
        filter(state=="CO") %>%
        select(county_code, county)
    baf = left_join(vtd_baf, distr_baf, by="GEOID20") %>%
        left_join(co_counties, by="county_code") %>%
        group_by(vtd, county) %>%
        summarize(county = county[1],
                  cd = as.integer(names(which.max(table(cd))))) %>%
        ungroup()
    baf$vtd[baf$vtd == "027001" & baf$county == "Hinsdale County"] = "053001"

    prelim_shp = read_rds(here("data/CO/CO_prelim_vtd_20.Rds")) %>%
        as_tibble() %>%
        select(county=COUNTYFP20, vtd=VTDST20, cd_prelim=cd)
    prelim_shp$vtd[prelim_shp$vtd == "027001" & prelim_shp$county == "053"] = "053001"

    co_final_shp = left_join(co_final_shp, baf, by=c("county", "vtd")) %>%
        as_tibble() %>%
        st_as_sf() %>%
        relocate(cd, .after=vtd)
    co_final_shp = co_final_shp %>%
        left_join(select(prelim_shp, vtd, cd_prelim), by="vtd") %>%
        relocate(cd_prelim, .after=cd)
    co_final_shp = st_transform(co_final_shp, 2877)

    co_final_shp$adj = redist.adjacency(co_final_shp)

    write_rds(co_final_shp, here(shp_path), compress="xz")

    # return path to processed file
    shp_path
}
