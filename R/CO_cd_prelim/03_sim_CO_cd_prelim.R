# Simulate plans for `CO_cd_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    co_shp = read_rds(here(shp_path))
    co_map = redist_map(co_shp, pop_tol=0.01, total_pop=e_pop,
                        existing_plan=cd, adj=co_shp$adj)
    co_map
}

# Simulate redistricting plans
simulate = function(co_map) {
    plans = redist_smc(co_map, nsims=3e3, counties=COUNTYFP20)

    pl = plans %>%
        mutate(sim = "tol_001", .before="draw") %>%
        mutate(dev =  plan_parity(map),
               comp = distr_compactness(map),
               county_splits = county_splits(map, COUNTYFP20),
               dem_16 = group_frac(map, dem_16, dem_16 + rep_16),
               dem_18 = group_frac(map, dem_18, dem_18 + rep_18),
               black = group_frac(map, e_vap_black, e_vap),
               hisp = group_frac(map, e_vap_hisp, e_vap),
               minority = group_frac(map, e_vap - e_vap_white, e_vap)) %>%

    path = "data/CO/CO_cd_prelim_results.rds"
    write_rds(pl, here(path), compress="xz")

    # return path to simulation summary files
    path
}

