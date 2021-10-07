# Simulate plans for `WA_cd_prelim`
# © October 2021

shp_path = "data/WA/WA_cd_vtd_20.rds"

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    wa_shp = read_rds(here(shp_path))
    redist_map(wa_shp, pop_tol=0.01, ndists=10, adj=wa_shp$adj)
}

# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(map) {
    plans1 = redist_smc(map, nsims=3e3, counties=county)

    ndists = attr(map, "ndists")
    dvote = map$ndv
    rvote = map$nrv
    statewide = sum(dvote) / (sum(dvote) + sum(rvote))
    ker = function(x) pt((x-0.5)/0.035136, df=22)

    plans = plans1 %>%
        add_reference(map$dem_house, "dem_house") %>%
        add_reference(map$rep_house, "rep_house") %>%
        add_reference(map$dem_senate, "dem_senate") %>%
        add_reference(map$rep_senate, "rep_senate") %>%
        mutate(dev =  plan_parity(map),
               comp = distr_compactness(map),
               county_splits = county_splits(map, county),
               dem = group_frac(map, ndv, ndv+nrv),
               black = group_frac(map, pop_black),
               hisp = group_frac(map, pop_hisp),
               minority = group_frac(map, pop - pop_white))

    m_dem = ker(district_group(plans, dem))

    plans = plans %>%
        mutate(represent = rep(as.numeric(dvote %*% m_dem + rvote %*% (1-m_dem)) /
                   sum(dvote + rvote), each=ndists)) %>%
        group_by(draw) %>%
        mutate(proportion = statewide - sum(ker(dem)) / ndists) %>%
        ungroup()

    path = "data/WA/WA_cd_prelim_results.rds"
    write_rds(plans, here(path), compress="xz")

    plans
}

