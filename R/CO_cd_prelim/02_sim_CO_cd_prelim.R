# Simulate and analyze `CO_cd_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
CO_cd_prelim_setup_map = function(paths) {
    co_shp = read_rds(paths$shp)
    co_map = redist_map(co_shp, existing_plan=cd, pop_tol=0.001)

    # return a list of `redist_map` objects that will be used in simulation
    list(
        co_01 = co_map
    )
}

# Simulate redistricting plans
CO_cd_prelim_simulate = function(maps) {
    plans = redist_smc(maps$co_01, nsims=10e3, counties=county)

    # return a list of `redist_plans` objects
    list(
        co_01 = plans
    )
}


# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
CO_cd_prelim_analyze = function(sims, maps) {
    plans$co_01 = plans$co_01 %>%
        #pullback() %>%
        mutate(dev =  plan_parity(maps$co_01),
               comp = distr_compactness(maps$co_01),
               county_splits = county_splits(maps$co_01, county),
               town_splits = county_splits(maps$co_01, town),
               dem_16 = group_frac(maps$co_01, dem_16, dem_16 + gop_16),
               dem_20 = group_frac(maps$co_01, dem_20, dem_20 + gop_20),
               black = group_frac(maps$co_01, black),
               hisp = group_frac(maps$co_01, hisp),
               minority = group_frac(maps$co_01, pop - white)) %>%
        clean_plans()

    plans = bind_rows(plans, .id="sim")
    path = "data/CO/sim_CO_cd_prelim.rds"
    write_rds(plans, here(path), compress="xz")

    # return a named vector of simulation summary file paths
    c(sims=path)
}
