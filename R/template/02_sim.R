# Simulate and analyze ```SLUG```
# ``COPYRIGHT``

# Set up the redistricting problem, including filtering, cores, and population tolerance
``SLUG``_setup_map = function(path) {
    ``state``_shp = read_rds(path)
    ``state``_map = redist_map(``state``_shp, existing_plan=cd, pop_tol=0.01)

    # return a list of `redist_map` objects that will be used in simulation
    list(
        ``state``_01 = ``state``_map
    )
}

# Simulate redistricting plans
``SLUG``_simulate = function(maps) {
    plans = redist_smc(maps$``state``_01, nsims=10e3, counties=county)

    # return a list of `redist_plans` objects
    list(
        ``state``_01 = plans
    )
}


# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
``SLUG``_analyze = function(sims, maps) {
    plans$``state``_01 = plans$``state``_01 %>%
        #pullback() %>%
        mutate(dev =  plan_parity(maps$``state``_01),
               comp = distr_compactness(maps$``state``_01),
               county_splits = county_splits(maps$``state``_01, county),
               town_splits = county_splits(maps$``state``_01, town),
               dem_16 = group_frac(maps$``state``_01, dem_16, dem_16 + gop_16),
               dem_20 = group_frac(maps$``state``_01, dem_20, dem_20 + gop_20),
               black = group_frac(maps$``state``_01, black),
               hisp = group_frac(maps$``state``_01, hisp),
               minority = group_frac(maps$``state``_01, pop - white)) %>%
        clean_plans()

    plans = bind_rows(plans, .id="sim")
    path = "data/``STATE``/sim_``SLUG``.rds"
    write_rds(plans, here(path), compress="xz")

    # return path to simulation summary files
    path
}
