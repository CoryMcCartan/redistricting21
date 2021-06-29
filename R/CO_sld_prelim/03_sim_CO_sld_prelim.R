# Simulate plans for `CO_sld_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(co_map) {
    plans1 = redist_smc(set_pop_tol(co_map, 0.01),
                        nsims=10e3, counties=county)

    plans = list(
        tol_01 = plans1
    )

    plans = purrr::map(plans, function(p) {
        p %>%
            #pullback() %>%
            mutate(dev =  plan_parity(map),
                   comp = distr_compactness(map),
                   county_splits = county_splits(map, county),
                   dem_16 = group_frac(map, dem_16, dem_16 + gop_16),
                   dem_20 = group_frac(map, dem_20, dem_20 + gop_20),
                   black = group_frac(map, black),
                   hisp = group_frac(map, hisp),
                   minority = group_frac(map, pop - white))
    })

    pl = bind_rows(plans, .id="sim")
    path = "data/CO/CO_sld_prelim_results.rds"
    write_rds(pl, here(path), compress="xz")

    # return path to simulation summary files
    path
}

