# Analyze simulations for ```SLUG```
# ``COPYRIGHT``

# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
analyze = function(plans, map) {
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
    path = "data/``STATE``/``SLUG``_results.rds"
    write_rds(pl, here(path), compress="xz")

    # return path to simulation summary files
    path
}
