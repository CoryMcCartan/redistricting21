# Analyze simulations for ```SLUG```
# ``COPYRIGHT``

# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
analyze = function(out) {
    out$plans = map(out$plans, function(plans) {
        plans %>%
            #pullback() %>%
            mutate(dev =  plan_parity(out$map),
                   comp = distr_compactness(out$map),
                   county_splits = county_splits(out$map, county),
                   town_splits = county_splits(out$map, town),
                   dem_16 = group_frac(out$map, dem_16, dem_16 + gop_16),
                   dem_20 = group_frac(out$map, dem_20, dem_20 + gop_20),
                   black = group_frac(out$map, black),
                   hisp = group_frac(out$map, hisp),
                   minority = group_frac(out$map, pop - white)) %>%
            clean_plans()
    })

    plans = bind_rows(out$plans, .id="sim")
    path = "data/``STATE``/sim_``SLUG``.rds"
    write_rds(plans, here(path), compress="xz")

    # return path to simulation summary files
    path
}
