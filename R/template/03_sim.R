# Simulate plans for ```SLUG```
# ``COPYRIGHT``

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    ``state``_shp = read_rds(here(shp_path))
    ``state``_map = redist_map(``state``_shp, pop_tol=0.01,
                               existing_plan=cd, adj=``state``_shp$adj)

    ``state``_map
}

# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(``state``_map) {
    plans1 = redist_smc(set_pop_tol(``state``_map, 0.01),
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
    path = "data/``STATE``/``SLUG``_results.rds"
    write_rds(pl, here(path), compress="xz")

    # return path to simulation summary files
    path
}

