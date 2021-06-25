# Simulate plans for ```SLUG```
# ``COPYRIGHT``

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
simulate = function(shp_path) {
    ``state``_map = read_rds(here(shp_path))

    plans = redist_smc(set_pop_tol(``state``_map, 0.01),
                       nsims=10e3, counties=county)

    # return a list of `redist_plans` objects
    list(
        tol_01 = plans
    )
}

